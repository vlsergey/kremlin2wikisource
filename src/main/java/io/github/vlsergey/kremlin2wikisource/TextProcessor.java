package io.github.vlsergey.kremlin2wikisource;

import static org.apache.commons.lang3.StringUtils.*;

import java.net.URI;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

@Slf4j
@Component
public class TextProcessor {

	@Data
	static class ToUpload {
		private String wikidataData;
		private String wikisourceContent;
		private String wikisourceTitle;
	}

	static final SimpleDateFormat DATE_FORMAT_DOTS = new SimpleDateFormat("dd.MM.yyyy");

	static final SimpleDateFormat DATE_FORMAT_HUMAN = new SimpleDateFormat("dd MMMM yyyy");

	private static final Map<String, String> NORMALIZED_DOC_TYPE_NAMES;

	private static final String REGEXP_DATE_DOTS = "\\d+\\.\\d+\\.\\d+";

	private static final String REGEXP_DATE_HUMAN = "\\d+\\s+[а-я]+ \\d+";

	static {
		NORMALIZED_DOC_TYPE_NAMES = new LinkedHashMap<>();
		NORMALIZED_DOC_TYPE_NAMES.put("распоряжение", "Распоряжение");
		NORMALIZED_DOC_TYPE_NAMES.put("распоряжения", "Распоряжение");
		NORMALIZED_DOC_TYPE_NAMES.put("указ", "Указ");
		NORMALIZED_DOC_TYPE_NAMES.put("указа", "Указ");
		NORMALIZED_DOC_TYPE_NAMES.put("указами", "Указ");
		NORMALIZED_DOC_TYPE_NAMES.put("указов", "Указ");
		NORMALIZED_DOC_TYPE_NAMES.put("указом", "Указ");
	}

	static String addWikilinks(String src, String title, String wikilink) {
		return Pattern.compile("( )(" + Pattern.quote(title) + ")( )", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE)
				.matcher(src).replaceAll("$1[[:w:ru:" + wikilink + "|$2]]$3");
	}

	static void assertNotBlank(String str) {
		if (isBlank(str)) {
			throw new AssertionError();
		}
	}

	static void assertNotNull(String str) {
		if (str == null) {
			throw new AssertionError();
		}
	}

	private static Date parseDate(String strDate) throws Exception {
		if (strDate.matches("^" + REGEXP_DATE_DOTS + "$")) {
			return DATE_FORMAT_DOTS.parse(strDate);
		} else if (strDate.matches("^" + REGEXP_DATE_HUMAN + "$")) {
			return DATE_FORMAT_HUMAN.parse(strDate);
		} else {
			throw new UnsupportedOperationException("Unable to parse: " + strDate);
		}
	}

	static String replaceAll(String src, final Pattern pattern, BiFunction<Matcher, String, String> replacement) {
		final Matcher matcher = pattern.matcher(src);
		matcher.reset();
		boolean result = matcher.find();
		if (result) {
			StringBuffer sb = new StringBuffer();
			do {
				String replace = replacement.apply(matcher, matcher.group(1));
				matcher.appendReplacement(sb, replace);
				result = matcher.find();
			} while (result);
			matcher.appendTail(sb);
			return sb.toString();
		}
		return src;
	}

	static String replaceAll(String src, String regexp, BiFunction<Matcher, String, String> replacement) {
		return replaceAll(src, Pattern.compile(regexp), replacement);
	}

	@Autowired
	private WikidataModelHelper wikidataModelHelper;

	ToUpload importAsРаспоряжение(URI url, String title, String summary, String contentWithHeader) throws Exception {
		contentWithHeader = contentWithHeader.replaceAll("\\r", "");

		final Pattern titlePattern = Pattern
				.compile("^Распоряжение Президента Российской Федерации от (.*) г. № (\\d+\\-рп)$");
		final Matcher titleMatcher = titlePattern.matcher(title);
		if (!titleMatcher.matches())
			throw new AssertionError();
		final String docDateStr = titleMatcher.group(1);
		final String docNumber = titleMatcher.group(2);
		String expired = "";

		final Date docDate = DATE_FORMAT_DOTS.parse(docDateStr);
		String content;

		String space = "[\\s\\n]+";
		Matcher expiredMatcher = Pattern.compile("ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ" + space + "Утратил[oо]" + space
				+ "силу\\s*(.*)" + space + "\\-" + space + "(Распоряжение|Указ)" + space + "Президента" + space
				+ "Российской" + space + "Федерации" + space + "от" + space + "([\\d\\.]+)" + space + "г." + space + "N"
				+ space + "(\\d[\\d\\-а-я]+)\\n").matcher(contentWithHeader);
		if (expiredMatcher.find()) {
			String expiredSinceDate = expiredMatcher.group(1).trim();
			if (expiredSinceDate.isEmpty()) {
				expired = "[[" + expiredMatcher.group(2) + " Президента РФ от " + expiredMatcher.group(3) + " № "
						+ expiredMatcher.group(4) + "]]";
			} else {
				expired = expiredSinceDate + ": [[" + expiredMatcher.group(2) + " Президента РФ от "
						+ expiredMatcher.group(3) + " № " + expiredMatcher.group(4) + "]]";
			}
			content = contentWithHeader.substring(expiredMatcher.end()).trim();
			assertNotBlank(content);
		} else {
			content = substringAfter(contentWithHeader, "ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\n\n\n");
			assertNotBlank(content);
		}

		// skip summary
		if (!summary.isEmpty()) {
			content = substringAfter(content, "\n\n");
			assertNotBlank(content);
		}

		content = "\n" + content.trim() + "\n";

		final Matcher signMatcher = Pattern.compile("\\n[ ]+Президент Российской Федерации\\s+([^ ].*)\\n")
				.matcher(content);
		if (!signMatcher.find())
			throw new AssertionError();

		content = processSignature(content);
		content = processTextContent(content);

		String etc = "";
		if (content.startsWith("(В редакции ")) {
			int enclosingChar = content.indexOf(")");
			etc = content.substring(1, enclosingChar);
			content = content.substring(enclosingChar + 1);
		}

		String wikiContent = MessageFormat.format("'{{'Распоряжение Президента РФ\n" //
				+ "| НОМЕР          = {0}\n" //
				+ "| ДАТА           = {1}\n" //
				+ "| НАЗВАНИЕ       = {2}\n" //
				+ "| ДАТАПУБЛИКАЦИИ = \n" //
				+ "| ИСТОЧНИК       = {3}\n" //
				+ "| ДРУГОЕ         = {4}\n" //
				+ "| УТРАТИЛ СИЛУ   = {5}\n" //
				+ "| ПРЕДЫДУЩИЙ     = \n" //
				+ "| СЛЕДУЮЩИЙ      = \n" //
				+ "| КАЧЕСТВО       = \n" //
				+ "'}}'<div class=\"text\">\n\n{6}\n\n</div>", docNumber, docDateStr, summary, url, etc, expired,
				content.trim());

		final String articleTitle = "Распоряжение Президента РФ от " + docDateStr + " № " + docNumber;
		ToUpload toUpload = new ToUpload();
		toUpload.setWikisourceTitle(articleTitle);
		toUpload.setWikisourceContent(wikiContent);
		toUpload.setWikidataData("{"//
				+ "\"labels\":" + wikidataModelHelper.toLabelalike("ru", articleTitle) + "," //
				+ (isBlank(summary) ? "" : "\"descriptions\":" + wikidataModelHelper.toLabelalike("ru", summary) + ",")
				+ "\"claims\":[" //
				+ wikidataModelHelper.toWikibaseEntityidClaim(31, 20873831) + "," // type
				+ wikidataModelHelper.toWikibaseEntityidClaim(407, 7737) + "," // language
				+ wikidataModelHelper.toWikibaseEntityidClaim(50, 218295) + "," // author
				+ wikidataModelHelper.toWikibaseEntityidClaim(1433, 4426104) + "," // published in
				+ (isBlank(summary) ? "" : wikidataModelHelper.toMonolingualClaim(1476, "ru", summary) + ",") // title
				+ wikidataModelHelper.toTimeClaim(571, docDate) + "," // inception
				+ wikidataModelHelper.toStringClaim(1545, docNumber) + "," // series ordinal
				+ wikidataModelHelper.toUrlClaim(953, url) // source
				+ "]," //
				+ "\"sitelinks\":" + wikidataModelHelper.toSitelinks("ruwikisource", articleTitle.replaceAll(" ", "_"))
				+ "}");

		return toUpload;
	}

	ToUpload importAsУказ(URI url, String title, String summary, String contentWithHeader) throws Exception {
		contentWithHeader = contentWithHeader.replaceAll("\\r", "");

		final String docDateStr, docNumber;
		{
			final Pattern titlePattern = Pattern
					.compile("^Указ\\sПрезидента\\sРоссийской\\sФедерации\\sот\\s(.*)\\sг.\\s№\\s(\\d+)$");
			final Matcher titleMatcher = titlePattern.matcher(title);
			if (!titleMatcher.matches())
				throw new AssertionError();
			docDateStr = titleMatcher.group(1);
			docNumber = titleMatcher.group(2);
		}
		String expired = "";

		final Date docDate = TextProcessor.DATE_FORMAT_DOTS.parse(docDateStr);
		String content;

		String space = "[\\s\\n]+";
		Matcher expiredMatcher = Pattern
				.compile("ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\\n" + "\\n" + space + "Утратил силу\\s*(.*)" + space + "\\-"
						+ space + "Указ" + space + "Президента" + space + "Российской" + space + "Федерации" + space
						+ "от" + space + "([\\d\\.]+)\\s+г." + space + "N" + space + "(\\d[\\dа-я]+)\\n")
				.matcher(contentWithHeader);
		if (expiredMatcher.find()) {
			String expiredSinceDate = expiredMatcher.group(1).trim();
			if (expiredSinceDate.isEmpty()) {
				expired = "[[Указ Президента РФ от " + expiredMatcher.group(2) + " № " + expiredMatcher.group(3) + "]]";
			} else {
				expired = expiredSinceDate + ": [[Указ Президента РФ от " + expiredMatcher.group(2) + " № "
						+ expiredMatcher.group(3) + "]]";
			}
			content = contentWithHeader.substring(expiredMatcher.end()).trim();
			assertNotBlank(content);
			content = substringAfter(content, "\n\n");
			assertNotBlank(content);
		} else {
			content = substringAfter(contentWithHeader, "ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\n\n\n");
			assertNotBlank(content);
			content = substringAfter(content, "\n\n");
			assertNotBlank(content);
		}

		content = "\n" + content.trim() + "\n";
		content = processSignature(content);
		content = processTextContent(content);

		String etc = "";
		if (content.startsWith("(В редакции ")) {
			int enclosingChar = content.indexOf(")");
			etc = content.substring(1, enclosingChar);
			content = content.substring(enclosingChar + 1);
		}

		String wikiContent = MessageFormat.format("'{{'Указ Президента РФ\n" //
				+ "| НОМЕР          = {0}\n" //
				+ "| ДАТА           = {1}\n" //
				+ "| НАЗВАНИЕ       = {2}\n" //
				+ "| ДАТАПУБЛИКАЦИИ = \n" //
				+ "| ИСТОЧНИК       = {3}\n" //
				+ "| ДРУГОЕ         = {4}\n" //
				+ "| УТРАТИЛ СИЛУ   = {5}\n" //
				+ "| ПРЕДЫДУЩИЙ     = \n" //
				+ "| СЛЕДУЮЩИЙ      = \n" //
				+ "| КАЧЕСТВО       = \n" //
				+ "'}}'<div class=\"text\">\n\n{6}\n\n</div>", docNumber, docDateStr,
				processTextContent(summary).trim(), url, etc, expired, content.trim());

		final String articleTitle = "Указ Президента РФ от " + docDateStr + " № " + docNumber;
		ToUpload toUpload = new ToUpload();
		toUpload.setWikisourceTitle(articleTitle);
		toUpload.setWikisourceContent(wikiContent);
		toUpload.setWikidataData("{"//
				+ "\"labels\":" + wikidataModelHelper.toLabelalike("ru", articleTitle) + "," //
				+ "\"descriptions\":" + wikidataModelHelper.toLabelalike("ru", summary) + "," //
				+ "\"claims\":[" //
				+ wikidataModelHelper.toWikibaseEntityidClaim(31, 2061228) + "," // type
				+ wikidataModelHelper.toWikibaseEntityidClaim(407, 7737) + "," // language
				+ wikidataModelHelper.toWikibaseEntityidClaim(50, 218295) + "," // author
				+ wikidataModelHelper.toWikibaseEntityidClaim(1433, 4426104) + "," // published in
				+ wikidataModelHelper.toMonolingualClaim(1476, "ru", summary) + "," // title
				+ wikidataModelHelper.toTimeClaim(571, docDate) + "," // inception
				+ wikidataModelHelper.toStringClaim(1545, docNumber) + "," // series ordinal
				+ wikidataModelHelper.toUrlClaim(953, url) // source
				+ "]," //
				+ "\"sitelinks\":" + wikidataModelHelper.toSitelinks("ruwikisource", articleTitle.replaceAll(" ", "_"))
				+ "}");

		return toUpload;
	}

	private Optional<Integer> getDelimeter(int lines, int[] candidates, float minSureProcent) {
		int[] candidateColumns = IntStream.range(0, candidates.length).filter(i -> candidates[i] != 0).mapToObj(i -> i)
				.sorted(Comparator.<Integer, Integer>comparing(i -> Integer.valueOf(candidates[i])).reversed())
				.mapToInt(i -> i).toArray();

		if (candidateColumns.length == 0) {
			return Optional.empty();
		}

		final int mostOftenDelimeterPos = candidateColumns[0];
		if (candidateColumns.length == 1) {
			if (1.0 * candidates[mostOftenDelimeterPos] / lines < minSureProcent) {
				// just occasion
				return Optional.empty();
			}
			return Optional.of(mostOftenDelimeterPos);
		}
		if (candidates[mostOftenDelimeterPos] - candidates[candidateColumns[1]] < 2) {
			// not stable enough
			return Optional.empty();
		}
		return Optional.of(mostOftenDelimeterPos);
	}

	String processSignature(String src) {
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");

		Pattern signPattern = Pattern.compile("^\\s+(Президент Российской Федерации)\\s+([А-Я]\\.\\s*[А-Я][а-я]*)\\n$");

		for (int i = 0; i < lines.length; i++) {
			Matcher signMatcher = signPattern.matcher(lines[i]);
			if (!signMatcher.matches()) {
				continue;
			}
			lines[i] = "{{Подпись|" + signMatcher.group(1) + "|" + signMatcher.group(2) + "}}\n";

			// usually after signature there is a lift-aligned lines
			if (i + 2 < lines.length && lines[i + 1].trim().isEmpty()) {
				for (int n = i + 2; n < lines.length; n++) {
					String nextLine = lines[n].trim();
					if (nextLine.isEmpty())
						break;

					if (nextLine.matches("^\\s*_{5,}\\s*$")) {
						lines[n] = "\n\n\n" + nextLine.trim() + "\n\n\n";
						break;
					}

					nextLine = nextLine.replaceAll("^N (\\d)", "№ $1");
					lines[n] = "{{left|" + nextLine + "}}\n";
				}
			}

			// only single signature per doc
			break;
		}

		return join(lines);
	}

	private String processBorderlessSinglelineCellsTables(String src) {
		// ширина таблицы из двух колонок одинаковая для разных указов... но вот какая?
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");

		MultiValueMap<Integer, Integer> firstLinesOfTables = new LinkedMultiValueMap<>();
		int[] candidates = new int[Arrays.stream(lines).mapToInt(String::length).max().getAsInt()];
		for (int firstLineIndex = 1; firstLineIndex < lines.length; firstLineIndex++) {
			final String line = lines[firstLineIndex];
			if (line.equals("\n"))
				continue;

			// not a first line for sure
			if (!"\n".equals(lines[firstLineIndex - 1]))
				continue;

			d: for (int d = line.indexOf('-', 2); d != -1; d = line.indexOf('-', d + 1)) {
				if (line.charAt(d - 1) != ' ' || line.length() == d + 1 || line.charAt(d + 1) != ' ') {
					continue d;
				}

				// check next lines if d is good delimeter candidate
				for (int n = firstLineIndex + 1; n < lines.length; n++) {
					String nextLine = lines[n];
					if ("\n".equals(nextLine)) {
						candidates[d] += n - firstLineIndex + 1;
						firstLinesOfTables.add(d, firstLineIndex);
						continue d;
					}
					if (!" - ".equals(substring(nextLine, d - 1, d + 2))) {
						continue d;
					}
				}
			}
		}

		Optional<Integer> delimeterPositionOptional = getDelimeter(lines.length, candidates, 0.5F);
		if (!delimeterPositionOptional.isPresent()) {
			return src;
		}
		final int delimiterPosition = delimeterPositionOptional.get();

		final int lineWidth = Arrays.stream(lines).mapToInt(String::length).max().getAsInt();
		final int column1Width = Math.round(100F * (delimiterPosition - 2) / lineWidth);
		final int column2Width = Math.round(300F / lineWidth);
		final int column3Width = 100 - column1Width - column2Width;

		for (int firstLineIndex : firstLinesOfTables.get(delimiterPosition)) {
			StringBuilder table = new StringBuilder();
			table.append("{|\n");

			for (int row = firstLineIndex; row < lines.length; row++) {
				if (!" - ".equals(substring(lines[row], delimiterPosition - 1, delimiterPosition + 2))) {
					// table ended
					break;
				}

				String cell1 = substring(lines[row], 0, delimiterPosition);
				String cell2 = substring(lines[row], delimiterPosition + 1, lines[row].length());

				table.append("| align=left valign=top width=" + column1Width + "% | " + cell1.trim() + "\n");
				table.append("| align=left valign=top width=" + column2Width + "% | —\n");
				table.append("| align=left valign=top width=" + column3Width + "% | " + cell2.trim() + "\n");
				table.append("|-\n");

				lines[row] = "";
			}

			table.append("|}\n");
			lines[firstLineIndex] = table.toString();
		}

		return join(lines);
	}

	private String processBorderlessMultilineCellsTables(String src) {
		// ширина таблицы из двух колонок одинаковая для разных указов... но вот какая?
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");
		int[] candidates = new int[Arrays.stream(lines).mapToInt(String::length).max().getAsInt()];
		for (int i = 1; i < lines.length; i++) {
			final String line = lines[i];
			if (line.equals("\n"))
				continue;

			// not a first line for sure
			if (!"\n".equals(lines[i - 1]))
				continue;
			final int firstInColumn1 = indexOfAnyBut(line, ' ');

			d: for (int d = line.indexOf('-', 2); d != -1; d = line.indexOf('-', d + 1)) {
				if (line.charAt(d - 1) != ' ' || line.length() == d + 1 || line.charAt(d + 1) != ' ') {
					continue d;
				}
				final int firstInColumn2 = indexOfAnyBut(line.substring(d + 1), ' ') + d + 1;

				// check next lines if d is good delimeter candidate
				for (int n = i + 1; n < lines.length; n++) {
					String nextLine = lines[n];
					if ("\n".equals(nextLine)) {
						candidates[d]++;
						continue d;
					}
					final int nextLineFirstNonSpace = indexOfAnyBut(nextLine, ' ');

					if (!(firstInColumn1 == nextLineFirstNonSpace || firstInColumn2 == nextLineFirstNonSpace)
							|| !substring(nextLine, d - 1, d + 1).trim().isEmpty()) {
						continue d;
					}
				}
			}
		}

		Optional<Integer> delimeterPositionOptional = getDelimeter(lines.length, candidates, 0.03F);
		if (!delimeterPositionOptional.isPresent()) {
			return src;
		}
		int delimiterPosition = delimeterPositionOptional.get();

		final int lineWidth = Arrays.stream(lines).mapToInt(String::length).max().getAsInt();
		final int column1Width = Math.round((delimiterPosition - 2.0F) / lineWidth * 100);
		final int column2Width = Math.round(3F / lineWidth * 100);
		final int column3Width = 100 - column1Width - column2Width;

		// эвристика для таблиц
		StringBuilder withTables = new StringBuilder();
		i: for (int i = 0; i < lines.length; i++) {
			String line = lines[i];

			String column1 = "";
			String column2 = "";
			Matcher firstLineMatcher = Pattern.compile("^(.{" + (delimiterPosition - 1) + "}) \\- (.*)\n$")
					.matcher(line);
			if (firstLineMatcher.matches()) {
				// assume table begin
				column1 = firstLineMatcher.group(1).trim();
				column2 = firstLineMatcher.group(2).trim();

				for (int nextLine = i + 1; nextLine < lines.length; nextLine++) {
					if ("\n".equals(lines[nextLine])) {
						// line break
						withTables.append("{|\n");
						withTables.append("| align=left valign=top width=" + column1Width + "% |" + column1 + "\n");
						withTables.append("| align=center valign=top width=" + column2Width + "% | —\n");
						withTables.append("| align=justify valign=top width=" + column3Width + "% |" + column2 + "\n");
						withTables.append("|}\n");
						withTables.append("\n");
						i = nextLine;
						continue i;
					}
					column1 = (column1 + " " + substring(lines[nextLine], 0, delimiterPosition - 1)).trim();
					column2 = (column2 + " " + substring(lines[nextLine], delimiterPosition + 2)).trim();
				}
			} else {
				withTables.append(line);
			}
		}
		return withTables.toString().replace("\n|}\n\n{|\n", "\n|-\n");
	}

	private String processObvilions(String src) {
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");
		line: for (int i = 0; i < lines.length - 1; i++) {
			final String firstLine = lines[i];
			final String secondLine = lines[i + 1];

			final int secondLineIndent = indexOfAnyBut(secondLine, ' ');

			if (firstLine.matches("^[А-Я][А-Я\\-]{3,}.*\\n") && secondLine.startsWith("   ")
					&& !secondLine.trim().isEmpty()) {
				final StringBuilder builder = new StringBuilder(firstLine.trim());
				builder.append(" ");
				builder.append(secondLine.trim());

				for (int n = i + 2; n < lines.length; n++) {
					final String nextLine = lines[n];
					if (!nextLine.trim().isEmpty()) {
						int indent = indexOfAnyBut(nextLine, ' ');
						// shall have the same ident
						if (indent != secondLineIndent) {
							continue line;
						}
						builder.append(" ");
						builder.append(nextLine.trim());
					} else {
						if (n - i > 1) {
							Arrays.fill(lines, i, n + 1, "");
							lines[i] = "{{висячий отступ|5em|" + builder.toString() + "}}\n";
							lines[i + 1] = "\n";
						}
					}
				}
			}
		}
		return join(lines);
	}

	private String processSubdocs(String src) {
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");
		for (int lineIndex = 0; lineIndex < lines.length; lineIndex++) {
			final String line = lines[lineIndex];
			if (line.matches("^\\s+(УТВЕРЖДЕНЫ|ПРИЛОЖЕНИЕ)(\\s+N\\s+[0-9])?\n$")) {
				while (lineIndex < lines.length && !lines[lineIndex].trim().isEmpty()) {
					String nextLine = lines[lineIndex].trim();
					nextLine = nextLine.replaceAll("N (\\d[\\d\\-а-я]*)([ ]|$)", "№ $1$2");
					lines[lineIndex] = "{{left|" + nextLine + "}}\n";
					lineIndex++;
				}

				// suppose next line is header
				while (lineIndex < lines.length && lines[lineIndex].trim().equals(""))
					lineIndex++;
				if (lineIndex == lines.length) {
					break;
				}

				if (lines[lineIndex].matches("^\\s*[А-Я ]+\n$")) {
					// yes, it's header
					int headerStartLine = lineIndex;
					StringBuilder header = new StringBuilder();
					header.append("== ");
					header.append(lines[lineIndex].trim());
					boolean stillAllCaps = true;

					lineIndex++;
					while (lineIndex < lines.length && !lines[lineIndex].trim().isEmpty()) {
						if (!lines[lineIndex].matches("^\\s*[А-Я ]+\n$")) {
							if (stillAllCaps) {
								stillAllCaps = false;
								header.append(" <br>");
							}
						}
						header.append(lines[lineIndex].trim());
						lines[lineIndex] = "";
						lineIndex++;
					}
					header.append(" ==\n");
					lines[headerStartLine] = header.toString();
				}
			}
		}
		return join(lines);
	}

	private String processTables(String src) {
		String result = src;
		result = processBorderlessMultilineCellsTables(result);
		result = processBorderlessSinglelineCellsTables(result);
		result = processTablesWithHeader(result);
		return result;
	}

	private String processTablesWithHeader(String src) {
		final String[] lines = (src.trim() + "\n").replace("\n", "\n☆").split("☆");
		StringBuilder withTables = new StringBuilder();

		tableCandidate: for (int i = 0; i < lines.length; i++) {
			if (!lines[i].matches("^-{10,}\n$")) {
				withTables.append(lines[i]);
				continue;
			}
			final int tableWidth = lines[i].trim().length();

			final int[] delimeterPositions;
			{
				final String secondLine = lines[i + 1];
				delimeterPositions = IntStream.rangeClosed(0, secondLine.length() - 1)
						.filter(p -> secondLine.charAt(p) == '¦').toArray();
				if (delimeterPositions.length == 0) {
					withTables.append(lines[i]);
					continue tableCandidate;
				}
			}

			final int columnsCount = delimeterPositions.length + 1;
			final int[] columnWidthes = new int[columnsCount];
			for (int c = 0; c < columnsCount; c++) {
				int prevPosition = c == 0 ? -1 : delimeterPositions[c - 1];
				int currPosition = c == delimeterPositions.length ? (tableWidth + 1) : delimeterPositions[c];
				columnWidthes[c] = Math.round(100.0F * (currPosition - prevPosition - 1) / tableWidth);
				if (columnWidthes[c] == 0) {
					withTables.append(lines[i]);
					continue tableCandidate;
				}
			}

			withTables.append("{| class=wikitable |\n");

			boolean isHeader = true;
			String[] nextRow = new String[columnsCount];
			Arrays.fill(nextRow, "");

			for (int n = i + 1; n < lines.length; n++) {
				final String nextLine = lines[n];
				if (nextLine.matches("^-{10,}\n$")) {
					if (isHeader) {
						isHeader = false;
						for (int j = 0; j < nextRow.length; j++) {
							withTables.append("! width=" + columnWidthes[j] + "% | " + nextRow[j] + "\n");
						}
						withTables.append("|-\n");
						Arrays.fill(nextRow, "");
					} else {
						i = n;
						withTables.append("|}\n");
						continue tableCandidate;
					}
				} else if (nextLine.trim().equals("")) {
					// row break
					if (lines[n - 1].trim().isEmpty()) {
						// second-in-row -- table end ?
						withTables.append("|}\n");
						i = n;
						continue tableCandidate;
					}
					for (int j = 0; j < nextRow.length; j++) {
						withTables.append("| " + nextRow[j] + "\n");
					}
					withTables.append("|-\n");
					Arrays.fill(nextRow, "");
				} else if (nextLine.trim().equals("____________")) {
					withTables.append("|}\n\n\n----\n\n\n");
					i = n;
					continue tableCandidate;
				} else {
					for (int c = 0; c < columnsCount; c++) {
						int prevDelimeter = c == 0 ? -1 : delimeterPositions[c - 1];
						int nextDelimeter = c == delimeterPositions.length ? (tableWidth + 1) : delimeterPositions[c];
						String cell = substring(nextLine, prevDelimeter + 1, nextDelimeter);
						nextRow[c] = (nextRow[c] + " " + cell).trim();
					}
				}
			}
		}

		return withTables.toString();
	}

	String processTextContent(String content) {
		content = processSubdocs(content);
		content = processObvilions(content);
		content = processTables(content);

		// строки, начавшиеся с открытия скобки
		content = replaceAll(content, Pattern.compile("(\\n|^)\\s*\\(([^\\)]+)\\)\n", Pattern.DOTALL),
				(matcher, g1) -> matcher.group(1) + "(" + matcher.group(2).replace('\n', ' ') + ")\n");

		// объединение строк
		content = Pattern.compile("\\n     (.)", Pattern.MULTILINE).matcher(content).replaceAll("\n\n$1");
		content = Pattern.compile("([^ \n\\}\\>])\\n([^\n \\{\\|\\<\\!])", Pattern.MULTILINE).matcher(content)
				.replaceAll("$1 $2");
		content = content.replace("дом- интернат", "дом-интернат");
		content = content.replace("психо- неврологический", "психо-неврологический");
		content = content.replace("социально- реабилитационный", "социально-реабилитационный");

		final Pattern whitespacePattern = Pattern.compile("([^\n ])  ");
		while (whitespacePattern.matcher(content).find()) {
			content = whitespacePattern.matcher(content).replaceAll("$1 ");
		}

		content = content.replace("п о с т а н о в л я ю", "{{razr|постановляю}}");

		// splitted headers
		content = content.replaceAll("\\n[ ]{1,}([\"А-Я\\- ]+)\\n\\n[ ]{1,}([А-ЯIVX][\"IVXА-Я\\- ]+)\\n",
				"\n      $1 $2\n");

		// headers
		content = content.replace(
				"\"ЗАСЛУЖЕННЫЙ РАБОТНИК ТЕКСТИЛЬНОЙ И ЛЕГКОЙ ПРОМЫШЛЕННОСТИ\n" + "\n"
						+ "                  РОССИЙСКОЙ ФЕДЕРАЦИИ\"\n",
				"\"ЗАСЛУЖЕННЫЙ РАБОТНИК ТЕКСТИЛЬНОЙ И ЛЕГКОЙ ПРОМЫШЛЕННОСТИ РОССИЙСКОЙ ФЕДЕРАЦИИ\"\n");
		content = content.replaceAll("\\n[ ]*([\"А-ЯIVX\\- ]+)\\n", "\n=== $1 ===\n");

		content = content.replaceAll("\n\\s+[_]{5,}\n", "\n\n----\n\n");

		// other centered strings
		content = content.replaceAll("\\n[ ]+([^\\s].*)\\n", "\n{{center|$1}}\n");

		// awards
		content = replaceAll(content, "\\n=== [«\"]ЗАСЛУЖЕННЫЙ (.*) РОССИЙСКОЙ ФЕДЕРАЦИИ[»\"] ===\n",
				(matcher, g1) -> "\n=== [[:w:ru:Заслуженный " + g1.toLowerCase() + " Российской Федерации|«ЗАСЛУЖЕННЫЙ "
						+ g1 + " {{nobr|РОССИЙСКОЙ ФЕДЕРАЦИИ}}»]] ===\n");

		for (String years : new String[] { "XX", "XXV", "XXX", "XL", "L" })
			content = addWikilinks(content, "ЗНАКОМ ОТЛИЧИЯ \"ЗА БЕЗУПРЕЧНУЮ СЛУЖБУ\" " + years + " ЛЕТ",
					"Знак отличия «За безупречную службу» " + years + " лет");

		content = addWikilinks(content, "МЕДАЛЬЮ НЕСТЕРОВА", "Медаль Нестерова");
		content = addWikilinks(content, "МЕДАЛЬЮ ОРДЕНА \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" I СТЕПЕНИ",
				"Медаль ордена «За заслуги перед Отечеством» I степени");
		content = addWikilinks(content, "МЕДАЛЬЮ ОРДЕНА \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" II СТЕПЕНИ",
				"Медаль ордена «За заслуги перед Отечеством» II степени");
		content = addWikilinks(content, "МЕДАЛЬЮ \"ЗА СПАСЕНИЕ ПОГИБАВШИХ\"", "Медаль «За спасение погибавших»");

		content = addWikilinks(content, "ОРДЕНОМ АЛЕКСАНДРА НЕВСКОГО", "Орден Александра Невского (Россия)");
		content = addWikilinks(content, "ОРДЕНОМ ДРУЖБЫ", "Орден Дружбы (Россия)");
		content = addWikilinks(content, "ОРДЕНОМ МУЖЕСТВА", "Орден Мужества");
		content = addWikilinks(content, "ОРДЕНОМ ПОЧЕТА", "Орден Почёта (Россия)");
		for (String ledge : new String[] { "I", "II", "III", "IV" })
			content = addWikilinks(content, "ОРДЕНОМ \"ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ\" " + ledge + " СТЕПЕНИ",
					"Орден «За заслуги перед Отечеством» " + ledge + " степени");

		content = addWikilinks(content, "ОРДЕНОМ \"РОДИТЕЛЬСКАЯ СЛАВА\"", "Орден «Родительская слава»");

		// quotes
		content = content.replaceAll("\\n\"(.*)\"([\\.;])\\n", "\n<blockquote>«$1»$2</blockquote>\n");
		// wikify
		content = Wikify.wikify(content);
		// multiline quote
		content = Pattern.compile("\n\"([^\"]+)\".\n", Pattern.DOTALL).matcher(content)
				.replaceAll("\n<blockquote>«$1».</blockquote>\n");

		// Ссылки на различные документы, хранящиеся в Викитеке
		content = content.replaceAll("статьи (\\d+) Конституции Российской Федерации",
				"[[Конституция Российской Федерации#Статья $1|статьи $1 Конституции Российской Федерации]]");

		content = content.replaceAll("статьями (\\d+) и (\\d+) Конституции Российской Федерации",
				"[[Конституция Российской Федерации#Статья $1|статьями $1]] и [[Конституция Российской Федерации#Статья $2|$2]] Конституции Российской Федерации");

		/*
		 * абзац; (Утратил силу с 1 июля 2003 г. — Указ Президента Российской Федерации
		 * от 19.11.2003 г. N 1365)
		 */
		/*
		 * 2. Внести в распоряжение Президента Российской Федерации от 31 октября 1997
		 * г. N 452-рп (Собрание законодательства Российской Федерации, 1997, {{nobr|№
		 * 44}}, ст. 5059) и в Положение о попечительском совете общероссийского
		 * государственного телевизионного канала «Культура», утвержденное этим
		 * распоряжением, следующие изменения:
		 */
		content = replaceAll(content,
				"([Рр]аспоряжение|[Рр]аспоряжения|Указ|Указа|Указом)\\s+Президента\\s+Российской\\s+Федерации\\s+"
						+ "от\\s+(.*)\\s+г.\\s+\\" + "{\\{nobr\\|№\\s+([0-9][0-9а-я\\-]*)\\}\\}" + "( «([^»\"\n]+)»)?",
				(matcher, g1) -> {
					try {
						final String normDocType = NORMALIZED_DOC_TYPE_NAMES.get(matcher.group(1).toLowerCase());
						final String strDate = matcher.group(2);
						final Date date = parseDate(strDate);
						String docNumber = matcher.group(3);
						if (isBlank(matcher.group(4))) {
							return "[[" + normDocType + " Президента РФ от " + DATE_FORMAT_DOTS.format(date) + " № "
									+ docNumber + "|" + matcher.group(1) + " Президента Российской Федерации {{nobr|от "
									+ strDate + " г.}} {{nobr|№ " + docNumber + "}}]]";
						}
						final String title = matcher.group(5);
						return "[[" + normDocType + " Президента РФ от " + DATE_FORMAT_DOTS.format(date) + " № "
								+ docNumber + "|" + matcher.group(1) + " Президента Российской Федерации {{nobr|от "
								+ strDate + " г.}} {{nobr|№ " + docNumber + "}} «" + title + "»]]";
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		final String regexpItem = "от (" + REGEXP_DATE_DOTS + "|" + REGEXP_DATE_HUMAN
				+ ") г. \\{\\{nobr\\|№ ([0-9][0-9а-я\\-]+)\\}\\}(\\s+«([^»]+)»)?";
		content = replaceAll(content, "(указов|указами)\\s+Президента\\s+Российской\\s+Федерации\\s+" + "(("
				+ regexpItem + "(\\sи\\s|[;\\)\\n ]+|$)" + ")+)", (matcher, g1) -> {
					try {
						final StringBuilder builder = new StringBuilder();
						final String normDocType = NORMALIZED_DOC_TYPE_NAMES.get(matcher.group(1).toLowerCase());

						builder.append(g1);
						builder.append(" Президента Российской Федерации ");

						final String all = matcher.group(2);
						final String wikilinked = replaceAll(all, regexpItem, (itemMatcher, itemG1) -> {
							try {
								final String itemStrDate = itemMatcher.group(1);
								final Date itemDate = parseDate(itemStrDate);
								final String itemNumber = itemMatcher.group(2);
								final String itemTitle = itemMatcher.group(4);

								return "[[" + normDocType + " Президента РФ от " + DATE_FORMAT_DOTS.format(itemDate)
										+ " № " + itemNumber + "|от {{nobr|" + itemStrDate + " г.}} {{nobr|№ "
										+ itemNumber + "}}" + (isBlank(itemTitle) ? "" : " «" + itemTitle + "»") + "]]";
							} catch (Exception exc) {
								log.error(exc.getMessage(), exc);
								return itemMatcher.group();
							}
						});
						builder.append(wikilinked);
						return builder.toString();
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		content = replaceAll(content,
				"статьей (\\d+) Федерального закона от ([0-9]+\\s+[а-я]+\\s+\\d+) г. \\{\\{nobr\\|№ (\\d+\\-ФЗ)\\}\\} «([^\"]+)»",
				(matcher, g1) -> {
					try {
						String articleNumber = matcher.group(1);
						Date date = DATE_FORMAT_HUMAN.parse(matcher.group(2));
						String docNumber = matcher.group(3);
						String title = matcher.group(4);
						return "[[Федеральный закон от " + DATE_FORMAT_DOTS.format(date) + " № " + docNumber
								+ "#Статья " + articleNumber + "|статьей " + articleNumber + " Федерального закона от "
								+ matcher.group(2) + " г. {{nobr|№ " + docNumber + "}} «" + title + "»]]";
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		content = replaceAll(content,
				"Федерального закона от ([0-9]+\\s+[а-я]+\\s+\\d+) г. \\{\\{nobr\\|№ (\\d+\\-ФЗ)\\}\\} «([^\"\n]+)»",
				(matcher, g1) -> {
					try {
						Date date = DATE_FORMAT_HUMAN.parse(matcher.group(1));
						String docNumber = matcher.group(2);
						String title = matcher.group(3);
						return "[[Федеральный закон от " + DATE_FORMAT_DOTS.format(date) + " № " + docNumber
								+ "|Федерального закона от " + matcher.group(2) + " г. {{nobr|№ " + docNumber + "}} «"
								+ title + "»]]";
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		content = content.replaceAll("резолюцией Совета Безопасности ООН (\\d+) от \\d+ [а-я]+ (\\d+) г.",
				"[[Резолюция Совета Безопасности ООН № S/RES/$1 ($2)|$0]]");

		return content;
	}

	private String wrapInLeft(String content, final int signStart, String regexp, String replacement)
			throws AssertionError {
		final Matcher signPlaceMatcher = Pattern.compile(regexp).matcher(content);
		if (!signPlaceMatcher.find(signStart))
			throw new AssertionError();
		content = signPlaceMatcher.replaceFirst(replacement);
		return content;
	}

}
