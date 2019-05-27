package io.github.vlsergey.kremlin2wikisource;

import java.net.URI;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
@SuppressWarnings("deprecation")
public class Importer {

	@Data
	private static class ToUpload {
		private String wikidataData;
		private String wikisourceContent;
		private String wikisourceTitle;
	}

	private static final SimpleDateFormat DATE_FORMAT_DOTS = new SimpleDateFormat("dd.MM.yyyy");

	private static final SimpleDateFormat DATE_FORMAT_HUMAN = new SimpleDateFormat("dd MMMM yyyy");

	private static final Scanner scan = new Scanner(System.in);

	static String addWikilinks(String src, String title, String wikilink) {
		return Pattern.compile("( )(" + Pattern.quote(title) + ")( )", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE)
				.matcher(src).replaceAll("$1[[:w:ru:" + wikilink + "|$2]]$3");
	}

	private static Boolean prompt(String prmpt) {
		List<String> allowed = Arrays.asList(Boolean.FALSE.toString().toLowerCase(),
				Boolean.TRUE.toString().toLowerCase());
		String word = "";
		do {
			System.out.print(prmpt);
			if (scan.hasNextLine()) {
				word = scan.nextLine().toLowerCase().trim();
			}
		} while (!allowed.contains(word)); // no need for == true
		return Boolean.valueOf(word);
	}

	static String replaceAll(String src, String regexp, BiFunction<Matcher, String, String> replacement) {
		final Matcher matcher = Pattern.compile(regexp).matcher(src);
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

	@Autowired
	private RestTemplate restTemplate;

	@Autowired
	private WikidataApiHelper wikidataApiHelper;

	@Autowired
	private WikidataModelHelper wikidataModelHelper;

	@Autowired
	private WikisourceApiHelper wikisourceApiHelper;

	private void assertNotBlank(String str) {
		if (StringUtils.isBlank(str)) {
			throw new AssertionError();
		}
	}

	private ToUpload importAsРаспоряжение(URI url, String title, String summary, String contentWithHeader)
			throws Exception {
		contentWithHeader = contentWithHeader.replaceAll("\\r", "");

		final Pattern titlePattern = Pattern
				.compile("^Распоряжение Президента Российской Федерации от (.*) г. № (\\d+\\-рп)$");
		final Matcher titleMatcher = titlePattern.matcher(title);
		if (!titleMatcher.matches())
			throw new AssertionError();
		final String docDateStr = titleMatcher.group(1);
		final String docNumber = titleMatcher.group(2);

		final Date docDate = DATE_FORMAT_DOTS.parse(docDateStr);

		String content = StringUtils.substringAfter(contentWithHeader, "ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\n\n\n");
		assertNotBlank(content);
		content = StringUtils.substringAfter(content, "\n\n");
		assertNotBlank(content);

		content = "\n" + content.trim() + "\n";

		final Matcher signMatcher = Pattern.compile("\\n[ ]+Президент Российской Федерации\\s+([^ ].*)\\n")
				.matcher(content);
		if (!signMatcher.find())
			throw new AssertionError();

		content = signMatcher.replaceFirst("\n{{Подпись|Президент Российской Федерации|$1}}\n");

		final int signStart = content.indexOf("{{Подпись|Президент Российской Федерации|");
		String replacement = "\n{{left|$1}}\n";

		content = wrapInLeft(content, signStart,
				"\\n[ ]+(" + Pattern.quote(new SimpleDateFormat("d MMMM yyyy").format(docDate) + " года") + ")\\n",
				replacement);
		content = wrapInLeft(content, signStart, "\\n[ ]+N (" + Pattern.quote(docNumber) + ")\\n", "\n{{left|№ $1}}\n");

		content = processTextContent(content);

		String wikiContent = MessageFormat.format("'{{'Распоряжение Президента РФ\n" //
				+ "| НОМЕР          = {0}\n" //
				+ "| ДАТА           = {1}\n" //
				+ "| НАЗВАНИЕ       = {2}\n" //
				+ "| ДАТАПУБЛИКАЦИИ = \n" //
				+ "| ИСТОЧНИК       = {3}\n" //
				+ "| ДРУГОЕ         = \n" //
				+ "| УТРАТИЛ СИЛУ   = \n" //
				+ "| ПРЕДЫДУЩИЙ     = \n" //
				+ "| СЛЕДУЮЩИЙ      = \n" //
				+ "| КАЧЕСТВО       = \n" //
				+ "'}}'<div class=\"text\">\n\n{4}\n\n</div>", docNumber, docDateStr, summary, url, content.trim());

		final String articleTitle = "Распоряжение Президента РФ от " + docDateStr + " № " + docNumber;
		ToUpload toUpload = new ToUpload();
		toUpload.setWikisourceTitle(articleTitle);
		toUpload.setWikisourceContent(wikiContent);
		toUpload.setWikidataData("{"//
				+ "\"labels\":" + wikidataModelHelper.toLabelalike("ru", articleTitle) + "," //
				+ "\"descriptions\":" + wikidataModelHelper.toLabelalike("ru", summary) + "," //
				+ "\"claims\":[" //
				+ wikidataModelHelper.toWikibaseEntityidClaim(31, 20873831) + "," // type
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

	private ToUpload importAsУказ(URI url, String title, String summary, String contentWithHeader) throws Exception {
		contentWithHeader = contentWithHeader.replaceAll("\\r", "");

		final Pattern titlePattern = Pattern.compile("^Указ Президента Российской Федерации от (.*) г. № (\\d+)$");
		final Matcher titleMatcher = titlePattern.matcher(title);
		if (!titleMatcher.matches())
			throw new AssertionError();
		final String docDateStr = titleMatcher.group(1);
		final String docNumber = titleMatcher.group(2);
		String expired = "";

		final Date docDate = DATE_FORMAT_DOTS.parse(docDateStr);
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
			content = StringUtils.substringAfter(content, "\n\n");
			assertNotBlank(content);
		} else {
			content = StringUtils.substringAfter(contentWithHeader, "ПРЕЗИДЕНТА РОССИЙСКОЙ ФЕДЕРАЦИИ\n\n\n");
			assertNotBlank(content);
			content = StringUtils.substringAfter(content, "\n\n");
			assertNotBlank(content);
		}

		content = "\n" + content.trim() + "\n";

		final Matcher signMatcher = Pattern.compile("\\n[ ]+Президент\\s+Российской\\s+Федерации\\s+([^ ].*)\\n")
				.matcher(content);
		if (!signMatcher.find())
			throw new AssertionError();

		content = signMatcher.replaceFirst("\n{{Подпись|Президент Российской Федерации|$1}}\n");

		final int signStart = content.indexOf("{{Подпись|Президент Российской Федерации|");
		String replacement = "\n{{left|$1}}\n";

		content = wrapInLeft(content, signStart, "\\n[ ]+(Москва\\,[ ]*Кремль)\\n", replacement);
		content = wrapInLeft(content, signStart,
				"\\n[ ]+(" + Pattern.quote(new SimpleDateFormat("d MMMM yyyy").format(docDate) + " года") + ")\\n",
				replacement);
		content = wrapInLeft(content, signStart, "\\n[ ]+N (" + Pattern.quote(docNumber) + ")\\n", "\n{{left|№ $1}}\n");

		content = processTextContent(content);

		String wikiContent = MessageFormat.format("'{{'Указ Президента РФ\n" //
				+ "| НОМЕР          = {0}\n" //
				+ "| ДАТА           = {1}\n" //
				+ "| НАЗВАНИЕ       = {2}\n" //
				+ "| ДАТАПУБЛИКАЦИИ = \n" //
				+ "| ИСТОЧНИК       = {3}\n" //
				+ "| ДРУГОЕ         = \n" //
				+ "| УТРАТИЛ СИЛУ   = {4}\n" //
				+ "| ПРЕДЫДУЩИЙ     = \n" //
				+ "| СЛЕДУЮЩИЙ      = \n" //
				+ "| КАЧЕСТВО       = \n" //
				+ "'}}'<div class=\"text\">\n\n{5}\n\n</div>", docNumber, docDateStr, summary, url, expired,
				content.trim());

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

	private void importFrom(URI originalUrl) throws Exception {

		int page = 1;
		URI nextUrl = originalUrl;

		String html, title = null, summary = null, content = null;
		do {
			html = restTemplate.getForEntity(nextUrl, String.class).getBody();

			if (page == 1) {
				title = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<h1>", "</h1>"));
				summary = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html,
						"<div class=\"read__lead entry-summary p-summary\" itemprop=\"description\">", "</div>"));
				content = StringEscapeUtils.unescapeHtml4(StringUtils.substringBetween(html, "<pre>", "</pre>"));

				assertNotBlank(title);
				assertNotBlank(summary);
				assertNotBlank(content);
			} else {
				content += "\n";
				final String nextPageContent = StringUtils.substringBetween(html, "<pre class=\"masha-ignore\">",
						"</pre>");
				assertNotBlank(nextPageContent);
				content += StringEscapeUtils.unescapeHtml4(nextPageContent);
			}
			nextUrl = URI.create(originalUrl + "/page/" + (++page));
		} while (html.contains("Показать следующую страницу документа"));

		ToUpload toUpload;
		if (title.startsWith("Распоряжение Президента Российской Федерации от")) {
			toUpload = importAsРаспоряжение(originalUrl, title, summary, content);
		} else if (title.startsWith("Указ Президента Российской Федерации")) {
			toUpload = importAsУказ(originalUrl, title, summary, content);
		} else if (title.startsWith("Федеральный конституционный закон")) {
			return;
		} else if (title.startsWith("Федеральный закон")) {
			return;
		} else {
			throw new AssertionError("Unsupported: \"" + title + "\" from " + originalUrl);
		}

		System.out.println();
		System.out.println(toUpload.wikisourceTitle);
		System.out.println(toUpload.wikisourceContent);

		if (!prompt("Agree to upload? ")) {
			throw new RuntimeException("Aborted");
		}

		final String editSummary = "Importing from " + originalUrl;
		wikisourceApiHelper.edit(editSummary, toUpload.getWikisourceTitle(), toUpload.getWikisourceContent());
		wikidataApiHelper.wbeditentity_new(editSummary, toUpload.getWikidataData());
	}

	private String processTables(String src) {
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
			final int firstNonSpace = StringUtils.indexOfAnyBut(line, ' ');

			d: for (int d = line.indexOf('-', 2); d != -1; d = line.indexOf('-', d + 1)) {
				if (line.charAt(d - 1) != ' ' || line.length() == d + 1 || line.charAt(d + 1) != ' ') {
					continue d;
				}

				// check next lines if d is good delimeter candidate
				for (int n = i + 1; n < lines.length; n++) {
					String nextLine = lines[n];
					if ("\n".equals(nextLine)) {
						candidates[d]++;
						continue d;
					} else {
						final int nextLineFirstNonSpace = StringUtils.indexOfAnyBut(nextLine, ' ');

						if (firstNonSpace != nextLineFirstNonSpace
								|| !StringUtils.substring(nextLine, d - 1, d + 1).trim().isEmpty()) {
							continue d;
						}
					}
				}
			}
		}

		int[] candidateColumns = IntStream.range(0, candidates.length).filter(i -> candidates[i] != 0).mapToObj(i -> i)
				.sorted(Comparator.<Integer, Integer>comparing(i -> Integer.valueOf(candidates[i])).reversed())
				.mapToInt(i -> i).toArray();
		log.info("Possible candidates for table delimetes: {}", candidateColumns);

		int delimiterPosition;
		if (candidateColumns.length == 0) {
			return src;
		} else if (candidateColumns.length == 1) {
			if (1.0 * candidates[candidateColumns[0]] / lines.length < 0.03) {
				// just occasion
				return src;
			}
			delimiterPosition = candidateColumns[0];
		} else {
			if (candidates[candidateColumns[0]] - candidates[candidateColumns[1]] < 2) {
				// not stable enough
				return src;
			}
			delimiterPosition = candidateColumns[0];
		}

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
					} else {
						column1 = (column1 + " " + StringUtils.substring(lines[nextLine], 0, delimiterPosition - 1))
								.trim();
						column2 = (column2 + " " + StringUtils.substring(lines[nextLine], delimiterPosition + 2))
								.trim();
					}
				}
			} else {
				withTables.append(line);
			}
		}
		return StringUtils.join(withTables).replace("\n|}\n\n{|\n", "\n|-\n");
	}

	private String processTextContent(String content) {

		content = processTables(content);
		content = Pattern.compile("([^ ])\\n     (.)", Pattern.MULTILINE).matcher(content).replaceAll("$1\n\n$2");
		content = Pattern.compile("([^ \n\\}])\\n([^\n \\{\\|])", Pattern.MULTILINE).matcher(content)
				.replaceAll("$1 $2");

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

		// other centered strings
		content = content.replaceAll("\\n[ ]+([^\\s].*)\\n", "\n{{center|$1}}\n");
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

		// Ссылки на различные документы, хранящиеся в Викитеке
		content = content.replaceAll("статьями (\\d+) и (\\d+) Конституции Российской Федерации",
				"[[Конституция Российской Федерации#Статья $1|статьями $1]] и [[Конституция Российской Федерации#Статья $2|$2]] Конституции Российской Федерации");

		content = replaceAll(content,
				"(Указом|Указ)\\s+Президента\\s+Российской\\s+Федерации\\s+от\\s+([0-9]+\\s+[а-я]+\\s+\\d+)\\s+г.\\s+N\\s+([0-9][0-9а-я]*) \"([^\"]+)\"",
				(matcher, g1) -> {
					try {
						Date date = DATE_FORMAT_HUMAN.parse(matcher.group(2));
						String docNumber = matcher.group(3);
						String title = matcher.group(4);
						return "[[Указ Президента РФ от " + DATE_FORMAT_DOTS.format(date) + " № " + docNumber + "|"
								+ matcher.group(1) + " Президента Российской Федерации от " + matcher.group(2)
								+ " г. {{nobr|№ " + docNumber + "}} «" + title + "»]]";
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		// абзац; (Утратил силу с 1 июля 2003 г. — Указ Президента Российской Федерации
		// от 19.11.2003 г. N 1365)
		content = replaceAll(content,
				"(Указом|Указ)\\s+Президента\\s+Российской\\s+Федерации\\s+от\\s+(\\d+\\.\\d+\\.\\d+)\\s+г.\\s+N\\s+([0-9][0-9а-я]*)\\)",
				(matcher, g1) -> {
					try {
						Date date = DATE_FORMAT_DOTS.parse(matcher.group(2));
						String docNumber = matcher.group(3);
						return "[[Указ Президента РФ от " + DATE_FORMAT_DOTS.format(date) + " № " + docNumber + "|"
								+ matcher.group(1) + " Президента Российской Федерации от " + matcher.group(2)
								+ " г. {{nobr|№ " + docNumber + "}})]]";
					} catch (Exception exc) {
						log.error("Unable to convert " + matcher.group(), exc);
						return matcher.group();
					}
				});

		content = replaceAll(content,
				"статьей (\\d+) Федерального закона от ([0-9]+\\s+[а-я]+\\s+\\d+) г. N (\\d+\\-ФЗ) \"([^\"]+)\"",
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

		// quotes
		content = content.replaceAll("\\n\"(.*)\"([\\.;])\\n", "\n<blockquote>«$1»$2</blockquote>\n");

		// wikify
		content = Wikify.wikify(content);
		return content;
	}

	public void run() throws Exception {
		for (int pageId = 18363; pageId <= 20000; pageId++) {
			URI uri = URI.create("http://www.kremlin.ru/acts/bank/" + pageId);
			try {
				importFrom(uri);
			} catch (Throwable exc) {
				log.error("Unable to import from " + uri);
				throw exc;
			}
		}
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
