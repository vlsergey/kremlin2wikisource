package io.github.vlsergey.kremlin2wikisource;

import java.util.Arrays;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;

class Wikify {

	static String wikify(String content) {
		while (content.contains("\n\n\n"))
			content = content.replaceAll("\n\n\n", "\n\n");
		content = wikifyQuotes(content);

		content = content.replaceAll(" ([IVX]+) СТЕПЕНИ", " {{nobr|$1 СТЕПЕНИ}}");
		content = content.replaceAll(" ([IVX]+) ЛЕТ", " {{nobr|$1 ЛЕТ}}");

		content = content.replaceAll(" N (\\d+)([ ,»\n])", " {{nobr|№ $1}}$2");
		content = content.replaceAll(" \\- ", " — ");
		content = content.replaceAll("==\n\n([А-Яа-я])", "==\n$1");
		return content;
	}

	static String wikifyQuotes(String content) {
		// wikify quotes
		String emphasizeTokens = content;
		emphasizeTokens = emphasizeTokens.replaceAll("([\n;])", "☆$1☆");
		emphasizeTokens = emphasizeTokens.replaceAll("\\. ", "☆.☆ ");
		emphasizeTokens = emphasizeTokens.replace(".\n", "☆.☆\n");
		emphasizeTokens = emphasizeTokens.replace(".☆", "☆.☆");
		emphasizeTokens = emphasizeTokens.replaceAll("\\.$", "☆.");
		content = Arrays.asList(StringUtils.splitPreserveAllTokens(emphasizeTokens, "☆")).stream()
				.map(Wikify::wikifyQuotesImpl).collect(Collectors.joining());
		return content;
	}

	static String wikifyQuotesImpl(String src) {
		String result = src;
		result = result.replaceAll("([ \\[])\"([А-Яа-я][^\"]+[А-Яа-я0-9])\"([ ,;\\]]|$)", "$1«$2»$3");

		if (StringUtils.countMatches(result, '"') == 1 && StringUtils.countMatches(result, '»') == 1) {
			result = result.replaceAll("([ ])\"(.*)«(.*)»", "$1«$2„$3“»");
		}

		return result;
	}

}
