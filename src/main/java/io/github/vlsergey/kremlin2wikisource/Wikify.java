package io.github.vlsergey.kremlin2wikisource;

import java.util.Arrays;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;

class Wikify {

	static String wikify(String content) {
		while (content.contains("\n\n\n"))
			content = content.replaceAll("\n\n\n", "\n\n");
		content = content.replace("\n\n----\n\n", "\n\n\n----\n\n\n");

		content = wikifyQuotes(content);

		content = content.replaceAll(" ([IVX]+) СТЕПЕНИ", " {{nobr|$1 СТЕПЕНИ}}");
		content = content.replaceAll(" ([IVX]+) ЛЕТ", " {{nobr|$1 ЛЕТ}}");

		content = content.replaceAll(" N (\\d+(-рп|-ФЗ|-ФКЗ|с)?)([ ,;)»\n])", " {{nobr|№ $1}}$3");
		content = content.replaceAll(" \\- ", "\u00A0— ");
		content = content.replaceAll("==\n\n([А-Яа-я])", "==\n$1");
		return content;
	}

	static String addNonbreakingSpaces(String src) {
		String result = src;
		result = result.replaceAll("([а-я]) бул\\.,", "$1\u00a0бул.,");
		result = result.replaceAll("(^| )г\\. ([А-Я])", "$1г.\u00a0$2");
		result = result.replaceAll(", д\\. ([0-9])", ", д.\u00a0$1");
		result = result.replaceAll(", корп\\. ([0-9])", ", корп.\u00a0$1");
		result = result.replaceAll("([а-я]) обл\\.,", "$1\u00a0обл.,");
		result = result.replaceAll("([а-я]) пер\\.,", "$1\u00a0пер.,");
		result = result.replaceAll("([а-я]) пл\\.,", "$1\u00a0пл.,");
		result = result.replaceAll(", пос\\. ([А-Я])", ", пос.\u00a0$1");
		result = result.replaceAll("([а-я]) пр\\.,", "$1\u00a0пр.,");
		result = result.replaceAll("([а-я]) просп\\.,", "$1\u00a0просп.,");
		result = result.replaceAll("([а-я]) р\\-н,", "$1\u00a0р-н,");
		result = result.replaceAll(", стр\\. ([0-9])", ", стр.\u00a0$1");
		result = result.replaceAll(", ул\\. ([А-Я])", ", ул.\u00a0$1");
		return result;
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
		content = wikifyQuotesImpl(content);

		return content;
	}

	static String wikifyQuotesImpl(String src) {
		String result = src;
		result = result.replaceAll("([ \\[]|^)\"([А-Яа-я][^\"]+[А-Яа-я0-9\\)])\"([ ,;\\]\\)]|$)", "$1«$2»$3");

		// single char
		result = result.replaceAll("( )\"([а-я])\"( )", "$1«$2»$3");

		/*
		 * заменив в пункте 2 слова ", а в г. Москве и Московской области также со
		 * Службой безопасности Президента Российской Федерации" словами ". Открытие
		 * залов официальных лиц, вновь организуемых в составе аэропортов (аэродромов)
		 * г. Москвы и Московской области, производится Управлением делами Президента
		 * Российской Федерации по согласованию с указанными федеральными органами
		 * исполнительной власти и Федеральной службой охраны Российской Федерации".
		 */
		result = result.replaceAll("(слов |слова |словами )\"([^\"]+)\"([ ;\\.])", "$1«$2»$3");

		if (StringUtils.countMatches(result, '"') == 1 && StringUtils.countMatches(result, '»') == 1) {
			result = result.replaceAll("([ ])\"(.*)«(.*)»", "$1«$2„$3“»");
		}

		return result;
	}

}
