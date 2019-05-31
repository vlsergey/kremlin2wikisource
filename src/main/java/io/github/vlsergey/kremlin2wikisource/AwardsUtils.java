package io.github.vlsergey.kremlin2wikisource;

import static io.github.vlsergey.kremlin2wikisource.RegexpUtils.replaceAll;

import java.util.regex.Pattern;

class AwardsUtils {

	static String wikilink(String content) {
		content = replaceAll(content, "\\n=== [«\"]ЗАСЛУЖЕННЫЙ (.*) РОССИЙСКОЙ ФЕДЕРАЦИИ[»\"] ===\n", true,
				(matcher, g1) -> "\n=== [[:w:ru:Заслуженный " + g1.toLowerCase() + " Российской Федерации|«ЗАСЛУЖЕННЫЙ "
						+ g1 + " {{nobr|РОССИЙСКОЙ ФЕДЕРАЦИИ}}»]] ===\n");

		for (String years : new String[] { "XX", "XXV", "XXX", "XL", "L" })
			content = wikilinkImpl(content, "ЗНАКОМ ОТЛИЧИЯ «ЗА БЕЗУПРЕЧНУЮ СЛУЖБУ» " + years + " ЛЕТ",
					"Знак отличия «За безупречную службу» " + years + " лет");

		content = wikilinkImpl(content, "МЕДАЛЬЮ НЕСТЕРОВА", "Медаль Нестерова");

		for (String ledge : new String[] { "I", "II" })
			content = wikilinkImpl(content, "МЕДАЛЬЮ ОРДЕНА «ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ» " + ledge + " СТЕПЕНИ",
					"Медаль ордена «За заслуги перед Отечеством» " + ledge + " степени");
		content = wikilinkImpl(content, "МЕДАЛЬЮ \"ЗА СПАСЕНИЕ ПОГИБАВШИХ\"", "Медаль «За спасение погибавших»");

		content = wikilinkImpl(content, "ОРДЕНОМ АЛЕКСАНДРА НЕВСКОГО", "Орден Александра Невского (Россия)");
		content = wikilinkImpl(content, "ОРДЕНОМ ДРУЖБЫ НАРОДОВ", "Орден Дружбы народов)");
		content = wikilinkImpl(content, "ОРДЕНОМ ДРУЖБЫ", "Орден Дружбы (Россия)");
		content = wikilinkImpl(content, "ОРДЕНОМ МУЖЕСТВА", "Орден Мужества");
		content = wikilinkImpl(content, "ОРДЕНОМ ПОЧЕТА", "Орден Почёта (Россия)");

		content = wikilinkImpl(content, "ОРДЕНОМ «ЗА ВОЕННЫЕ ЗАСЛУГИ»", "Орден «За военные заслуги» (Россия)");
		for (String ledge : new String[] { "I", "II", "III", "IV" })
			content = wikilinkImpl(content, "ОРДЕНОМ «ЗА ЗАСЛУГИ ПЕРЕД ОТЕЧЕСТВОМ» " + ledge + " СТЕПЕНИ",
					"Орден «За заслуги перед Отечеством» " + ledge + " степени");
		content = wikilinkImpl(content, "ОРДЕНОМ «РОДИТЕЛЬСКАЯ СЛАВА»", "Орден «Родительская слава»");
		return content;
	}

	private static String wikilinkImpl(String src, String title, String wikilink) {
		String result = src;
		result = replace(result, title, wikilink);
		result = replace(result, title.replace('«', '"').replace('»', '"'), wikilink);
		return result;
	}

	private static String replace(String src, String title, String wikilink) {
		return RegexpUtils.replaceAll(src,
				Pattern.compile("(^| )(" + Pattern.quote(title) + ")( |$)",
						Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE),
				false, (matcher, g1) -> matcher.group(1) + "[[:w:ru:" + wikilink + "|" + matcher.group(2) + "]]"
						+ matcher.group(3));
	}

}
