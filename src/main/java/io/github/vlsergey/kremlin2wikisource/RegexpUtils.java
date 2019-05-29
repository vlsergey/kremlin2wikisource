package io.github.vlsergey.kremlin2wikisource;

import java.util.BitSet;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexpUtils {

	private static BitSet getWikilinkedChars(String src) {
		final BitSet result = new BitSet(src.length());

		for (int c = src.indexOf('['); c != -1; c = src.indexOf('-', c + 1)) {
			int firstClosing = src.indexOf(']', c);
			if (firstClosing == -1) {
				result.set(c, src.length(), true);
				break;
			}
			int nextOpen = src.indexOf('[', firstClosing);
			if (nextOpen == -1) {
				result.set(c, firstClosing + 1, true);
				break;
			}
			int closeBeforeOpen = src.substring(0, nextOpen).lastIndexOf(']');
			if (closeBeforeOpen == -1) {
				// shall be at least firstClosing here!
				throw new AssertionError();
			}
			result.set(c, closeBeforeOpen + 1, true);
		}

		return result;
	}

	static String replaceAll(String src, final Pattern pattern, boolean replaceInWikilinks,
			BiFunction<Matcher, String, String> replacement) {
		final Ranges prohibited;
		if (!replaceInWikilinks) {
			prohibited = new Ranges(getWikilinkedChars(src));
		} else {
			prohibited = Ranges.EMPTY;
		}

		final Matcher matcher = pattern.matcher(src);
		matcher.reset();
		boolean result = matcher.find();
		if (result) {
			StringBuffer sb = new StringBuffer();
			do {
				if (!prohibited.contains(matcher.start())) {
					String replace = replacement.apply(matcher, matcher.group(1));
					matcher.appendReplacement(sb, replace);
				} else {
					matcher.appendReplacement(sb, matcher.group());
				}
				result = matcher.find();
			} while (result);
			matcher.appendTail(sb);
			return sb.toString();
		}
		return src;
	}

	static String replaceAll(String src, String regexp, boolean replaceInWikilinks,
			BiFunction<Matcher, String, String> replacement) {
		return replaceAll(src, Pattern.compile(regexp), replaceInWikilinks, replacement);
	}

}
