package io.github.vlsergey.kremlin2wikisource;

import java.util.BitSet;

class TextRangeUtils {

	static BitSet getWikilinkedChars(String src) {
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

}
