package io.github.vlsergey.kremlin2wikisource;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import org.apache.commons.lang3.Range;

public class Ranges {

	public static final Ranges EMPTY = new Ranges(new BitSet());

	private final List<Range<Integer>> ranges = new ArrayList<>();

	public Ranges(BitSet bs) {
		for (int start = bs.nextSetBit(0); start >= 0;) {
			final int end = bs.nextClearBit(start);
			this.ranges.add(Range.between(start, end - 1));
			start = bs.nextSetBit(end);
		}
	}

	public boolean contains(int pos) {
		return ranges.stream().anyMatch(range -> range.contains(pos));
	}

	public BitSet toBitSet() {
		BitSet bitSet = new BitSet();
		for (Range<Integer> range : this.ranges) {
			bitSet.set(range.getMinimum(), range.getMaximum() + 1, true);
		}
		return bitSet;
	}

	@Override
	public String toString() {
		return this.ranges.toString();
	}

}
