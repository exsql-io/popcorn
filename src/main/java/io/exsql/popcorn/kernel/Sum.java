package io.exsql.popcorn.kernel;

import jdk.incubator.vector.LongVector;
import jdk.incubator.vector.VectorSpecies;

public final class Sum {

    private Sum() {}

    private static final VectorSpecies<Long> LONG_SPECIES = LongVector.SPECIES_PREFERRED;

    public static long[] ofLong(final long[] left, final long[] right) {
        final long[] output = new long[left.length];

        final int bound = LONG_SPECIES.loopBound(output.length);

        var i = 0;
        for (; i < bound; i += LONG_SPECIES.length()) {
            var leftVec = LongVector.fromArray(LONG_SPECIES, left, i);
            var rightVec = LongVector.fromArray(LONG_SPECIES, right, i);
            leftVec.add(rightVec).intoArray(output, i);
        }
        for (; i < output.length; i++) {
            output[i] = left[i] + right[i];
        }

        return output;
    }

    public static void ofLongUnsafe(final long[] left, final long[] right) {
        final int bound = LONG_SPECIES.loopBound(left.length);

        var i = 0;
        for (; i < bound; i += LONG_SPECIES.length()) {
            var leftVec = LongVector.fromArray(LONG_SPECIES, left, i);
            var rightVec = LongVector.fromArray(LONG_SPECIES, right, i);
            leftVec.add(rightVec).intoArray(left, i);
        }
        for (; i < left.length; i++) {
            left[i] += right[i];
        }
    }

}
