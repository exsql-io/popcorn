package io.exsql.popcorn.kernel;

import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorMask;
import jdk.incubator.vector.VectorSpecies;

public class And {

    private And() {}

    private static final VectorSpecies<Byte> BYTE_SPECIES = ByteVector.SPECIES_PREFERRED;

    public static boolean[] and(final boolean[] left, final boolean[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length < bound) {
            return and(left, right, 0);
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            and(left, right, i);
        }

        for (; i < left.length; i++) {
            left[i] &= right[i];
        }

        return left;
    }

    private static boolean[] and(final boolean[] left, final boolean[] right, final int offset) {
        var leftVec = VectorMask.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = VectorMask.fromArray(BYTE_SPECIES, right, offset);
        leftVec.and(rightVec).intoArray(left, offset);
        return left;
    }

}
