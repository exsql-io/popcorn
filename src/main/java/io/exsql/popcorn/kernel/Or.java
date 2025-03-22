package io.exsql.popcorn.kernel;

import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorMask;
import jdk.incubator.vector.VectorSpecies;

public class Or {

    private Or() {}

    private static final VectorSpecies<Byte> BYTE_SPECIES = ByteVector.SPECIES_PREFERRED;

    public static boolean[] or(final boolean[] left, final boolean[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length < bound) {
            return or(left, right, 0);
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            or(left, right, i);
        }

        for (; i < left.length; i++) {
            left[i] |= right[i];
        }

        return left;
    }

    private static boolean[] or(final boolean[] left, final boolean[] right, final int offset) {
        var leftVec = VectorMask.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = VectorMask.fromArray(BYTE_SPECIES, right, offset);
        leftVec.or(rightVec).intoArray(left, offset);
        return left;
    }

}
