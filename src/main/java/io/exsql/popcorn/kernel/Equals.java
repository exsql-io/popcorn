package io.exsql.popcorn.kernel;

import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorSpecies;

public class Equals {

    private Equals() {}

    private static final VectorSpecies<Byte> BYTE_SPECIES = ByteVector.SPECIES_PREFERRED;

    public static boolean equals(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }
        
        if (left.length < bound) {
            return equals(left, right, 0);
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!equals(left, right, i)) {
                return false;
            }
        }

        for (; i < left.length; i++) {
            if (left[i] != right[i]) {
                return false;
            }
        }

        return true;
    }

    private static boolean equals(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.eq(rightVec).allTrue();
    }

}
