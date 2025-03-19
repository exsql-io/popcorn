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
            var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, 0);
            var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, 0);
            return leftVec.eq(rightVec).allTrue();
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, i);
            var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, i);
            if (!leftVec.eq(rightVec).allTrue()) {
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

}
