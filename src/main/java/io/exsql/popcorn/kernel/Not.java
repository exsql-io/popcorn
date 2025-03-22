package io.exsql.popcorn.kernel;

import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorMask;
import jdk.incubator.vector.VectorSpecies;

public class Not {

    private Not() {}

    private static final VectorSpecies<Byte> BYTE_SPECIES = ByteVector.SPECIES_PREFERRED;

    public static boolean[] not(final boolean[] values) {
        final int bound = BYTE_SPECIES.loopBound(values.length);
        if (values.length < bound) {
            return not(values, 0);
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            not(values, i);
        }

        for (; i < values.length; i++) {
            values[i] = !values[i];
        }

        return values;
    }

    private static boolean[] not(final boolean[] values, final int offset) {
        var vec = VectorMask.fromArray(BYTE_SPECIES, values, offset);
        vec.not().intoArray(values, offset);
        return values;
    }

}
