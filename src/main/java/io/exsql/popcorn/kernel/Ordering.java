package io.exsql.popcorn.kernel;

import jdk.incubator.vector.ByteVector;
import jdk.incubator.vector.VectorSpecies;

import static jdk.incubator.vector.VectorOperators.*;

public class Ordering {

    private Ordering() {}

    private static final VectorSpecies<Byte> BYTE_SPECIES = ByteVector.SPECIES_PREFERRED;

    public static boolean equal(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }
        
        if (left.length < bound) {
            return equal(left, right, 0);
        }
        
        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!equal(left, right, i)) {
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

    public static boolean notEqual(final byte[] left, final byte[] right) {
        return !equal(left, right);
    }

    public static boolean greaterThan(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }

        if (left.length < bound) {
            return greaterThan(left, right, 0);
        }

        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!greaterThan(left, right, i)) {
                return false;
            }
        }

        for (; i < left.length; i++) {
            if (left[i] <= right[i]) {
                return false;
            }
        }

        return true;
    }

    public static boolean greaterThanOrEqual(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }

        if (left.length < bound) {
            return greaterThanOrEqual(left, right, 0);
        }

        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!greaterThanOrEqual(left, right, i)) {
                return false;
            }
        }

        for (; i < left.length; i++) {
            if (left[i] < right[i]) {
                return false;
            }
        }

        return true;
    }

    public static boolean lessThan(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }

        if (left.length < bound) {
            return lessThan(left, right, 0);
        }

        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!lessThan(left, right, i)) {
                return false;
            }
        }

        for (; i < left.length; i++) {
            if (left[i] >= right[i]) {
                return false;
            }
        }

        return true;
    }

    public static boolean lessThanOrEqual(final byte[] left, final byte[] right) {
        final int bound = BYTE_SPECIES.loopBound(left.length);
        if (left.length != right.length) {
            return false;
        }

        if (left.length < bound) {
            return lessThanOrEqual(left, right, 0);
        }

        var i = 0;
        for (; i < bound; i += BYTE_SPECIES.length()) {
            if (!lessThanOrEqual(left, right, i)) {
                return false;
            }
        }

        for (; i < left.length; i++) {
            if (left[i] > right[i]) {
                return false;
            }
        }

        return true;
    }

    private static boolean equal(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.eq(rightVec).allTrue();
    }

    private static boolean greaterThan(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.compare(GT, rightVec).allTrue();
    }

    private static boolean greaterThanOrEqual(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.compare(GE, rightVec).allTrue();
    }

    private static boolean lessThan(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.compare(LT, rightVec).allTrue();
    }

    private static boolean lessThanOrEqual(final byte[] left, final byte[] right, final int offset) {
        var leftVec = ByteVector.fromArray(BYTE_SPECIES, left, offset);
        var rightVec = ByteVector.fromArray(BYTE_SPECIES, right, offset);
        return leftVec.compare(LE, rightVec).allTrue();
    }

}
