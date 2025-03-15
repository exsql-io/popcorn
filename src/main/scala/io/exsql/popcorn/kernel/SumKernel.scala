package io.exsql.popcorn.kernel

import io.exsql.popcorn.kernel.Kernel.Kernel
import jdk.incubator.vector.{IntVector, VectorSpecies}

class SumKernel extends Kernel[Int, Array[Int]] {
  override def execute(args: Array[Int]*): Array[Int] = {
    val left = args.head
    val right = args(1)
    val species = IntVector.SPECIES_PREFERRED
    val length = left.length
    val result = Array.ofDim[Int](length)
    
    for
      i <- 0 to (length - species.length()) by species.length()
    do {
      val vLeft = IntVector.fromArray(species, left, i)
      val vRight = IntVector.fromArray(species, right, i)
      val sum = vLeft.add(vRight)
      sum.intoArray(result, i)  
    }
    
    result
  }
}
