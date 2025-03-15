package io.exsql.popcorn.kernel

import io.exsql.popcorn.kernel.Kernel.UnitKernel

class PrintlnKernel[Input] extends UnitKernel[Input] {
  override def execute(args: Array[Input]*): Unit = {
    println(args.head.mkString("[", ",", "]"))
  }
}
