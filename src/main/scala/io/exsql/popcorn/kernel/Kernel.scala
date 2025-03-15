package io.exsql.popcorn.kernel

object Kernel {

  trait Kernel[Input, Output] {
    def execute(args: Array[Input]*): Output
  }

  type UnitKernel[Input] = Kernel[Input, Unit]

  def sum(left: Array[Int], right: Array[Int]): Array[Int] = {
    new SumKernel().execute(left, right)
  }

  def println[T](input: Array[T]): Unit = {
    new PrintlnKernel[T].execute(input)
  }

}