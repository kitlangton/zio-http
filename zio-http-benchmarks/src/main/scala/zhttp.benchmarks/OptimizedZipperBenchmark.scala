package zio.benchmarks

import org.openjdk.jmh.annotations._
import zio.http._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class OptimizedZipperBenchmark {

//  private val MAX = 1000
  import zio.http.api.internal.Box

  val box = Box.succeed(10) zip Box.succeed(20) zip Box.succeed(30) zip Box.succeed(40) zip
    Box.succeed(50) zip Box.succeed(60) zip Box.succeed(70)

  val optConstructor = Box.makeConstructor(box)
  val constructor    = Box.makeConstructor(box, false)

  @Benchmark
  def benchmarkOptimizedZip(): Unit = {
    val _ = optConstructor(Array())
  }

  @Benchmark
  def benchmarkUnoptimizedZip(): Unit = {
    val _ = constructor(Array())
  }
}
