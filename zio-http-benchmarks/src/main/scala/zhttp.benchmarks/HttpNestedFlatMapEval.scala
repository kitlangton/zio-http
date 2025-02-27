package zhttp.benchmarks

import org.openjdk.jmh.annotations._
import zhttp.http._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class HttpNestedFlatMapEval {

  private val MAX = 10000

  val programFlatMap: Http[Any, Nothing, Int, Int] =
    (0 to MAX).foldLeft(Http.identity[Int])((a, _) => a.flatMap(i => Http.succeed(i + 1)))

  @Benchmark
  def benchmarkHttpFlatMap(): Unit = {
    programFlatMap.evaluate(0).asOut
    ()
  }
}
