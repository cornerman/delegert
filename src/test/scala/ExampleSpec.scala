import org.specs2.mutable.Specification

class ExampleSpec extends Specification {
  import delegert.example

  // using a trait instead of a class compiles in 2.12.2
  class Wrap {
    @example val x: Int = 1
  }
}
