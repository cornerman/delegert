import org.specs2.mutable.Specification

class ExampleSpec extends Specification {
  import delegert.example

  class Wrap {
    @example val x: Int
  }
}
