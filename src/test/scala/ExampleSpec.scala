import org.specs2.mutable.Specification
import delegert.delegert

trait Tret {
  def a(): String
  def b(): String
}

class TretImpl extends Tret {
  def a() = "a"
  def b() = "b"
}

class ExampleSpec extends Specification {
  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  "let delegert delegate" >> {
    val wrap = new TretWrap(new TretImpl)
    wrap.a must beEqualTo("myA")
    wrap.b must beEqualTo("b")
  }
}
