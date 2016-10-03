import org.specs2.mutable.Specification

trait Tret {
  def a(): String
  def b(v: Int): String
}

class TretImpl extends Tret {
  def a() = "a"
  def b(v: Int) = "b" + v
}

class ExampleSpec extends Specification {
  import delegert.delegert

  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  "let delegert delegate" >> {
    val wrap = new TretWrap(new TretImpl)
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }
}
