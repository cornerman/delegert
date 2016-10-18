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

  trait TretWrapTret extends Tret {
    @delegert val inner: Tret
    def a() = "myA"
  }

  "let delegert delegate in class" >> {
    val wrap = new TretWrap(new TretImpl)
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }

  "let delegert delegate in trait" >> {
    val wrap = new TretWrapTret { val inner = new TretImpl }
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }
}
