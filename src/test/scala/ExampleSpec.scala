import org.specs2.mutable.Specification
import delegert.delegert

trait Tret {
  def a(): String
  def b(): String
}

trait Tret2 {
  def b(): String
  def c(): String
}

class TretImpl extends Tret {
  def a() = "a"
  def b() = "b"
}

class Tret2Impl extends Tret2 {
  def b() = "b2"
  def c() = "c2"
}

class ExampleSpec extends Specification {
  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  class TretWraps(@delegert val inner: Tret, @delegert val inner2: Tret2) extends Tret with Tret2 {
    def a() = "myA"
  }

  "one delegate" >> {
    val wrap = new TretWrap(new TretImpl)
    wrap.a must beEqualTo("myA")
    wrap.b must beEqualTo("b")
  }

  "two delegates" >> {
    val wrap = new TretWraps(new TretImpl, new Tret2Impl)
    wrap.a must beEqualTo("myA")
    wrap.b must beEqualTo("b")
    wrap.c must beEqualTo("c2")
  }
}
