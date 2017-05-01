import org.specs2.mutable.Specification

trait Tret {
  def a(): String
  def b(v: Int): String
}

class TretImpl extends Tret {
  def a() = "a"
  def b(v: Int) = "b" + v
}

case class Data(x: Int)

class ExampleSpec extends Specification {
  import delegert.delegert

  class ClassWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  trait TraitWrap extends Tret {
    @delegert val inner: Tret
    def a() = "myA"
  }

  object ObjectWrap extends Tret {
    @delegert val inner: Tret = new TretImpl
    def a() = "myA"
  }

  class DataWrap(@delegert(vals) val inner: Data)

  "let delegert delegate in class" >> {
    val wrap = new ClassWrap(new TretImpl)
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }

  "let delegert delegate in trait" >> {
    val wrap = new TraitWrap { val inner = new TretImpl }
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }

  "let delegert delegate in object" >> {
    val wrap = ObjectWrap
    wrap.a must beEqualTo("myA")
    wrap.b(1) must beEqualTo("b1")
  }

  "let delegert delegate to case class" >> {
    val wrap = new DataWrap(Data(3))
    wrap.x must beEqualTo(3)
  }
}
