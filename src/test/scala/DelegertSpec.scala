import macroni.CompileSpec
import scala.reflect.runtime.universe._

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

object Wraps {
  class TretWrap(@delegert.delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  class TretWraps(@delegert.delegert val inner: Tret, @delegert.delegert val inner2: Tret2) extends Tret with Tret2 {
    def a() = "myA"
  }
}

class DelegertSpec extends CompileSpec {
  import Wraps._

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

  // needs own block around macro usage, as current scope is not accessible in macro expansion: not found type Tret
  // related: https://github.com/aztek/scala-workflow/issues/2
  "works on constructor value" >> {
    q"""
      trait Tret { def a() = 1 }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a() = A.this.inner.a()
          }"""
      ))
  }

  // TODO: embedding class?
  // "works on member value" >> {
  //   q"""{trait Tret { def a() = 1 }; trait A { @delegert.delegert val inner: Tret }}""" must compile
  // }
}
