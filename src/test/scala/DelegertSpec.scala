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

// needs own block around macro usage, as current scope is not accessible in macro expansion: not found type Tret
// related: https://github.com/aztek/scala-workflow/issues/2
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

  "works on constructor parameter" >> {
    q"""
      trait Tret { def a(value: Int): Int }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a(value: Int) = A.this.inner.a(value)
          }"""
      ))
  }

  "ignores private methods in trait" >> {
    q"""
      trait Tret { private def foo = 1 }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""class A(val inner: Tret) extends Tret"""
      ))
  }

  "multiple parameter lists" >> {
    q"""
      trait Tret { def a(i: Int)(j: Int): Int }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a(i: Int)(j: Int) = A.this.inner.a(i)(j)
          }"""
      ))
  }.pendingUntilFixed

  "type parameters" >> {
    q"""
      trait Tret { def a[T](t: T): T }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a[T](t: T) = A.this.inner.a[T](t)
          }"""
      ))
  }.pendingUntilFixed

  "use existing implementations" >> {
    q"""
      trait Tret { def a: Int; def b(value: Int): String }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret {
          def a = 0
        }
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a = 0
            def b(value: Int) = A.this.inner.b(value)
          }"""
      ))
  }

  "works with overloading" >> {
    q"""
      trait Tret { def a: Int; def a(value: Int): Int; }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret {
          def a = -1
        }
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a = -1
            def a(value: Int) = A.this.inner.a(value)
          }"""
      ))
  }

  // TODO: embedding class?
  // "works on member value" >> {
  //   q"""{trait Tret { def a() = 1 }; trait A { @delegert.delegert val inner: Tret }}""" must compile
  // }
}
