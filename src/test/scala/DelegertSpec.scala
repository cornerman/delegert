import macroni.CompileSpec
import scala.reflect.runtime.universe._

class DelegertSpec extends CompileSpec {
  // needs own block around macro usage, as current scope is not accessible in macro expansion: not found type Tret
  // related: https://github.com/aztek/scala-workflow/issues/2

  "with different Type and implicit return" >> {
    q"""
      trait Tret { def a(value: Int): String }
      trait Diff { def a(value: Int): Int }
      {
        implicit def intToString(i: Int): String = i.toString
        class A(@delegert.delegert val inner: Diff) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Diff) extends Tret {
            def a(value: Int) = intToString(A.this.inner.a(value))
          }"""
      ))
  }

  "method with one parameter" >> {
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

  "method without brackets" >> {
    q"""
      trait Tret { def no: Int }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""class A(val inner: Tret) extends Tret {
          def no = A.this.inner.no
        }"""
      ))
  }

  "method with default arguments" >> {
    q"""
      trait Tret { def jo(a: Int = 1): Int }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""class A(val inner: Tret) extends Tret {
          def jo(a: Int) = A.this.inner.jo(a)
          def jo$$default$$1 = A.this.inner.jo$$default$$1
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
  }

  "type parameter without argument" >> {
    q"""
      trait Tret { def a[T]: T }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a[T] = A.this.inner.a[T]
          }"""
      ))
  }

  "type parameter with argument" >> {
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

  // cannot find type T in ValDef, need to typecheck whole unit?
  "generic trait" >> {
    q"""
      trait Tret[T] { def a(t: T): T }
      {
        class A[T](@delegert.delegert val inner: Tret[T]) extends Tret[T]
      }""" must compile.to(containTree(
        q"""
          class A[T](val inner: Tret[T]) extends Tret[T] {
            def a(t: T) = A.this.inner.a(t)
          }"""
      ))
  }.pendingUntilFixed

  "generic trait and concrete wrapper" >> {
    q"""
      trait Tret[T] { def a(t: T): T }
      {
        class A(@delegert.delegert val inner: Tret[Int]) extends Tret[Int]
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret[scala.Int]) extends Tret[scala.Int] {
            def a(t: Int) = A.this.inner.a(t)
          }"""
      ))
  }

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

  //TODO: is this desired or error out?
  "multiple delegerts" >> {
    q"""
      trait Tret { def a(): Int; def b(): Int }
      trait Tret2 { def a(): Int; def c(): Int }
      {
        class A(@delegert.delegert val inner: Tret, @delegert.delegert val inner2: Tret2) extends Tret with Tret2
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret, val inner2: Tret2) extends Tret with Tret2 {
            def a() = A.this.inner.a()
            def b() = A.this.inner.b()
            def c() = A.this.inner2.c()
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

  "find inherited methods" >> {
    q"""
      trait TretsTret { def a: Int }
      trait Tret extends TretsTret { def b: Int }
      {
        class A(@delegert.delegert val inner: Tret) extends Tret
      }""" must compile.to(containTree(
        q"""
          class A(val inner: Tret) extends Tret {
            def a = A.this.inner.a
            def b = A.this.inner.b
          }"""
      ))
  }

  "wrap Seq[T]" >> {
    q"class A[T](@delegert.delegert val inner: Seq[T]) extends Seq[T]" must compile
  }.pendingUntilFixed

  "works on member value" >> {
    q"""
      trait Tret { def a: Int; def b: Int }
      {
        trait A extends Tret {
          @delegert.delegert val inner: Tret = null
          def a = 2
        }
      }""" must compile.to(containTree(
        q"""
          trait A extends Tret {
            val inner: Tret = null
            def b = A.this.inner.b
            def a = 2
          }"""
      ))
  }
}
