import macroni.CompileSpec
import scala.reflect.runtime.universe._

class DelegertSpec extends CompileSpec {
  // needs own block around macro usage, as current scope is not accessible in macro expansion: not found type Tret
  // related: https://github.com/aztek/scala-workflow/issues/2

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

  // TODO: embedding class?
  // "works on member value" >> {
  //   q"""{trait Tret { def a() = 1 }; trait A { @delegert.delegert val inner: Tret }}""" must compile
  // }
}
