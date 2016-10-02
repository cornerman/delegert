import macroni.CompileSpec
import scala.reflect.runtime.universe._

class DelegertSpec extends CompileSpec {
  "works on constructor value" >> {
    q"""trait Tret; class A(@delegert.delegert val a: Tret)""" must compile
  }

  "works on member value" >> {
    q"""trait A { @delegert.delegert val a: A }""" must compile
  }

  "fails on var" >> {
    q"""trait Tret; class A(@delegert.delegert var a: Tret)""" must abort(
      "reflective typecheck has failed: delegert only delegates value accessors"
    )
  }
}
