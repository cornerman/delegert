package delegert

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

case class ValueAccessor(name: String, typeName: String)

class DelegertTranslator[C <: Context](val context: C) {
  import context.universe._

  def translate(value: ValueAccessor): Tree = {
    q"val ${TermName(value.name)}: ${TypeName(value.typeName)} = ${EmptyTree}"
  }
}

object DelegertTranslator {
  def apply(c: Context): DelegertTranslator[c.type] = new DelegertTranslator(c)
}

object DelegertMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val translator = DelegertTranslator(c)
    import translator._

    val trees = annottees.map(_.tree)
    val value = trees.headOption map {
      case q"..$mods val $name: $typeName = ${EmptyTree}" => ValueAccessor(name.toString, typeName.toString)
      case _ => c.abort(c.enclosingPosition, "delegert only delegates value accessors")
    } getOrElse {
      c.abort(c.enclosingPosition, "delegert does not annotate anything")
    }

    val outputs = translate(value) +: trees.tail
    c.Expr[Any](q"..$outputs")
  }
}

@compileTimeOnly("only for compile time expansion")
class delegert extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegertMacro.impl
}
