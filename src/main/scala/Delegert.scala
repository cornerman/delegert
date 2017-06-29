package delegert

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.util.{Success, Failure}

object Macro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    annottees.map(_.tree) match {
      case (valDef: ValDef) :: _ =>

        //this works in scala 2.11.11 and 2.12.2
        c.typecheck(valDef, withMacrosDisabled = true)

        //this works in scala 2.11.11, but not in 2.12.2
        val unmodValDef = ValDef(Modifiers(), valDef.name, valDef.tpt, valDef.rhs)
        c.typecheck(unmodValDef, withMacrosDisabled = true)

        annottees.head
      case _ => c.abort(c.enclosingPosition, "unsupported annotation")
    }
  }
}

@compileTimeOnly("only for compile time expansion")
class example extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macro.impl
}
