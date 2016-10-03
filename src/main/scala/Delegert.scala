package delegert

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.util.{Try => uTry, Success, Failure}

class DelegertTranslator[C <: Context](val c: C) {
  import c.universe._

  case class ValueAccessor(name: TermName, typeTree: Tree, initTree: Tree)

  def unmodValDefs(tree: Tree): Tree = tree match {
    case ValDef(_, name, tpe, init) => ValDef(Modifiers(), name, tpe, init)
    case t => t
  }

  def translate(value: ValueAccessor, embeddingClass: ClassDef): Tree = {
    Option(value.typeTree.tpe) match {
      case Some(tpe) =>
        val ClassDef(mods, name, tparams, Template(parents, self, body)) = embeddingClass

        val methods = tpe.decls.filter(d => d.isMethod && !d.isPrivate && !d.isConstructor).map(_.asMethod)
        val existingMethods = body collect { case d: DefDef => d }
        val missingMethods = methods.filterNot { m =>
          existingMethods.exists(e => e.name == m.name && e.tparams == m.typeParams && e.vparamss == m.paramLists)
        }

        val methodsImpls = missingMethods.map { method =>
          val params = method.paramLists.map(_.map { param =>
            val name = TermName(param.name.toString)
            val tpe = param.typeSignature

            q"val $name: $tpe"
          })

          val paramArgs = method.paramLists.map(_.map(_.name))

          val tparams = {
            import compat._ // TODO
            method.typeParams.map(TypeDef(_))
          }

          val tparamArgs = method.typeParams.map(_.name)

          q"override def ${method.name}[..$tparams](...$params) = ${value.name}.${method.name}[..$tparamArgs](...$paramArgs)"
        }

        ClassDef(mods, name, tparams, Template(parents, self, body ++ methodsImpls))
      case None => c.abort(c.enclosingPosition, "type tree does not have type")
    }
  }

  def translate(utree: Tree, embeddingClass: ClassDef): Tree = {
    val tree = unmodValDefs(utree)
    val typedTree = uTry(c.typecheck(tree.duplicate, silent = false, withMacrosDisabled = true))
    val value = typedTree match {
      case Success(q"..$mods val $name: $typeTree = $initTree") => ValueAccessor(name, typeTree, initTree)
      case Success(_) => c.abort(c.enclosingPosition, "delegert only delegates value accessors")
      case Failure(err) => c.abort(c.enclosingPosition, s"cannot typecheck given expression: $err")
    }

    translate(value, embeddingClass)
  }
}

object DelegertTranslator {
  def apply(c: Context): DelegertTranslator[c.type] = new DelegertTranslator(c)
}

object DelegertMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val translator = DelegertTranslator(c)
    val trees = annottees.map(_.tree)
    val annottee = trees.head
    val nextAnnottees = trees.tail.collect {
      case c: ClassDef => translator.translate(annottee, c)
      case t => t
    }

    val outputs = nextAnnottees
    c.Expr[Any](q"..$outputs")
  }
}

@compileTimeOnly("only for compile time expansion")
class delegert extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegertMacro.impl
}
