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
        val methods = tpe.decls.filter(_.isMethod).map(_.asMethod)

        val ClassDef(mods, name, tparams, Template(parents, self, body)) = embeddingClass
        val existingMethods = body.flatMap(tree => tree match {
          case DefDef(_, n, _, _, _, _) => Some(n)
          case _ => None
        }).toSet

        val missingMethods = methods.filterNot(m => existingMethods.contains(m.name))

        val methodsImpls = missingMethods.map { method =>
          val (params, paramNames) = method.paramLists.headOption.map(_.map { param => //TODO multi paramlists
            val tpe = param.typeSignature
            val name = TermName(param.name.toString)
            (q"$name: $tpe", q"$name")
          }.unzip).getOrElse((List.empty, List.empty))

          q"def ${method.name}(..$params) = ${value.name}.${method.name}(..$paramNames)"
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
