package delegert

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.util.{Success, Failure}

class DelegertTranslator[C <: Context](val c: C) {
  import c.universe._

  case class ValueAccessor(name: TermName, typeTree: Tree, initTree: Tree)

  def unmodValDefs(tree: Tree): Tree = tree match {
    case ValDef(_, name, tpe, init) => ValDef(Modifiers(), name, tpe, init)
    case t => t
  }

  def methodToDef(value: ValueAccessor)(methodSymbol: MethodSymbol): Tree = {
    val method = methodSymbol.typeSignatureIn(value.typeTree.tpe)
    val methodName = methodSymbol.name

    val paramArgs = method.paramLists.map(_.map(_.name))
    val params = method.paramLists.map(_.map { param =>
      val name = TermName(param.name.toString)
      val tpe = param.typeSignature

      q"val $name: $tpe"
    })

    val tparamArgs = method.typeParams.map(_.name)
    val tparams = {
      import compat._ // TODO
      method.typeParams.map(TypeDef(_))
    }

    q"override def ${methodName}[..$tparams](...$params) = ${value.name}.${methodName}[..$tparamArgs](...$paramArgs)"
  }

  def methodsInType(tpe: Type): Iterable[MethodSymbol] = {
    val members = tpe.members.toSeq.diff(typeOf[AnyRef].members.toSeq).sortBy(_.name.toString)
    members.filter { decl =>
      decl.isMethod && !decl.isPrivate && !decl.isConstructor
    }.map(_.asMethod)
  }

  def treeToValueAccessor(moddedTree: Tree): ValueAccessor = {
    val tree = unmodValDefs(moddedTree)
    val typedTree = util.Try(c.typecheck(tree.duplicate, withMacrosDisabled = true))
    typedTree match {
      case Success(q"..$mods val $name: $typeTree = $initTree") =>
        if (typeTree.tpe == null || typeTree.tpe == NoType)
          c.abort(c.enclosingPosition, "type tree does not have type")
        ValueAccessor(name, typeTree, initTree)
      case Success(_) => c.abort(c.enclosingPosition, "delegert only delegates value accessors")
      case Failure(err) => c.abort(c.enclosingPosition, s"cannot typecheck given expression: $err")
    }
  }

  def translate(value: ValueAccessor, embeddingClass: ClassDef): Tree = {
    val ClassDef(mods, name, tparams, Template(parents, self, body)) = embeddingClass

    val existingMethods = body collect { case d: DefDef => d }
    val neededMethods = methodsInType(value.typeTree.tpe)
    val missingMethods = neededMethods.filterNot { m =>
      existingMethods.exists(e => e.name == m.name && e.tparams == m.typeParams && e.vparamss == m.paramLists)
    }

    val newBody = body ++ missingMethods.map(methodToDef(value: ValueAccessor))
    ClassDef(mods, name, tparams, Template(parents, self, newBody))
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
    trees.headOption map { annottee =>
      val value = translator.treeToValueAccessor(annottee)
      val nextAnnottees = trees.tail.collect {
        case c: ClassDef => translator.translate(value, c)
        case t => t
      }

      val outputs = nextAnnottees
      c.Expr[Any](q"..$outputs")
    } getOrElse(c.abort(c.enclosingPosition, "delegert does not annotate anything"))
  }
}

@compileTimeOnly("only for compile time expansion")
class delegert extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegertMacro.impl
}
