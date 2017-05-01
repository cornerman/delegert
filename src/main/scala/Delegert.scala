package delegert

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.util.{Success, Failure}

class DelegertTranslator[C <: Context](val c: C) {
  import c.universe._

  case class ValueAccessor(name: TermName, tpe: Type)
  case class MethodInfo(name: TermName, paramLists: List[List[Symbol]], typeParams: List[Symbol])

  def symbolOwnerChain(sym: Symbol): List[Symbol] = {
    sym.owner match {
      case NoSymbol => sym :: Nil
      case owner => sym :: symbolOwnerChain(owner)
    }
  }

  def enclosingOwnerClasses: Seq[ClassSymbol] = {
    val owner = c.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(owner)
    ownerChain collect { case s: ClassSymbol => s }
  }

  def methodToDef(value: ValueAccessor)(methodSymbol: MethodSymbol): Tree = {
    val method = methodSymbol.typeSignatureIn(value.tpe)
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

  def treeToValueAccessor(valDef: ValDef): Either[String, ValueAccessor] = {
    val unmoddedTree = ValDef(Modifiers(), valDef.name, valDef.tpt, valDef.rhs)
    val typedTree = util.Try(c.typecheck(unmoddedTree.duplicate, withMacrosDisabled = true))
    typedTree match {
      case Success(q"..$mods val $name: $typeTree = $initTree") =>
        typeTree.tpe match {
          case null | NoType => Left("type tree does not have type")
          case tpe => Right(ValueAccessor(name, tpe))
        }
      case Success(_) => Left("delegert only delegates value accessors")
      case Failure(err) => Left(s"cannot typecheck given expression: $err")
    }
  }

  def translate(value: ValueAccessor, excludedMethods: Seq[MethodInfo] = Seq.empty): List[Tree] = {
    val neededMethods = methodsInType(value.tpe)
    val missingMethods = neededMethods.filterNot { m =>
      excludedMethods.exists(e => e.name == m.name && e.typeParams == m.typeParams && e.paramLists == m.paramLists)
    }

    missingMethods.map(methodToDef(value)).toList
  }

  //TODO: crashes on embeddingClass.toType.decls
  // def translateToMembers(value: ValueAccessor, embeddingClass: ClassSymbol): List[Tree] = {
  //   val existingMethods = embeddingClass.toType.decls.filter(_.isMethod).map(_.asMethod).toSeq
  //   val existingMethods = Seq.empty[MethodSymbol]
  //   translate(value, existingMethods.map(m => MethodInfo(m.name, m.paramLists, m.typeParams)))
  // }

  def translateToMembers(value: ValueAccessor, embeddingClass: ImplDef): List[Tree] = {
    val Template(_, _, body) = embeddingClass.impl
    val existingMethods = body collect { case d: DefDef => d }
    translate(value, existingMethods.map(m => MethodInfo(m.name, m.vparamss.map(_.map(_.symbol)), m.tparams.map(_.symbol))))
  }

  def translateToClass(value: ValueAccessor, embeddingClass: ClassDef): ClassDef = {
    val ClassDef(mods, name, tparams, Template(parents, self, body)) = embeddingClass
    val members = translateToMembers(value, embeddingClass)
    ClassDef(mods, name, tparams, Template(parents, self, body ++ members))
  }
}

object DelegertTranslator {
  def apply(c: Context): DelegertTranslator[c.type] = new DelegertTranslator(c)
}

object DelegertMacro {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val translator = DelegertTranslator(c)
    annottees.map(_.tree) match {
      case (valDef: ValDef) :: (classDef: ClassDef) :: rest =>
        translator.treeToValueAccessor(valDef) match {
          case Left(err) => c.abort(c.enclosingPosition, err)
          case Right(value) =>
            val translated = translator.translateToClass(value, classDef)
            val outputs = translated +: rest
            c.Expr[Any](q"..$outputs")
        }
      case (valDef: ValDef) :: rest =>
        // TODO: should use enclosingOwner instead of enclosingClass
        // val classOpt = translator.enclosingOwnerClasses.headOption
        val classOpt = Some(c.enclosingClass) collect { case c: ImplDef => c }
        classOpt match {
          case None =>
            c.abort(c.enclosingPosition, "annotated value accessor be embedded in a impl definition")
          case Some(classDef) =>
            translator.treeToValueAccessor(valDef) match {
              case Left(err) => c.abort(c.enclosingPosition, err)
              case Right(value) =>
                val translated = translator.translateToMembers(value, classDef)
                val outputs = valDef :: translated ++ rest
                c.Expr[Any](q"..$outputs")
            }
        }
      case _ =>
        c.abort(c.enclosingPosition, "delegert can only annotate value accessors")
    }
  }
}

@compileTimeOnly("only for compile time expansion")
class delegert extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DelegertMacro.impl
}
