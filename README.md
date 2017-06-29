# Steps to reproduce

[A macro annotation](https://github.com/cornerman/delegert/blob/cyclic-reference-2.12/src/main/scala/Delegert.scala) with paradise, which typechecks an annotated `ValDef`.

Works in scala 2.11.11:
```sh
sbt ++2.11.11 test
```

Does not work in scala 2.12.2:
```sh
sbt ++2.12.2 test
```

which leads to:
```
[error] [0] /scala/ExampleSpec.scala:7: illegal cyclic reference involving class Wrap
```

[Failing code](https://github.com/cornerman/delegert/blob/cyclic-reference-2.12/src/main/scala/Delegert.scala#L20):
```scala
val unmodValDef = ValDef(Modifiers(), valDef.name, valDef.tpt, valDef.rhs)
c.typecheck(unmodValDef, withMacrosDisabled = true)
```
