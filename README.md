# delegert

delegert delegates things...in scala

## Usage

Annotate class or trait members and let delegert implement delegate methods.

```scala
trait Tret {
  def a(): String
  def b(v: Int): String
}

object Example {
  import delegert.delegert

  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }

  trait TretWrapTret extends Tret {
    @delegert val inner: Tret
    def a() = "myA"
  }
}
```

This generates the following implementation of TretWrap:

```scala
  class TretWrap(val inner: Tret) extends Tret {
    def a() = "myA"
    def b(v: Int) = inner.b(v)
  }

  trait TretWrapTret extends Tret {
    val inner: Tret
    def a() = "myA"
    def b(v: Int) = inner.b(v)
  }
```
