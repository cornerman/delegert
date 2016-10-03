# delegert

delegert delegates things...in scala

## Usage

```scala
trait Tret {
  def a(): String
  def b(v: Int): String
}

class TretImpl extends Tret {
  def a() = "a"
  def b(v: Int) = "b" + v
}

object Example {
  import delegert.delegert

  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
  }
}
```

This generates the following implementation of TretWrap:

```scala
  class TretWrap(@delegert val inner: Tret) extends Tret {
    def a() = "myA"
    def b(v: Int) = inner.b(v)
  }
```
