package aufgaben

object SBaum:

  enum SBaum:
    case SBLeer()
    case Knoten(wert: Int, li: SBaum, re: SBaum)

  import SBaum.*

  def ein(x: Int, b: SBaum): SBaum =
    b match
      case SBLeer() => Knoten(x, SBLeer(), SBLeer())
      case Knoten(w, li, re) =>
        if w < x then Knoten(w, li, ein(x, re))
        else Knoten(w, ein(x, li), re)

  def inBaum(x: Int, b: SBaum): Boolean =
    b match
      case SBLeer() => false
      case Knoten(w, li, re) =>
        if (x == w) then true
        else
          if (x < w) then inBaum(x, li)
          else inBaum(x, re)

  def headSB(b: SBaum): Int =
    b match
      case Knoten(w, SBLeer(), _) => w
      case Knoten(_,Knoten(w, li, re), _) => headSB(Knoten(li, w, re))

  def tailSB(b: SBaum): SBaum =
    b match
      case Knoten(_, SBLeer(), re) => re
      case Knoten(w, li, re) => Knoten(w, tailSB(li), re)

  def lastSB(b: SBaum): Int =
    b match
      case Knoten(w, _, SBLeer()) => w
      case Knoten(_, _, Knoten(w, li, re)) => lastSB(Knoten(w, li, re))

  def initSB(b: SBaum): SBaum =
    b match
      case Knoten(_, li, SBLeer()) => li
      case Knoten(w, li, re) => Knoten(w, li, initSB(re))


  def sizeSB(sb: SBaum): Int =
    sb match
      case SBLeer() => 0
      case Knoten(w, l, r) =>
        sizeSB(l)  + 1 + sizeSB(r)

  def loesche(x: Int, sb: SBaum): SBaum =
    sb match
      case SBLeer() => SBLeer()
      case Knoten(w, li, re) =>
        if (x < w) then Knoten(w, loesche(x, li), re)
        else
          if (x > w) then Knoten(w, li, loesche(x, re))
          // x= current wert
          if li == SBLeer() then re
          else
            if re == SBLeer() then li
            else
              if (sizeSB(li) > sizeSB(re)) then Knoten()

  def list(b: SBaum): List[Int] =
    b match
      case SBLeer() => List()
      case Knoten(w, li, re) =>
        list(li) ::: w :: list(re)

  def einL(a: List[Int], b: SBaum): Unit =
    a.foldRight(b)((x, b) => ein(x, b)

  def main(args: Array[String]): Unit =

    var b: SBaum = SBLeer()

    val a = List(1, 3, -2, 4, 9)

    for (x <- a) do
      b = ein(x, b)


    print(list(b))
alskjdklasjklaskdj
salkajsdlk

as


laskjdlkjasklda