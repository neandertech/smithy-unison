package smithyu.codegen

trait Lines { self =>
  def get: List[String]
  def render = get.mkString(System.lineSeparator())

  def map(f: String => String) = Lines.fromList(get.map(f))

  def distinct: Lines                    = Lines.fromList(get.distinct)
  def sorted: Lines                      = Lines.fromList(get.sorted)
  def ++(other: Lines): Lines            = new Lines {
    def get: List[String] = self.get ++ other.get
  }
  def prependToFirst(str: String): Lines = new Lines {
    def get: List[String] = {
      val g        = self.get
      val newFirst = str + g.headOption.getOrElse("")
      newFirst +: (g.drop(1))
    }
  }
  def appendToLast(str: String): Lines   = new Lines {
    def get: List[String] = {
      val g       = self.get
      val newLast = g.lastOption.getOrElse("") + str
      (g.dropRight(1)) :+ newLast
    }
  }

  def indent: Lines = map { str =>
    if (!str.isBlank()) { "  " + str }
    else { str }
  }

  def renderAsSingleLine: String = self.get.mkString

}

object Lines {

  object empty   extends Lines {
    def get: List[String] = Nil
  }
  object newline extends Lines {
    def get: List[String] = List("")
  }

  def fromList(lines: List[String]): Lines = new Lines {
    def get: List[String] = lines
  }

  def when(condition: Boolean)(lines: Lines*): Lines =
    if (condition) apply(lines*) else empty

  def make[A](lines: List[A])(implicit toLines: ToLines[A]): Lines = new Lines {
    def get: List[String] = lines.flatMap(toLines.lines(_))
  }

  def apply(all: Lines*): Lines = new Lines {
    def get: List[String] = all.flatMap(_.get).toList
  }

  def indent(all: Lines*): Lines = apply(all*).indent

  implicit def fromToLines[A: ToLines](a: A): Lines = new Lines {
    def get = ToLines(a)
  }

}

private[codegen] trait ToLines[A] {
  def lines(value: A): List[String]
}

private[codegen] object ToLines {

  def apply[A](value: A)(implicit ev: ToLines[A]): List[String] =
    ev.lines(value)

  implicit val stringToLines: ToLines[String] = (a: String) =>
    a.linesIterator.toList

  implicit def flattenedLines[A: ToLines]: ToLines[List[A]] = (a: List[A]) =>
    a.flatMap(ToLines(_))

  implicit def flattenedLinesOption[A: ToLines]: ToLines[Option[A]] =
    (a: Option[A]) => a.toList.flatMap(ToLines(_))

  implicit def linesToLInes: ToLines[Lines] = (a: Lines) => a.get

}
