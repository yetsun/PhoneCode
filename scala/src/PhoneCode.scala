import scala.io.Source

object phoneCode extends App {

  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

  //filter out Bhagavad-Gita,home-brew,Ibero-,Modula-2,Modula-3,Serbo-,Sino-
  //convert the iterator to list
  //  val words = in.getLines().filter(!_.contains("-")).filter(!_.contains("\'")).toList
  val words = in.getLines().filter(_.forall(_.isLetter)).toList


  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")


  //"A" -> 2
  val charCode: Map[Char, Char] = for {
    (number, letters) <- mnem
    letter <- letters
  } yield letter -> number


  //"xxxxx" to "99999"
  def wordCode(word: String): String = {
    word.toUpperCase map charCode
  }


  //"99999" -> List("xxxxx", "yyyyy")
  val wordsForNumbers: Map[String, Seq[String]] =
    (words groupBy wordCode).withDefaultValue(List())


  val phoneNumber = wordCode("scalaisfun") //7225247386


  def foo(numbers: String): Set[List[String]] = {
    //println(numbers)

    if (numbers.isEmpty) {
      Set(List())
    } else {
      (for {
        i <- 1 to numbers.length
        word <- wordsForNumbers(numbers.take(i))
        rest <- foo(numbers drop i)
      } yield word :: rest).toSet
    }
  }

  val result = foo(phoneNumber)
  print(result.map(_.mkString(" ")).mkString("\n"))


}