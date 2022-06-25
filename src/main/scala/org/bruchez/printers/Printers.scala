package org.bruchez.printers

import java.io.{BufferedReader, InputStreamReader}
import java.net.{HttpURLConnection, URL}
import scala.collection.mutable._

object Printers {
  // scalastyle:off method.length cyclomatic.complexity
  def main(args: Array[String]): Unit = {
    if (args.size != 1) {
      println("File needed")
      for { a <- args } { println(a) }
    } else {
      val fileString = scala.io.Source.fromFile(args(0)).mkString

      val (printers0, books0) = parse(fileString)

      // println("Imprimeurs (first pass): "+printers0.size)

      val aliases = computeAliases(printers0)
      /*for (alias <- aliases) {
        println(alias._1+" -> "+alias._2)
      }*/

      val (printers, books) = parse(fileString, aliases)

      val printersBooks = HashMap[String, Int]()
      val printersPages = HashMap[String, Int]()

      for { printer <- printers } {
        val booksPrinter = books.map(_._2).filter(_.printers.contains(printer))
        if (printersBooks.contains(printer)) {
          println("Error: printersBooks(" + printer + ") already set!")
        }
        printersBooks(printer) = booksPrinter.size
        printersPages(printer) = booksPrinter.flatMap(_.pageCount).sum
      }

      println("Livres : " + books.size)
      println("Imprimeurs : " + printers.size)
      println()

      println("Imprimeurs par nombre de livres:")
      println()
      for { printerBooks <- printersBooks.toList.sortWith((i1, i2) => i1._2 < i2._2).reverse } {
        val pageCount = printersPages.get(printerBooks._1).map(_.toString).getOrElse("")
        println(printerBooks._1 + ", " + printerBooks._2 + ", " + pageCount)
      }
      println()

      println("Imprimeurs par nombre de pages:")
      println()
      for { printerPages <- printersPages.toList.sortWith((i1, i2) => i1._2 < i2._2).reverse } {
        val bookCount = printersBooks.get(printerPages._1).map(_.toString).getOrElse("")
        println(printerPages._1 + ", " + printerPages._2 + ", " + bookCount)
      }
      println()

      val formatsBooks = HashMap[Int, Int]()

      for { book <- books.values } {
        book.format foreach { format =>
          if (formatsBooks.contains(format)) {
            formatsBooks(format) += 1
          } else {
            formatsBooks(format) = 1
          }
        }
      }

      println("Nombre de livres par format:")
      println()
      for { (format, bookCount) <- formatsBooks } {
        println(format + ", " + bookCount)
      }
      println()

      val groups = HashSet[PrintersGroup]()
      for { book <- books } {
        val group = PrintersGroup(book._2.printers)
        if (group.printers.size >= 2) groups += group
      }

      println("Groupes d'imprimeurs : " + groups.size)
      println("+ = fait aussi partie d'un autre groupe")
      println("* = imprime aussi seul")
      println()

      def printsAlone(printer: String): Boolean =
        books.values
          .filter(book => book.printers.size == 1 && book.printers.contains(printer))
          .size > 0

      def isInMultipleGroups(printer: String): Boolean =
        groups.filter(_.printers.contains(printer)).size > 1

      for { group <- groups.toList.sortWith(_.length < _.length) } {
        val printableGroup = group.printers map { printer =>
          printer +
            (if (printsAlone(printer)) " (*)" else "") +
            (if (isInMultipleGroups(printer)) " (+)" else "")
        } reduceLeft (_ + ", " + _)
        println(printableGroup)
      }
    }
  }
  // scalastyle:on method.length cyclomatic.complexity

  def computeAliases(names: Set[String]): Map[String, String] = {
    val aliases = HashMap[String, String]()

    names foreach { name =>
      val candidateShortName = shortName(name)
      if (name != candidateShortName) {
        aliases.get(candidateShortName) foreach { existingAlias =>
          if (existingAlias != name) println("Conflict between " + existingAlias + " and " + name)
        }
        aliases.put(candidateShortName, name)
      }
    }

    aliases
  }

  def shortName(name: String): String = {
    def isLowerCase(c: Char): Boolean = c == c.toLower
    val words = name.split(" ")
    val firstWord = words.headOption
    firstWord filterNot { word =>
      word.endsWith(".") || word.size < 1 || isLowerCase(word.substring(0, 1).charAt(0))
    } map { word =>
      val newFirstWord = word.substring(0, 1) + "."
      val newName = (newFirstWord :: words.drop(1).toList).reduce(_ + " " + _)
      newName
    } getOrElse (name)
  }

  def applyAliases(name: String, aliases: Map[String, String]): String =
    aliases.get(name).getOrElse(name)

  def parse(
      fileString: String,
      aliases: Map[String, String] = Map[String, String]()
  ): (Set[String], Map[Int, Book]) = {
    val printers = HashSet[String]()
    val books = HashMap[Int, Book]()

    for { line <- CSV.parse(fileString).drop(1) } {
      val url = line(0)
      val html = urlToHtml(url)
      val urlPrinters = htmlToPrinters(
        html
      ) // Use the printers found in the URL first, instead of line(4)

      // scalastyle:off magic.number
      val book = Book(
        number = line(1).toInt,
        shortTitle = line(2),
        title = line(3),
        printers = explodePrinters(urlPrinters.getOrElse(line(4)), aliases).distinct,
        year = line(5),
        url = url,
        pageCount = htmlToPageCount(html),
        format = htmlToFormat(html)
      )
      // scalastyle:on magic.number

      books(book.number) = book
      book.printers.foreach(printers.add(_))
    }

    (printers, books)
  }

  // scalastyle:off null
  def urlToHtml(urlToRead: String): String = {
    // Straight from Java...
    var result = ""
    val url = new URL(urlToRead)
    val conn = url.openConnection().asInstanceOf[HttpURLConnection]
    conn.setRequestMethod("GET")
    val rd = new BufferedReader(new InputStreamReader(conn.getInputStream(), "UTF-8"))
    var line: String = null
    do {
      line = rd.readLine()
      if (line != null) result += line
    } while (line != null)
    rd.close()
    result
  }
  // scalastyle:on null

  // First .* is non-greedy (?), so that we find the first <td>...</td>
  private val RawPrinters = """Lieu/Imprimeur/Date :.*?<td valign="top">([^<]*)<\/td>""".r
  private val CleanedPrinters = """.*?: \[?([^.\]]+)\]?. \d*""".r
  private val RawCount = """Nb\. pages.*?<td valign="top">([^<]*)<\/td>""".r
  private val UnitlessCount = """(?:(?:\d+ ou )?\d+ vol\. *)?\[?(\d+)\]?""".r
  private val Formats = """Format :.*?<td valign="top">([^<]*)<\/td>""".r
  private val Signatures = """Signatures :.*?<td valign="top">([^<]*)<\/td>""".r

  def htmlToPrinters(html: String): Option[String] = {
    // Example of printers: [Genve] : Jean Crespin & Nicolas Barbier. 1555
    RawPrinters.findFirstMatchIn(html).map(_.group(1)) flatMap { rawPrinters =>
      CleanedPrinters.findFirstMatchIn(rawPrinters).map(_.group(1))
    }
  }

  // scalastyle:off method.length
  def htmlToPageCount(html: String): Option[Int] = {
    def multiplier(s: String): Option[Double] =
      if (s.endsWith("p.")) {
        // 1 page = 1 page
        Some(1)
      } else if (s.endsWith("f.")) {
        // 1 sheet = 2 pages
        Some(2)
      } else if (s.endsWith("col.")) {
        // 1 column = 0.5 page
        Some(0.5)
      } else if (
        s.endsWith("vol.") || s.endsWith("grav.") ||
        s.endsWith("part.") || s.endsWith("ill.") ||
        s.endsWith("planches hors texte") ||
        s.endsWith("planche hors texte")
      ) {
        // Ignore vol., grav., part., ill., etc.
        Some(0)
      } else {
        None
      }

    def unitlessCount(s: String): Option[Int] =
      UnitlessCount.findFirstMatchIn(s.trim).map(_.group(1).trim.toInt)

    RawCount.findFirstMatchIn(html).map(_.group(1)) map { rawCount =>
      // Fix typos, fix special characters, and remove comments
      rawCount
        // End of string comments (start with colon)
        .replaceAll(";.*$", "")
        // Inline comments (surrounded by parentheses)
        .replaceAll("\\(.*?\\)", "")
        // Sheet: missing dot at end of string
        .replaceAll(" f$", " f.")
        // Page: missing dot at end of string
        .replaceAll(" p$", " p.")
        // Sheet: extra space
        .replaceAll(" f \\.", " f.")
        // Page: extra space
        .replaceAll(" p \\.", " p.")
        // "mus." end of string comment: missing colon
        .replaceAll("\\., mus\\.$", ".; mus.")
        // Replace no-break space with regular space
        .replaceAll("\\u00a0", " ")
        .replace("4 ou 5 vol. [2], XL p. (Superius)", "2, 40")
        .replace("5 vol. LXII, [2] p.", "52, 2")
        .replace("[…] LXI, [3], p.", "61, 3")
        .replace(
          "[8] 343 [1] : 431, [1] : 348 : 272 : [8], 340, [36] : [56] f.",
          "8, 343, 1 : 431, 1 : 348 : 272 : 8, 340, 36 : 56 f."
        )
        .replace(
          "[8], 362 : 81, [1] : 108, [22] : 32  [18] f.",
          "8, 362 : 81, 1 : 108, 22 : 32, 18 f."
        )
        .replace(
          "[8] 862 [= 855], [1] : 194 [= 201], [1] : [10], 299 [= 270], [52] f.",
          "8, 862, 1 : 194, 1 : 10, 299, 52 f."
        )
        .replace("[16] [111] « 11 » : 200 p.", "16, 111 : 200")
        .replace("45 [3] f.", "45, 3 f.")
        .replace("112 [+16] p.", "112, 16")
    } map { _.trim } map { cleanCount =>
      val globalMultiplier = multiplier(cleanCount).filter(_ > 0)

      cleanCount.split(":").toList.map(_.trim) flatMap { splitByColon =>
        val partMultiplier = multiplier(splitByColon).filter(_ > 0)

        splitByColon.split(",").toList.map(_.trim) flatMap { splitByComma =>
          val localMultiplier = multiplier(splitByComma)
          val finalMultiplier =
            localMultiplier orElse partMultiplier orElse globalMultiplier getOrElse 1.0

          unitlessCount(splitByComma) map { count =>
            (count.toDouble * finalMultiplier + 0.5).toInt
          }
        }
      }
    } map { _.sum }
  }
  // scalastyle:on method.length

  def htmlToFormat(html: String): Option[Int] = {
    // Example of formats: "In-8°" (should return 8, i.e. 8 sheets per folio)
    Formats.findFirstMatchIn(html).map(_.group(1).trim.substring(3).replaceAll("°", "")) map {
      format =>
        val spaceIndex = format.indexOf(" ")
        if (spaceIndex < 0) format else format.substring(0, spaceIndex)
    } map { _.toInt }
  }

  def htmlToSignatures(html: String): Option[String] = {
    // Example of signatures: "*8 a4 a-z8 A-Z8 Aa-Oo8 Pp4 : A-C8"
    Signatures.findFirstMatchIn(html).map(_.group(1).trim)
  }

  def explodePrinters(printers: String, aliases: Map[String, String]): List[String] = {
    // Fix typos, remove places, remove brackets/question marks, etc.
    val cleanedPrinters =
      printers
        // Replace no-break space with regular space
        .replaceAll("\\u00a0", " ")
        .trim
        // Remove square brackets around names
        .replaceAll("[\\[\\]]", "")
        // Remove mentions of "Genve" and "Lyon"
        .replaceAll("^\\[?(?:Ge|Ly)(?:.\\?)?\\]?(?:.\\?)?, ", "")
        .replace("A. & J. Rivery & Jean Rivery", "A. Rivery & Jean Rivery")
        .replace("A. & J. s.n.", "A. Rivery & Jean Rivery")
        .replace("Jean Rivery.", "Jean Rivery")
        .replace("s.n..", "s.n.")
        .replace("« Va »", "s.n.")
        .replace(".II", ". II")
        .replaceAll(" +", " ")
        .replaceAll("\\u00a0", " ")
        .replace(", typographe d’Ulrich Fugger", "")
        .replace(", imprimeur parisien", "")
        .replace(", l’an", "")
        .replace(", d’ Arras", "")
        .replace(", dit Vigean", "")
        .replace(" pour ", ", ")

    cleanedPrinters.split(",|&").toList.map(_.trim).filter(_.size > 0) map { printer =>
      val cleanedPrinter =
        printer
          // Remove "?" at end of printer
          .replaceAll("^(.+) \\?$", "$1")
          // Remove "I" in names
          .replaceAll("^(.+) I (.+)$", "$1 $2")
      applyAliases(cleanedPrinter, aliases)
    }
  }
}

case class Book(
    number: Int,
    shortTitle: String,
    title: String,
    printers: List[String],
    year: String,
    url: String,
    pageCount: Option[Int],
    format: Option[Int]
)

// scalastyle:off equals.hash.code
case class PrintersGroup(printers: List[String]) {
  override def hashCode: Int = printers.sortWith(_ < _).foldLeft("")(_ + " " + _).hashCode
  def length: Int = printers.map(_.size).sum
}
// scalastyle:on equals.hash.code
