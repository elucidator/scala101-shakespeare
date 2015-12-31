package words

import Shakespeare._

import scala.collection.mutable


object Processing {

  implicit class NoLicenseIterator(it: Iterator[String]) {
    /**
      * Strips the initial and final license.
      * Use [[Shakespeare.endOfInitialLicense]] and [[Shakespeare.startOfFinalLicense]].
      */
    def stripLicenses: Iterator[String] = it.slice(Shakespeare.endOfInitialLicense, Shakespeare.startOfFinalLicense)
  }

  def toWords(line: String): List[String] = line.split("\\W").toList
}

object InMemory {

  import Processing._

  /** Takes a line iterator and returns a map of words and their count in the text.
    *
    * Process only lines starting from [[Shakespeare.endOfInitialLicense]] and until [[Shakespeare.startOfFinalLicense]].
    *
    * Use the function [[Processing.toWords]] to change a line into a list of words.
    * Use the function [[count]].
    */
  def wordCount(it: Iterator[String]): Map[String, Int] = {

    var result: Map[String, Int] = Map()

    it.foreach(line => {
        result = count(toWords(line)).foldLeft(result)((map, value) => {
        map.updated(value._1, map.getOrElse(value._1, 0) + value._2)
      })
    }
    )

    result
  }

  /** Takes a list of words and returns a map of words to their word count. */
  def count(words: List[String]): Map[String, Int] =
    words.groupBy(identity) map { case (word, listOfWords) => (word, listOfWords.size) }
}

object Lazy {

  import Processing._

  /** Takes a line iterator and returns a map of words and their count in the text.
    * The function operates lazily. That is; lines that aren't currently being processed are not loaded into memory yet.
    *
    * Process only lines starting from [[Shakespeare.endOfInitialLicense]] and until [[Shakespeare.startOfFinalLicense]].
    * Use the function [[Processing.toWords]] to change a line into a list of words.
    *
    * Use the function [[count]] to combine the incoming words into a single outcome.
    */
  def wordCount(it: Iterator[String]): Map[String, Int] = {
    var result: Map[String, Int] = Map()
    it.foreach(line => result = count(result, toWords(line)))
    result
  }

  /** Given the previous map from word to word count and the words from the current line,
    * returns the updated map from word to word count.
    */
  def count(acc: Map[String, Int], words: List[String]): Map[String, Int] = {
    words.foldLeft(acc)((map, value) => map.updated(value, map.getOrElse(value, 0) + 1))
  }
}

