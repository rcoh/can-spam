import java.io.{File, FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream}
import scala.io.Source
import scala.collection.mutable
import scala.collection.parallel

type FreqCounter = Map[String, Int]

object SpamAnalyzer {
  def countMap(source: String): FreqCounter = {
    source.toLowerCase.split("""[^a-z0-9$-']""").groupBy(x => x).mapValues(x => x.size)
  }

  def dictMerge(tab1: FreqCounter, tab2: FreqCounter): FreqCounter = {
    val result = mutable.Map(tab1.toSeq: _*).withDefaultValue(0)
    tab2.map {
      case (str, count) => {
        result(str) += count
      }
    }
    result.toMap
  }

  def directoryToFileContents(dirName: String): parallel.mutable.ParArray[String] = {
    val dir = new File(dirName)
    dir.listFiles.
      filter(_.isFile).par.
      map(file => Source.fromFile(file,"latin1").mkString)
    }

  def createCorpusFromDir(dirName: String): FreqCounter = {
    val files = directoryToFileContents(dirName)
    files.map(countMap(_)).reduce((x,y) => dictMerge(x,y)).withDefaultValue(0)
  }

  def createSpamProbabilityDict(hamDir: String, spamDir: String): Map[String, Double] = {
    val spam = SpamAnalyzer.createCorpusFromDir(spamDir)
    val ham = SpamAnalyzer.createCorpusFromDir(hamDir)

    val allTokens = spam.keySet ++ ham.keySet
    allTokens.map(
      token => (token, spam(token), 2*ham(token))
    ).collect {
      case (token, spamOcc, hamOcc) if spamOcc + hamOcc > 5  => {
        (token, spamOcc.toDouble / (hamOcc + spamOcc))
      }
    }.toMap
  }

  def classify(email: String, probDict: Map[String, Double]): Boolean = {
    val occurenceMap = countMap(email)
    val tokensOfInterest = occurenceMap.keySet.map { 
    token => 
      (token, probDict.getOrElse(token, .4))
    }.toList.sortBy {
      case (token, score) => math.abs(score - .5) 
    }.takeRight(15)


    val tokenProbabilities = tokensOfInterest.map(_._2)
    
    val prob = tokenProbabilities.product.toDouble / 
    (tokenProbabilities.product + tokenProbabilities.map(1-_).product) 
    prob > .9
  }

  def serialize(probDict: Map[String, Double]) {
    val outputFile = new File("db")
    val buffer = new FileOutputStream(outputFile)
    val output = new ObjectOutputStream(buffer)
    output writeObject probDict
    output.close()
  }

  def deserialize: Map[String, Double] = {
    val inputFile = new File("db")
    val buffer = new FileInputStream(inputFile)
    val input = new ObjectInputStream(buffer)
    input.readObject.asInstanceOf[Map[String, Double]]
  }

  def evaluatePerformance(hamDir: String, spamDir: String, probDict: Map[String, Double]) = {
    val hamFiles = SpamAnalyzer.directoryToFileContents(hamDir) 
    val spamFiles = SpamAnalyzer.directoryToFileContents(spamDir) 
    val hamResults = hamFiles.map(file => SpamAnalyzer.classify(file, probDict))
    val numErrors = hamResults.count(_ == true)
    println("Ham Results: Scanned %d files. Classsified %d as spam. Percent Error: %g".
      format(hamFiles.size, numErrors, numErrors.toDouble / hamFiles.size))

    val spamResults = spamFiles.map(file => SpamAnalyzer.classify(file, probDict))
    val numSpamErrors = spamResults.count(_ == false)
    println("Spam Results: Scanned %d files. Classsified %d as ham. Percent Error: %g".
      format(spamFiles.size, numSpamErrors, numSpamErrors.toDouble / spamFiles.size))
  }

}

val resultDict = SpamAnalyzer.createSpamProbabilityDict("hard_ham", "soft_spam")

SpamAnalyzer.evaluatePerformance("easy_ham", "more_spam", resultDict)
