import java.io.{File, FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream}
import scala.io.Source
import scala.collection.mutable
import scala.collection.parallel

object SpamCLI extends App {
  if(args.size == 0) {
    println("Usage: ")
    println("Create DB: scala canspam.scala crdb hamdir spamdir")
    println("Classify a message: scala canspam.scala classify message")
    println("Evaluate Performance: scala canspam.scala eval hamdir spamdir")
    System.exit(1)
  }
  if(args(0) == "crdb") {
    if(args.size != 3) {
      println("Not enough args")
      System.exit(1)
    }
    val results = SpamAnalyzer.createSpamProbabilityDict(args(1), args(2))
    SpamAnalyzer.serialize(results)
  }
  if(args(0) == "classify") {
    if(args.size != 2) {
      println("Not enough args")
      System.exit(1)
    }
    val dict = SpamAnalyzer.deserialize
    println(SpamAnalyzer.classify(Source.fromFile(args(1)) mkString, dict))
  }
  if(args(0) == "eval") {
    if(args.size != 3) {
      println("Not enough args")
      System.exit(1)
    }
    val dict = SpamAnalyzer.deserialize
    SpamAnalyzer.evaluatePerformance(args(1), args(2), dict)
  }
}

object SpamAnalyzer {
  type FreqCounter = Map[String, Int]
  def countMap(source: String): FreqCounter = {
    source.toLowerCase.split("""[^a-z0-9$-']""").groupBy(x => x).mapValues(x => x.size)
  }

  def dictMerge(tab1: FreqCounter, tab2: FreqCounter): FreqCounter = {
    tab1 ++ tab2.map{ case (k,v) => k -> (v + tab1.getOrElse(k,0)) }
  }

  def directoryToFileContents(dirName: String): parallel.mutable.ParArray[String] = {
    val dir = new File(dirName)
    dir.listFiles.
      filter(_.isFile).par.
      map(file => Source.fromFile(file,"latin1").mkString)
    }

  def createCorpusFromDir(dirName: String): FreqCounter = {
    val files = directoryToFileContents(dirName)
    println("have files")
    files.view.map(countMap(_)).reduce((x,y) => dictMerge(x,y)).withDefaultValue(0)
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
