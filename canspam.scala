import java.io.File
import scala.io.Source
import scala.collection.mutable

type FreqCounter = Map[String, Int]

object SpamAnalyzer {
  def countMap(source: String): FreqCounter = {
    source.split("""\s+""").groupBy(x => x).mapValues(x => x.size)
  }

  def dictMerge(tab1: FreqCounter, tab2: FreqCounter): FreqCounter = {
    val result = mutable.Map(tab1.toSeq: _*)
    tab2.map {
      case (str, count) => {
        tab1.get(str) match {
          case Some(_) => {
            result(str) += count
          }
          case None => {
            result(str) = count
          }
        }
      }
    }
    result.toMap
  }

  def createCorpusFromDir(dirName: String): FreqCounter = {
    val dir = new File(dirName)
    val files = dir.listFiles.
      filter(_.isFile).par.
      map(file => Source.fromFile(file,"latin1").mkString)

    files.map(countMap(_)).reduce((x,y) => dictMerge(x,y))
  }

  def createSpamProbabilityDict(hamDir: String, spamDir: String): Map[String, Double] = {
    val spam = SpamAnalyzer.createCorpusFromDir(spamDir)
    val ham = SpamAnalyzer.createCorpusFromDir(hamDir)

    val allTokens = spam.keySet ++ ham.keySet
    allTokens.map(
      token => (token, spam.getOrElse(token, 0), 2*ham.getOrElse(token, 0))
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
      (token, math.abs(.5 - probDict.getOrElse(token, .4)))
    }.toList.sortBy(_._2).takeRight(15)

    val tokenProbabilities = tokensOfInterest.map(_._2)
    
    val prob = tokenProbabilities.product.toDouble / 
    (tokenProbabilities.product + tokenProbabilities.map(1-_).product) 
    println(prob)
    prob > .9
  }

}


val resultDict = SpamAnalyzer.createSpamProbabilityDict("hard_ham", "soft_spam")
val spammyEmail = """Olá, você ja esteve apaixonado e não foi correspondido?

Tem alguma amiga e deseja algo mais que apenas amizade com ela?

Ou simplesmente acha que sua companheira/namorada/esposa está admirando outros
homens e não você?

Se você respondeu sim a qualquer uma das perguntas leia com atenção:

Esta semana os maiores Artistas em sedução estarão na sua Cidade.

Aproveite esta oportunidade para conversar pessoalmente e descobrir os Segredos
que envolvem a ciência de gerar atração nas mulheres. Torne realidade aquela
paixão que está somente em seus sonhos ou simplesmente viva o que a vida tem de
melhor para lhe oferecer. Nosso método já foi utilizado e aprovado por quase
4mil homens. Tanto que já é sucesso na TV e nas revistas. Nada de apostilas,
videos ou cds. Aqui você irá aprender na prática.

Acesse www.descubracomoseduzir.com e veja que é mais fácil do que você imagina.

Obrigado!

"""
println(SpamAnalyzer.classify(spammyEmail, resultDict))

  
    
