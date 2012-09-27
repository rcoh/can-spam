import java.io.{File, FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream}
import scala.io.Source
import scala.collection.mutable

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

  def createCorpusFromDir(dirName: String): FreqCounter = {
    val dir = new File(dirName)
    val files = dir.listFiles.
      filter(_.isFile).par.
      map(file => Source.fromFile(file,"latin1").mkString)

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

    println(tokensOfInterest)

    val tokenProbabilities = tokensOfInterest.map(_._2)
    
    val prob = tokenProbabilities.product.toDouble / 
    (tokenProbabilities.product + tokenProbabilities.map(1-_).product) 
    println(prob)
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
}

val resultDict = SpamAnalyzer.createSpamProbabilityDict("hard_ham", "soft_spam")
val spammy = """
From sabrina@mx3.1premio.com  Thu Aug 22 14:44:07 2002
Return-Path: <sabrina@mx3.1premio.com>
Delivered-To: zzzz@localhost.example.com
Received: from localhost (localhost [127.0.0.1])
	by phobos.labs.example.com (Postfix) with ESMTP id 1E90847C66
	for <zzzz@localhost>; Thu, 22 Aug 2002 09:44:02 -0400 (EDT)
Received: from mail.webnote.net [193.120.211.219]
	by localhost with POP3 (fetchmail-5.9.0)
	for zzzz@localhost (single-drop); Thu, 22 Aug 2002 14:44:03 +0100 (IST)
Received: from email.qves.com (email1.qves.net [209.63.151.251] (may be forged))
	by webnote.net (8.9.3/8.9.3) with ESMTP id OAA04953
	for <zzzz@example.com>; Thu, 22 Aug 2002 14:37:23 +0100
Received: from qvp0086 ([169.254.6.17]) by email.qves.com with Microsoft SMTPSVC(5.0.2195.2966);
	 Thu, 22 Aug 2002 07:36:20 -0600
From: "Slim Down" <sabrina@mx3.1premio.com>
To: <zzzz@example.com>
Subject: Guaranteed to lose 10-12 lbs in 30 days                          11.150
Date: Thu, 22 Aug 2002 07:36:19 -0600
Message-ID: <9a63c01c249e0$e5a9d610$1106fea9@freeyankeedom.com>
MIME-Version: 1.0
Content-Type: text/plain;
	charset="iso-8859-1"
Content-Transfer-Encoding: 7bit
X-Mailer: Microsoft CDO for Windows 2000
Thread-Index: AcJJ4OWpowGq7rdNSwCz5HE3x9ZZDQ==
Content-Class: urn:content-classes:message
X-MimeOLE: Produced By Microsoft MimeOLE V6.00.2462.0000
X-OriginalArrivalTime: 22 Aug 2002 13:36:20.0969 (UTC) FILETIME=[E692FD90:01C249E0]

1) Fight The Risk of Cancer!
http://www.adclick.ws/p.cfm?o=315&s=pk007

2) Slim Down - Guaranteed to lose 10-12 lbs in 30 days
http://www.adclick.ws/p.cfm?o=249&s=pk007

3) Get the Child Support You Deserve - Free Legal Advice
http://www.adclick.ws/p.cfm?o=245&s=pk002

4) Join the Web's Fastest Growing Singles Community
http://www.adclick.ws/p.cfm?o=259&s=pk007

5) Start Your Private Photo Album Online!
http://www.adclick.ws/p.cfm?o=283&s=pk007

Have a Wonderful Day,
Offer Manager
PrizeMama













If you wish to leave this list please use the link below.
http://www.qves.com/trim/?zzzz@example.com%7C17%7C308417
"""

println(SpamAnalyzer.classify(spammy, resultDict))
