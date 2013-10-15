package edu.ohsu.sonmezsysbio.svfactorie

import cc.factorie._
import java.io._
import cc.factorie.optimize.Trainer
import scala.util.Random
import org.rogach.scallop._

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 7/22/13
 * Time: 11:40 AM
 */
object SVFactorie {

  implicit val random = new scala.util.Random()

  // The variable classes
  object BinDomain extends CategoricalDomain[String] {
    override def dimensionName(idx:Int):String = {
      BinDomain.dimensionDomain(idx).category.toString
    }
  }
  class Bin(val loc:String, val label:Label) extends BinaryFeatureVectorVariable[String] {
    def domain = BinDomain
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelName: String, loc: String) extends LabeledCategoricalVariable(labelName) with ChainLink[Label,Window] {
    val bin = new Bin(loc, this)
    def domain = LabelDomain
  }
  class Window extends Chain[Window,Label]

  val objective = new HammingTemplate[Label]

  def evaluationString(windows: Iterable[Window]): Unit = {
    //println("Train Token accuracy = "+ NerObjective.aveScore(trainLabels))
    //println(" Test Token accuracy = "+ NerObjective.aveScore(testLabels))
    val buf = new StringBuffer
    // Per-token evaluation
    buf.append(new LabeledDiscreteEvaluation(windows.flatMap(_.asSeq)))
    println(buf)
//    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[Label](LabelDomain.categories.filter(_.length > 2).map(_.substring(2)))
//    for (doc <- window; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[BioConllNerLabel])
//    println("Segment evaluation")
//    println(segmentEvaluation)
  }



  object RandSet {
    val random = Random
    def rand (count: Int, lower: Int, upper: Int, sofar: Set[Int] = Set.empty): Set[Int] =
      if (upper == 0) sofar + 0 else
        if (count == sofar.size) sofar else
          rand (count, lower, upper, sofar + (random.nextInt (upper-lower) + lower))
  }

  def trainAndValidateModel(trainingDataDir: String, validationSplitOption: Option[Double], featureDescriptors: FeatureDescriptors, model: SVModel, validationOutputDir: Option[String]) {
    val directory: File = new File(trainingDataDir)
    val trainingDataFiles = directory.listFiles.map(trainingDataDir + _.getName)

    val validationSplit: Double = validationSplitOption match {
      case None => 1.0
      case Some(x: Double) => x
    }
    val numTrainingFiles: Int = (trainingDataFiles.length * validationSplit).round.toInt
    println("Total files: " + trainingDataFiles.length)
    println("Train/Validate: " + numTrainingFiles + "/" + (trainingDataFiles.length - numTrainingFiles))

    // todo: rewrite this
    var trainingFiles : IndexedSeq[String] =  Array[String]()
    var validationFiles : IndexedSeq[String] =  Array[String]()
    println("filtering files")
    if (validationSplit < 1.0) {
      val trainingIndices = RandSet.rand(numTrainingFiles, 0, trainingDataFiles.size - 1)
      println("filtering training files")
      trainingFiles = (0 to (trainingDataFiles.length - 1)).filter(trainingIndices.contains).map(trainingDataFiles(_))
      println("filtering validation files")
      validationFiles = (0 to (trainingDataFiles.length - 1)).filter(!trainingIndices.contains(_)).map(trainingDataFiles(_))
    } else {
      trainingFiles = trainingDataFiles
      validationFiles = Array[String]()
    }

    println("Loading training files")
    val trainingWindows = trainingFiles.map(load(_, featureDescriptors))
    println("Loading validation files")
    val validationWindows = validationFiles.map(load(_, featureDescriptors))
    println("bin domain length: " + BinDomain.dimensionDomain.length)

    val allBins: Seq[Bin] = (trainingWindows ++ validationWindows).flatten.map(_.bin)
    FeatureDescriptor.initRelativeFeatures(allBins)
    trainingWindows.flatten.foreach(_.setRandomly)
    validationWindows.flatten.foreach(_.setRandomly)

    println("loaded up features, model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("loaded up features, model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)

    println("bin domain length: " + BinDomain.dimensionDomain.length)

    // val summary = InferByBPChainSum.infer(trainingWindows(0), model)
    // assertStringEquals(summary.logZ, "6.931471805599453")
    //assertStringEquals(summary.marginal(document.tokens.head.attr[Label]).proportions, "Proportions(0.5,0.5)")

    // begin batch training code
    val examples = trainingWindows.map(new optimize.LikelihoodExample(_, model, InferByBPChainSum))

//    LBGFS
    val optimizer = new optimize.LBFGS with optimize.L2Regularization
    optimizer.variance = 10000.0


    Trainer.batchTrain(model.parameters, examples, optimizer = optimizer)

    // begin online trainer code
//    val sampler = new GibbsSampler(model, HammingObjective)
//
//    val trainLabels : Seq[Label] = trainingWindows.flatten.map(_.bin).map(_.label)
//    val sampleRankExamples = trainLabels.map(t => new optimize.SampleRankExample(t, sampler))
//
//    Trainer.onlineTrain(model.parameters, sampleRankExamples)

    // AdaGrad
//        val lr=1.0
//        val l1Factor = 0.02
//        val l2Factor = 0.000001
//        val optimizer = new optimize.AdaGradRDA(rate=lr, l1=l1Factor/examples.length, l2=l2Factor/examples.length)
//
//
//    Trainer.onlineTrain(model.parameters, examples, maxIterations=25, optimizer=optimizer, useParallelTrainer = false)

    if (validationFiles.length > 0) {
      val outFilePrintWriters =  Seq("validation_set_deletions.bed", "validation_set_insertions.bed").map(s => new java.io.PrintWriter(new BufferedWriter(new FileWriter(new File(validationOutputDir.get + s), true))))
      predict(validationWindows, model, validationOutputDir, outFilePrintWriters)
      evaluatePredictions(validationWindows)
      outFilePrintWriters.foreach(_.close())
    }
  }

  class MarginalProportionHolder(summary : Option[BPSummary]) {
    def getProportion(label : Label) : Seq[Double] = {
      summary match {
        case None => Array.fill[Double](label.domain.length)(1.0).toSeq
        case Some(_) => summary.get.marginal(label).proportions.asSeq
      }
    }
  }

  def findDeletions(windows: Seq[Label], summary : MarginalProportionHolder): Seq[String] = {
    findVariants(windows, summary, 0)
  }

  def findInsertions(windows: Seq[Label], summary : MarginalProportionHolder): Seq[String] = {
    findVariants(windows, summary, 2)
  }

  def findVariants(window: Seq[Label], summary : MarginalProportionHolder, variantColumnInLabel : Int): Seq[String] = {
    if (window.isEmpty) {
      List[String]()
    } else {
      window.head.categoryValue.charAt(variantColumnInLabel) match {
        case '0' => findVariants(window.tail, summary, variantColumnInLabel)
        case '1' => {
          val startLoc = window(0).bin.loc
          val delBins = window.takeWhile(_.categoryValue.charAt(variantColumnInLabel) == '1')
          val endLoc = delBins.last.bin.loc
          val m2 = delBins.map({
            label => LabelDomain.categories.zip(summary.getProportion(label)).filter(_._1.charAt(0) == '1').map(_._2).sum
          })
          val maxProportion = m2.max
          val avgProportion = m2.sum / m2.size
          List(
            List(startLoc.split(":")(0), startLoc.split(":")(1).split("-")(0), endLoc.split(":")(1).split("-")(1), maxProportion, avgProportion).mkString("\t")
          ) ++ findVariants(window.dropWhile(_.categoryValue.charAt(variantColumnInLabel) == '1'), summary, variantColumnInLabel)
        }
      }
    }
  }

  def predict(windows: IndexedSeq[SVFactorie.Window], model: SVModel, outputDir: Option[String], outFilePrintWriters : Seq[PrintWriter]) {
    println("*** Starting inference (#sentences=%d)".format(windows.map(_.size).sum))
    val summaries = windows.map {
      w => cc.factorie.BP.inferChainMax(w.asSeq, model)        
    }
    writeOutputWindows(outputDir, windows, summaries)

    windows.map(_.asSeq).zip(summaries).flatMap(p => findDeletions(p._1, new MarginalProportionHolder(Some(p._2)))).map(l => outFilePrintWriters(0).write(l + "\n"))
    // windows.map(_.asSeq).zip(summaries).flatMap(p => findInsertions(p._1, new MarginalProportionHolder(Some(p._2)))).map(l => outFilePrintWriters(1).write(l + "\n"))
  }


  def evaluatePredictions(validationWindows: IndexedSeq[SVFactorie.Window]) {
    val accuracy: Double = objective.accuracy(validationWindows.flatMap(_.asSeq))
    println("validation token accuracy=" + accuracy)

    evaluationString(validationWindows)

    val windowsWithTruePredictions = validationWindows.filter(w => w.asSeq.count(b => b.categoryValue.toInt != 0 && b.categoryValue == b.target.categoryValue) > 0)
    println("Number of windows with a non-zero accurate prediction: " +
      windowsWithTruePredictions.size + "/" + validationWindows.size)
    print(windowsWithTruePredictions.map(w => w(0).bin.loc).mkString("\n"))
  }

  def writeOutputWindows(outputDir: Option[String], windows: IndexedSeq[SVFactorie.Window], summaries: IndexedSeq[BPSummary]) {
    outputDir.foreach(outputDirName =>
      for ((w,summary) <- windows.zip(summaries)) {
        val pw = new java.io.PrintWriter(new File(outputDirName + w(0).bin.loc + ".bed"))
        try {
          for (label <- w) {
            pw.write(label.bin.loc + "\t" + label.target.categoryValue + "\t" + label.categoryValue + "\n")
          }
          pw.write("\nBEGIN MARGINALS\n")
          printTokenMarginals(w.asSeq, summary, pw)
        } finally {
          pw.close()
        }
      }
    )
  }

  def printTokenMarginals(labels:Seq[Label], summary:BPSummary, pw :PrintWriter): Unit = {
    //val summary = BP.inferChainSum(tokens.map(_.label), model)
    for (label <- labels)
      pw.write(label + " " + LabelDomain.categories.zip(summary.marginal(label).proportions.asSeq).sortBy(_._2).reverse.mkString(" ")+ "\n")
  }

  def load(filename:String, featureDescriptors : FeatureDescriptors) : Window = {
    import scala.io.Source

    val window = new Window
    // println("loading file: "+ filename)
    val source = Source.fromFile(new File(filename))
    for (line <- source.getLines()) {
      val fields = line.split("\t")
      val loc = fields(0) + ":" + fields(1) + "-" + fields(2)

      val numLabelFields = 6

      val delHap1: Boolean = fields(fields.length - numLabelFields) == "1"
      val delHap2: Boolean = fields(fields.length - numLabelFields + 1) == "1"
      val delPres = if (delHap1 || delHap2) "1" else "0"
      val delFlank = fields(fields.length - numLabelFields + 2)

      val insHap1: Boolean = fields(fields.length - numLabelFields + 3) == "1"
      val insHap2: Boolean = fields(fields.length - numLabelFields + 4)  == "1"
      val insPres = if (insHap1 || insHap2) "1" else "0"
      val insFlank = fields(fields.length - numLabelFields + 5)

      // use full label
      // val labelStr = fields.slice(fields.length - numLabelFields, fields.length).mkString("")
      // last tested accuracy - 80%

      // use del/ins/flank (no zygosity) 72% -- up to 78% with interaction
      // val labelStr = delPres + delFlank + insPres + insFlank

      // use just del/ins (no zygosity) accuracy 88% - 85%
      // val labelStr = delPres + insPres

      // use just deletion/del flank
      val labelStr = delPres + delFlank
      val label = new Label(labelStr, loc)

      val featureValues = fields.slice(3, fields.length - numLabelFields)

      featureDescriptors.features.map(_.toFeatures(featureValues)).flatten.map(label.bin +=)

      window += label
    }
    source.close()
    window
  }

  def testModel(model: SVModel, descriptors: FeatureDescriptors, testDataDir: String, testOutputDir: Option[String], outputFilePrefix: Option[String]) : Unit = {
    val directory: File = new File(testDataDir)
    val testDataFiles = directory.listFiles.map(testDataDir + _.getName)

    val numTestFiles: Int = testDataFiles.length
    println("Total test files: " + numTestFiles)

    val outFilePrintWriters = (outputFilePrefix match {
      case Some(_) => Seq(outputFilePrefix.get + "_deletions.bed", outputFilePrefix.get + "_insertions.bed")
      case None => Seq("deletions.bed", "insertions.bed")
    }).map(s => new java.io.PrintWriter(new BufferedWriter(new FileWriter(new File(testOutputDir.get + s), true))))

    println("Loading test files")
    testDataFiles.grouped(1000).foreach(l => testFiles(l, descriptors, model, testOutputDir, outFilePrintWriters))

    outFilePrintWriters.foreach(_.close())
  }


  def testFiles(testDataFiles: Array[String], descriptors: FeatureDescriptors, model: SVModel, testOutputDir: Option[String], outFilePrintWriters : Seq[PrintWriter]) {
    val startTime = compat.Platform.currentTime
    scala.Console.err.print("processing " + testDataFiles.length + " test files\n")
    val testWindows = testDataFiles.map(load(_, descriptors))

    val allBins: Seq[Bin] = testWindows.flatten.map(_.bin)
    FeatureDescriptor.initRelativeFeatures(allBins)
    testWindows.flatten.foreach(_.setRandomly)

    println("loaded up features, model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("loaded up features, model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)

    println("bin domain length: " + BinDomain.dimensionDomain.length)
    predict(testWindows, model, testOutputDir, outFilePrintWriters)
    scala.Console.err.print("Processed batch in " + (compat.Platform.currentTime - startTime) + "\n")
  }

  def main(args:Array[String]): Unit = {

    object Conf extends ScallopConf(args) {
      val featureDescriptorFile = opt[String]("featureDescriptorFile", required=true)
      val trainingDataDir = opt[String]("trainingDataDir")
      val validationSplit = opt[Double]("validationSplit", default=Some(0.8))
      val modelName = opt[String]("modelName")
      val validationOutputDir = opt[String]("validationOutputDir")
      val testDataDir = opt[String]("testDataDir")
      val testOutputDir = opt[String]("testOutputDir")
      val outputFilePrefix = opt[String]("outputFilePrefix")
    }

    val featureDescriptorFile = Conf.featureDescriptorFile.get

    // todo: non-idiomatic scala, change this
    val featureDescriptors = FeatureDescriptor.loadFeatureDescriptors(featureDescriptorFile.get, BinDomain)

    val model = new SVModel(featureDescriptors)


    Conf.trainingDataDir.get match {
      case Some(trainingDataDir : String) => {
        trainAndValidateModel(trainingDataDir, Conf.validationSplit.get, featureDescriptors, model, Conf.validationOutputDir.get)
        Conf.modelName.foreach(model.serialize(LabelDomain, BinDomain, _))
      }
      case None => {
        // load model from serialized file
        Conf.modelName.foreach(model.deserialize(LabelDomain, BinDomain, _))
      }
    }

    Conf.testDataDir.get.foreach(
      testModel(model, featureDescriptors, _, Conf.testOutputDir.get, Conf.outputFilePrefix.get)
    )

  }

}
