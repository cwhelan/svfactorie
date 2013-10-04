package edu.ohsu.sonmezsysbio.svfactorie

import cc.factorie._
import java.io.{PrintWriter, File}
import cc.factorie.optimize.Trainer
import scala.util.Random
import cc.factorie.util.BinarySerializer
import org.junit.Assert._
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

  class SVModel(featureDescriptors : FeatureDescriptors) extends TemplateModel with Parameters {
      // Bias term on each individual label
      val biasTemplate = new DotTemplateWithStatistics1[Label] {
        //def statisticsDomains = Tuple1(LabelDomain)
        val weights = Weights(new la.DenseTensor1(LabelDomain.size))
      }
      // Transition factors between two successive labels
      val transitionTemplate = new DotTemplateWithStatistics2[Label, Label] {
        //def statisticsDomains = ((LabelDomain, LabelDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
        def unroll1(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
        def unroll2(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      }
      // Factor between label and observed token
      val localTemplate = new DotTemplateWithStatistics2[Label, Bin] {
        //def statisticsDomains = ((LabelDomain, TokenDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, BinDomain.dimensionSize))
        def unroll1(label: Label) = Factor(label, label.bin)
        def unroll2(bin: Bin) = Factor(bin.label, bin)
      }
    this += biasTemplate
    this += localTemplate
    this += transitionTemplate

    def print(writer : PrintWriter) {
      writer.println("FEATURE DESCRIPTORS")
      featureDescriptors.print(writer)

      writer.println("\n")
      writer.println("TRANSITION TEMPLATE")
      val namedWeightsTx =
        for ((val0, idx0) <- LabelDomain.dimensionDomain.zipWithIndex; (val1, idx1) <- LabelDomain.dimensionDomain.zipWithIndex) yield {
          val w = transitionTemplate.weights.value(idx0, idx1)
          ((LabelDomain.dimensionName(val0.intValue), LabelDomain.dimensionName(val1.intValue)), w)
        }
      for (((cat0, cat1), w) <- namedWeightsTx.sortBy(-_._2)) {
        writer.println(cat0 + "\t" + cat1 + "\t" + w)
      }

      writer.println("\n")
      writer.println("LOCAL TEMPLATE")
      val namedWeights =
        for ((val0, idx0) <- LabelDomain.dimensionDomain.zipWithIndex; (val1, idx1) <- BinDomain.dimensionDomain.zipWithIndex) yield {
          val w = localTemplate.weights.value(idx0, idx1)
          ((LabelDomain.dimensionName(val0.intValue), BinDomain.dimensionName(val1.intValue)), w)
        }
      for (((cat0, cat1), w) <- namedWeights.sortBy(-_._2)) {
        writer.println(cat0 + "\t" + cat1 + "\t" + w)
      }
    }
  }



  def serialize(model : SVModel, labelDomain : CategoricalDomain[String], featuresDomain : CategoricalDomain[String], prefix: String) {
    val modelFile = new File(prefix + "-model")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    println("serializing, model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("serializing, model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)
    BinarySerializer.serialize(model, modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinarySerializer.serialize(labelDomain.dimensionDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinarySerializer.serialize(featuresDomain.dimensionDomain, featuresDomainFile)
    println("saved features domain, number of features = " + featuresDomain.dimensionDomain.size)
    val textWeightsFile = new File(prefix + "-weights.txt")
    model.print(new PrintWriter(textWeightsFile))
  }

  def deserialize(model : SVModel, labelDomain : CategoricalDomain[String], featuresDomain : CategoricalDomain[String], prefix: String) {
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-featuresDomain'")
    BinarySerializer.deserialize(featuresDomain.dimensionDomain, featuresDomainFile)
    println("Loaded features domain, number of features = " + featuresDomain.dimensionDomain.size)

    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-labelDomain'")
    BinarySerializer.deserialize(labelDomain.dimensionDomain, labelDomainFile)

    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix + "-model'")
    assertEquals(model.transitionTemplate.weights.value.length, labelDomain.length * labelDomain.length)
    BinarySerializer.deserialize(model, modelFile)
    println("deserializing, model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("deserializing, model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)

    println("Is the feature domain frozen: " +  featuresDomain.dimensionDomain.frozen)
    featuresDomain.dimensionDomain.freeze()
    println("Is the feature domain frozen: " +  featuresDomain.dimensionDomain.frozen)
  }

  // The model
  val objective = new HammingTemplate[Label]


  def evaluationString(windows: Iterable[Window]): Unit = {
    //println("Train Token accuracy = "+ NerObjective.aveScore(trainLabels))
    //println(" Test Token accuracy = "+ NerObjective.aveScore(testLabels))
    val buf = new StringBuffer
    // Per-token evaluation
    buf.append(new LabeledDiscreteEvaluation(windows.flatMap(_.asSeq)))
    println(buf)
//    val segmentEvaluation = new cc.factorie.app.chain.SegmentEvaluation[Label](LabelDomain.categories.filter(_.length > 2).map(_.substring(2)))
//    for (doc <- windows; sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[BioConllNerLabel])
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

  def trainAndValidateModel(trainingDataDir: String, validationSplitOption: Option[Double], featureDescriptors: FeatureDescriptors, model: SVFactorie.SVModel, validationOutputDir: Option[String]) {
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

    // LBGFS
    val optimizer = new optimize.LBFGS with optimize.L2Regularization
    optimizer.variance = 10000.0
//
//
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


//    Trainer.onlineTrain(model.parameters, examples, maxIterations=10, optimizer=optimizer, useParallelTrainer = false)

    if (validationFiles.length > 0) {
      predict(validationWindows, model, validationOutputDir)
      evaluatePredictions(validationWindows)
    }
  }


  def predict(windowsWindows: IndexedSeq[SVFactorie.Window], model: SVFactorie.SVModel, outputDir: Option[String]) {
    println("*** Starting inference (#sentences=%d)".format(windowsWindows.map(_.size).sum))
    windowsWindows.foreach {
      w => cc.factorie.BP.inferChainMax(w.asSeq, model)
    }
    writeOutputWindows(outputDir, windowsWindows)
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

  def writeOutputWindows(outputDir: Option[String], windows: IndexedSeq[SVFactorie.Window]) {
    outputDir.foreach(outputDirName =>
      for (w <- windows) {
        val pw = new java.io.PrintWriter(new File(outputDirName + w(0).bin.loc + ".bed"))
        try {
          for (label <- w) {
            pw.write(label.bin.loc + "\t" + label.target.categoryValue + "\t" + label.categoryValue + "\n")
          }
        } finally {
          pw.close()
        }
      }
    )
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
      val labelStr = delPres + delFlank + insPres + insFlank

      // use just del/ins (no zygosity) accuracy 88% - 85%
      // val labelStr = delPres + insPres

      val label = new Label(labelStr, loc)

      val featureValues = fields.slice(3, fields.length - numLabelFields)

      featureDescriptors.features.map(_.toFeatures(featureValues)).flatten.map(label.bin +=)

      window += label
    }
    source.close()
    window
  }

  def testModel(model: SVModel, descriptors: FeatureDescriptors, testDataDir: String, testOutputDir: Option[String]) : Unit = {
    val directory: File = new File(testDataDir)
    val testDataFiles = directory.listFiles.map(testDataDir + _.getName)

    val numTestFiles: Int = testDataFiles.length
    println("Total test files: " + numTestFiles)

    println("Loading test files")
    val testWindows = testDataFiles.map(load(_, descriptors))

    val allBins: Seq[Bin] = testWindows.flatten.map(_.bin)
    FeatureDescriptor.initRelativeFeatures(allBins)
    testWindows.flatten.foreach(_.setRandomly)

    println("loaded up features, model.transitionTemplate.weightsSet.length: " + model.transitionTemplate.weights.value.length)
    println("loaded up features, model.localTemplate.weightsSet.length: " + model.localTemplate.weights.value.length)

    println("bin domain length: " + BinDomain.dimensionDomain.length)
    predict(testWindows, model, testOutputDir)

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
    }

    val featureDescriptorFile = Conf.featureDescriptorFile.get

    // todo: non-idiomatic scala, change this
    val featureDescriptors = FeatureDescriptor.loadFeatureDescriptors(featureDescriptorFile.get, BinDomain)

    val model = new SVModel(featureDescriptors)


    Conf.trainingDataDir.get match {
      case Some(trainingDataDir : String) => {
        trainAndValidateModel(trainingDataDir, Conf.validationSplit.get, featureDescriptors, model, Conf.validationOutputDir.get)
        Conf.modelName.foreach(serialize(model, LabelDomain, BinDomain, _))
      }
      case None => {
        // load model from serialized file
        Conf.modelName.foreach(deserialize(model, LabelDomain, BinDomain, _))
      }
    }

    Conf.testDataDir.get.foreach(
      testModel(model, featureDescriptors, _, Conf.testOutputDir.get)
    )

  }

}
