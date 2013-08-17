package edu.ohsu.sonmezsysbio.svfactorie

import cc.factorie._
import cc.factorie.app.nlp.Token
import cc.factorie.app.chain.ChainModel
import java.io.File
import cc.factorie.optimize.Trainer
import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 7/22/13
 * Time: 11:40 AM
 */
object SVFactorie {
  // The variable classes
  object BinDomain extends CategoricalTensorDomain[String]
  class Bin(val loc:String, val label:Label) extends BinaryFeatureVectorVariable[String] {
    def domain = BinDomain
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(labelName: String, loc: String) extends LabeledCategoricalVariable(labelName) with ChainLink[Label,Window] {
    val bin = new Bin(loc, this)
    def domain = LabelDomain
  }
  class Window extends Chain[Window,Label]

  // The model
  val excludeSkipEdges = true
  val model = new TemplateModel with Parameters {
    addTemplates(
      // Bias term on each individual label
      new DotTemplateWithStatistics1[Label] {
        //def statisticsDomains = Tuple1(LabelDomain)
        val weights = Weights(new la.DenseTensor1(LabelDomain.size))
      },
      // Transition factors between two successive labels
      new DotTemplateWithStatistics2[Label, Label] {
        //def statisticsDomains = ((LabelDomain, LabelDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, LabelDomain.size))
        def unroll1(label: Label) = if (label.hasNext) Factor(label, label.next) else Nil
        def unroll2(label: Label) = if (label.hasPrev) Factor(label.prev, label) else Nil
      },
      // Factor between label and observed token
      new DotTemplateWithStatistics2[Label, Bin] {
        //def statisticsDomains = ((LabelDomain, TokenDomain))
        val weights = Weights(new la.DenseTensor2(LabelDomain.size, BinDomain.dimensionSize))
        def unroll1(label: Label) = Factor(label, label.bin)
        def unroll2(bin: Bin) = Factor(bin.label, bin)
      }
      //        ,
      //        // what does this template do?
      //        new DotTemplate2[Label,Label] /*DotStatistics1[BooleanValue]*/ {
      //          //def statisticsDomains = Tuple1(BooleanDomain)
      //          val weights = Weights(new la.DenseTensor1(BooleanDomain.size))
      //          def unroll1(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainAfter; if (other.bin.loc == label.bin.loc)) yield Factor(label, other)
      //          def unroll2(label: Label) = if (excludeSkipEdges) Nil else for (other <- label.chainBefore; if (other.bin.loc == label.bin.loc)) yield Factor(other, label)
      //          override def statistics(v1:Label#Value, v2:Label#Value) = BooleanValue(v1.intValue == v2.intValue)
      //        }
    )
  }

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
      if (count == sofar.size) sofar else
        rand (count, lower, upper, sofar + (random.nextInt (upper-lower) + lower))
  }

  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)

    val windowDir = args(0)
    val files = new File(windowDir).listFiles.map(windowDir + _.getName)

    val numTrainingFiles: Int = 700
    val trainingIndices = RandSet.rand(numTrainingFiles, 0, files.size - 1)
    val trainingFiles = (0 to (files.length - 1)).filter(trainingIndices.contains(_)).map(files(_))
    val testFiles = (0 to (files.length - 1)).filter(! trainingIndices.contains(_)).map(files(_))

    val trainingWindows = trainingFiles.map(load)
    val testWindows = testFiles.map(load)

    val allBins: Seq[Bin] = (trainingWindows ++ testWindows).flatten.map(_.bin)

    // Add interaction features
    allBins.foreach(bin => {
      val l = bin.activeCategories
      bin ++= l.map(_ => l).flatten.combinations(2).toList.filter(f => f(0) != f(1)).map(f => f(0) + "*" + f(1))
    })

    // Add features from next and previous tokens
    // println("Adding offset features...")
    allBins.foreach(bin => {
      if (bin.label.hasPrev) bin ++= bin.label.prev.bin.activeCategories.filter(!_.contains('@')).map(_+"@-1")
      if (bin.label.hasNext) bin ++= bin.label.next.bin.activeCategories.filter(!_.contains('@')).map(_+"@+1")
    })


    val summary = InferByBPChainSum.infer(trainingWindows(0), model)
    // assertStringEquals(summary.logZ, "6.931471805599453")
    //assertStringEquals(summary.marginal(document.tokens.head.attr[Label]).proportions, "Proportions(0.5,0.5)")

    val examples = trainingWindows.map(new optimize.LikelihoodExample(_, model, InferByBPChainSum))

    val optimizer1 = new optimize.LBFGS with optimize.L2Regularization
    optimizer1.variance = 10000.0
    Trainer.batchTrain(model.parameters, examples, optimizer=optimizer1)

    val objective = HammingObjective
    println("*** Starting inference (#sentences=%d)".format(testWindows.map(_.size).sum))
    testWindows.foreach {
      w => cc.factorie.BP.inferChainMax(w.asSeq, model)
    }
    val accuracy: Double = objective.accuracy(testWindows.flatMap(_.asSeq))
    println("test token accuracy=" + accuracy)

    evaluationString(testWindows)

  }

  def realToCategoricalFeatures(feature:Double, name:String, nullBin: Double, bins: Array[Double]) : Seq[String] = {
    if (feature <= nullBin) {
      List(name + "-N")
    } else {
      bins.filter(_ < feature).map(name + ">" + _)
    }
  }

  def load(filename:String) : Window = {
    import scala.io.Source

    val window = new Window
    val source = Source.fromFile(new File(filename))
    for (line <- source.getLines()) {
      val fields = line.split("\t")
      val loc = fields(0) + ":" + fields(1) + "-" + fields(2)

      val numLabelFields = 6

      val delHap1: Boolean = fields(fields.length - numLabelFields) == "1"
      val delHap2: Boolean = fields(fields.length - numLabelFields + 1) == "1"
      val delPres = if (delHap1 || delHap2) "1" else "0"
      val delFlank : String = if (delPres.equals("1")) "0" else fields(fields.length - numLabelFields + 2)

      val insHap1: Boolean = fields(fields.length - numLabelFields + 3) == "1"
      val insHap2: Boolean = fields(fields.length - numLabelFields + 4)  == "1"
      val insPres = if (insHap1 || insHap2) "1" else "0"
      val insFlank = if (insPres.equals("1")) "0" else fields(fields.length - numLabelFields + 5)

      // use full label
      // val labelStr = fields.slice(fields.length - numLabelFields, fields.length).mkString("")
      // last tested accuracy - 80%

      // use del/ins/flank (no zygosity) 72% -- up to 78% with interaction
      //val labelStr = delPres + delFlank + insPres + insFlank

      // use just del/ins (no zygosity) accuracy 88% - 85%
      val labelStr = delPres + insPres

      val label = new Label(labelStr, loc)

      val features = fields.slice(3, fields.length - numLabelFields).map(_.toDouble)

      realToCategoricalFeatures(features(0), "w0", -0.01, Array(.25, .5, 1)).map(label.bin +=)
      realToCategoricalFeatures(features(1), "mu1", 240, Array(255.0, 300.0, 345.0)).map(label.bin +=)
      realToCategoricalFeatures(features(2), "lr", 0, Array(.25, .5, 1, 2, 5, 100, 1000)).map(label.bin +=)
      realToCategoricalFeatures(features(4), "cov", 3, Array(10.0, 20, 40, 100)).map(label.bin +=)
      realToCategoricalFeatures(features(5), "ccov", 3, Array(10.0, 20, 40, 100)).map(label.bin +=)



      window += label
    }
    window
  }

}
