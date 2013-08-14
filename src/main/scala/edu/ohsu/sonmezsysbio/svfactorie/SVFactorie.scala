package edu.ohsu.sonmezsysbio.svfactorie

import cc.factorie._
import cc.factorie.app.nlp.Token
import cc.factorie.app.chain.ChainModel
import java.io.File

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

  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)


    val trainSentences = load(args(0)).take(1000)
    val testSentences = load(args(1)).take(200)

  }

  def realToCategoricalFeatures(feature:Double, name:String, nullBin: Double, bins: Array[Double]) : Seq[String] = {
    if (feature <= nullBin) {
      List(name + "-N")
    } else {
      bins.filter(_ < feature).map(name + "-" + _)
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
      val labelStr = fields.slice(fields.length - numLabelFields, fields.length).mkString("")
      val label = new Label(labelStr, loc)

      val features = fields.slice(3, fields.length - numLabelFields - 1).map(_.toDouble)

      realToCategoricalFeatures(features(0), "w0", -0.01, Array(.25, .5, 1)).map(label.bin +=)
      realToCategoricalFeatures(features(1), "lr", 0, Array(.25, .5, 1, 2, 5, 100, 1000)).map(label.bin +=)
      realToCategoricalFeatures(features(2), "mu1", 0, Array(300.0)).map(label.bin +=)
      realToCategoricalFeatures(features(4), "cov", 3, Array(10.0, 20, 40, 100)).map(label.bin +=)
      realToCategoricalFeatures(features(5), "ccov", 3, Array(10.0, 20, 40, 100)).map(label.bin +=)
      window += label
    }
    window
  }

}
