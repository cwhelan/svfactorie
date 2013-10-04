package edu.ohsu.sonmezsysbio.svfactorie

import cc.factorie._
import java.io.{PrintWriter, File}
import cc.factorie.util.BinarySerializer
import org.junit.Assert._

import SVFactorie._
/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 10/4/13
 * Time: 1:51 PM
 */
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

  def serialize(labelDomain : CategoricalDomain[String], featuresDomain : CategoricalDomain[String], prefix: String) {
    val modelFile = new File(prefix + "-model")
    if (modelFile.getParentFile ne null)
      modelFile.getParentFile.mkdirs()
    println("serializing, model.transitionTemplate.weightsSet.length: " + transitionTemplate.weights.value.length)
    println("serializing, model.localTemplate.weightsSet.length: " + localTemplate.weights.value.length)
    BinarySerializer.serialize(this, modelFile)
    val labelDomainFile = new File(prefix + "-labelDomain")
    BinarySerializer.serialize(labelDomain.dimensionDomain, labelDomainFile)
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    BinarySerializer.serialize(featuresDomain.dimensionDomain, featuresDomainFile)
    println("saved features domain, number of features = " + featuresDomain.dimensionDomain.size)
    val textWeightsFile = new File(prefix + "-weights.txt")
    print(new PrintWriter(textWeightsFile))
  }

  def deserialize(labelDomain : CategoricalDomain[String], featuresDomain : CategoricalDomain[String], prefix: String) {
    val featuresDomainFile = new File(prefix + "-featuresDomain")
    assert(featuresDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-featuresDomain'")
    BinarySerializer.deserialize(featuresDomain.dimensionDomain, featuresDomainFile)
    println("Loaded features domain, number of features = " + featuresDomain.dimensionDomain.size)

    val labelDomainFile = new File(prefix + "-labelDomain")
    assert(labelDomainFile.exists(), "Trying to load inexistent label domain file: '" + prefix + "-labelDomain'")
    BinarySerializer.deserialize(labelDomain.dimensionDomain, labelDomainFile)

    val modelFile = new File(prefix + "-model")
    assert(modelFile.exists(), "Trying to load inexisting model file: '" + prefix + "-model'")
    assertEquals(transitionTemplate.weights.value.length, labelDomain.length * labelDomain.length)
    BinarySerializer.deserialize(this, modelFile)
    println("deserializing, model.transitionTemplate.weightsSet.length: " + transitionTemplate.weights.value.length)
    println("deserializing, model.localTemplate.weightsSet.length: " + localTemplate.weights.value.length)

    println("Is the feature domain frozen: " +  featuresDomain.dimensionDomain.frozen)
    featuresDomain.dimensionDomain.freeze()
    println("Is the feature domain frozen: " +  featuresDomain.dimensionDomain.frozen)
  }

}
