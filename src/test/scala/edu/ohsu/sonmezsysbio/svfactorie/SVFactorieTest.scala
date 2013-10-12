package edu.ohsu.sonmezsysbio.svfactorie

import org.scalatest.FunSuite
import edu.ohsu.sonmezsysbio.svfactorie.SVFactorie.BinDomain
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 10/12/13
 * Time: 1:51 PM
 */
class SVFactorieTest extends FunSuite {
  test("train without validation") {
    // def trainAndValidateModel(trainingDataDir: String, validationSplitOption: Option[Double], featureDescriptors: FeatureDescriptors, model: SVModel, validationOutputDir: Option[String]) {
    val dir = "target/scala-2.10/test-classes/"
    val featureDescriptors = FeatureDescriptor.loadFeatureDescriptors(dir + "fd_coverage", BinDomain)
    SVFactorie.trainAndValidateModel(dir + "windows/", Some(1.0), featureDescriptors, new SVModel(featureDescriptors), None)
  }

  test("train with validation") {
    // def trainAndValidateModel(trainingDataDir: String, validationSplitOption: Option[Double], featureDescriptors: FeatureDescriptors, model: SVModel, validationOutputDir: Option[String]) {
    val dir = "target/scala-2.10/test-classes/"
    BinDomain.unfreeze()
    BinDomain.clear()
    val featureDescriptors = FeatureDescriptor.loadFeatureDescriptors(dir + "fd_coverage", BinDomain)
    SVFactorie.trainAndValidateModel(dir + "windows/", Some(0.5), featureDescriptors, new SVModel(featureDescriptors), Some(dir + "validation"))
  }
}
