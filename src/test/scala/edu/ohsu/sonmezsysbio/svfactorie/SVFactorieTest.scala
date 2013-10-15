package edu.ohsu.sonmezsysbio.svfactorie

import org.scalatest.FunSuite
import SVFactorie._

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

  test("find deletion variant") {

    val labels = List(
      new Label("0000", "1:100-124"), new Label("0100", "1:125-149"), new Label("1100", "1:150-174"), new Label("1000", "1:175-199"),
      new Label("1000", "1:200-224"), new Label("1100", "1:225-249"), new Label("0100", "1:250-274"), new Label("0000", "1:275-299")
    )
    assert("1\t150\t249\t4.0\t4.0" === SVFactorie.findDeletions(labels, new MarginalProportionHolder(None))(0))
  }
}
