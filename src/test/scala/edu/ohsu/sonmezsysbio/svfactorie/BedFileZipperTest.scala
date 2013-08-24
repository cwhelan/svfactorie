package edu.ohsu.sonmezsysbio.svfactorie

import org.scalatest.{FunSuite, FunSpec}

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 8/23/13
 * Time: 3:54 PM
 */
class BedFileZipperTest extends FunSuite {
  test("simple zip test") {
    val bins = "1\t0\t24\n1\t25\t49\n1\t50\t74\n1\t75\t99\n10\t0\t24\n10\t25\t49"
    val f1 = "1\t25\t49\t5\n1\t50\t74\n10\t25\t49\t6"
    val lines = BedFileZipper.zipLines(bins.split("\n").iterator, Array(new BedFileZipper.FeatureFile(f1.split("\n").iterator))).toArray
    assert(lines.size == 6)
    assert(lines(1).split("\t")(3) == "5")
    assert(lines(2).split("\t")(3) == "1")
    assert(lines(5).split("\t")(3) == "6")
  }

}
