package edu.ohsu.sonmezsysbio.svfactorie

import java.io.{OutputStreamWriter, File, PrintWriter}
import cc.factorie.CategoricalDomain
import scala.io.Source
import edu.ohsu.sonmezsysbio.svfactorie.SVFactorie.{Bin, BinDomain}

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 10/4/13
 * Time: 1:31 PM
 */
trait FeatureDescriptor {
  def toFeatures(featureList : Array[String]) : Seq[String]
  def possibleFeatures() : Seq[String]
  def print(writer : PrintWriter) : Unit

  def realToCategoricalCumulativeBinnedFeatures(feature:Double, name:String, nullBin: Double, bins: Array[Double]) : Seq[String] = {
    if (feature <= nullBin) {
      List(name + FeatureDescriptor.FeatureNameSeparator+ "-N")
    } else {
      bins.filter(_ <= feature).map(name + FeatureDescriptor.FeatureNameSeparator+ ">" + _)
    }
  }

  def realToCategoricalBinnedFeatures(feature:Double, name:String, bins: Array[Double]) : Seq[String] = {
    if (feature <= bins(0)) {
      List(name + FeatureDescriptor.FeatureNameSeparator+ "0")
    } else if (feature > bins(bins.size - 1)) {
      List(name + FeatureDescriptor.FeatureNameSeparator+ bins.size)
    } else {
      (0 to bins.size - 2).filter(i => feature >= bins(i) && feature <= bins(i + 1)).map(_ + 1).map(name + FeatureDescriptor.FeatureNameSeparator+ _)
    }
  }

}

object FeatureDescriptor {
  val FeatureNameSeparator = "|"

  def featureNamesNotEqual(f1: String, f2: String): Boolean = {
    f1.substring(0, f1.indexOf(FeatureNameSeparator)) != f2.substring(0, f2.indexOf(FeatureNameSeparator))
  }

  def loadFeatureDescriptors(featureFile : String, featureDomain : CategoricalDomain[String]) = {
    val source = Source.fromFile(new File(featureFile))
    val lines =  source.getLines().filter(! _.startsWith("#")).map(_.split("\t"))
    val featureDescriptors = lines.filter(_(0) == "feature").map(
      f => f(1) match {
        case "boolean" => new BooleanFeatureDescriptor(f(2).toInt, f(3))
        case "cumulativeBinnedReal" => new CumulativeBinnedRealFeatureDescriptor(f(2).toInt, f(3), f(4).toDouble, f(5).split(",").map(_.toDouble))
        case "binnedReal" => new BinnedRealFeatureDescriptor(f(2).toInt, f(3), f(4).split(",").map(_.toDouble))
      }
    ).toArray
    featureDescriptors.foreach(f => f.possibleFeatures().foreach(featureDomain.dimensionDomain += featureDomain.stringToCategory(_)))
    featureDomain.dimensionDomain.filter(f => ! f.toString().contains("@")).map(_ + "@1")
      .foreach(featureDomain.dimensionDomain += featureDomain.stringToCategory(_))
    featureDomain.dimensionDomain.filter(f => ! f.toString().contains("@")).map(_ + "@<>")
      .foreach(featureDomain.dimensionDomain += featureDomain.stringToCategory(_))
    featureDomain.dimensionDomain.combinations(2).toList.filter(f => f(0) != f(1) && FeatureDescriptor.featureNamesNotEqual(f(0).toString(), f(1).toString()))
      .map(f => f(0) + "*" + f(1))
      .foreach(featureDomain.dimensionDomain += featureDomain.stringToCategory(_))
    featureDomain.freeze()
    val fd = new FeatureDescriptors(featureDescriptors)
    fd.print(new PrintWriter(new OutputStreamWriter(Console.err)))
    fd
  }

  def initRelativeFeatures(allBins: Seq[SVFactorie.Bin]) {
    // Add interaction features
    // Add features from next and previous tokens
    println("Adding offset features...")
    allBins.foreach(bin => {
      if (bin.label.hasPrev) bin ++= bin.label.prev.bin.activeCategories.filter(!_.contains('@')).map(_ + "@1")
      if (bin.label.hasNext) bin ++= bin.label.next.bin.activeCategories.filter(!_.contains('@')).map(_ + "@1")
    })
    println("bin domain length: " + BinDomain.dimensionDomain.length)

    println("Adding neighbor features...")
    allBins.foreach(bin => {
      bin ++= neighborFeatures(bin, -6)
      bin ++= neighborFeatures(bin, 6)
    })
    println("bin domain length: " + BinDomain.dimensionDomain.length)

    println("Adding interaction features...")
    allBins.foreach(bin => {
      val l = bin.activeCategories
      bin ++= l.map(_ => l).flatten.combinations(2).toList.filter(f => f(0) != f(1) &&
        FeatureDescriptor.featureNamesNotEqual(f(0), f(1))).map(f => f(0) + "*" + f(1))
    })
    println("bin domain length: " + BinDomain.dimensionDomain.length)

  }

  def neighborFeatures(bin : Bin, index : Int, current : Int = 0) : Set[String] = {
    if (Math.abs(current - index) <= 1) {
      Set.empty
    } else {
      val left = index < 0
      if (left && ! bin.label.hasPrev) return Set.empty
      if (! left && ! bin.label.hasNext) return Set.empty
      val neighbor = if (left) bin.label.prev.bin else bin.label.next.bin
      neighbor.activeCategories.filter(!_.contains('@')).map(_+"@<>").toSet.union(
        neighborFeatures(neighbor, index, current + (if(left) -1 else 1)))
    }

  }

}

class BooleanFeatureDescriptor(column : Int, name : String) extends FeatureDescriptor {
  def toFeatures(featureList : Array[String]) : Seq[String] = {
    if (featureList(column) == "1") { Seq(name + FeatureDescriptor.FeatureNameSeparator) } else Seq()
  }
  def possibleFeatures() = {
    Array(name + FeatureDescriptor.FeatureNameSeparator)
  }
  def print(writer : PrintWriter) = {
    writer.println("feature\tboolean\t" + column + "\t" + name)
  }
}
class CumulativeBinnedRealFeatureDescriptor(column : Int, name : String, nullBin : Double, bins : Array[Double])
  extends FeatureDescriptor {
  def toFeatures(featureList : Array[String]) : Seq[String] = {
    val featureVal = if (featureList(column) == "inf" || featureList(column) == "nan") -1.0 else featureList(column).toDouble
    realToCategoricalCumulativeBinnedFeatures(featureVal, name, nullBin, bins)
  }
  def possibleFeatures() = {
    Array(name +FeatureDescriptor.FeatureNameSeparator+ "-N") ++ bins.map(name + FeatureDescriptor.FeatureNameSeparator+ ">" + _)
  }
  def print(writer : PrintWriter) = {
    writer.println("feature\tcumulativeBinnedReal\t" + column + "\t" + name + "\t" + nullBin + "\t" + bins.mkString(","))
  }

}

class BinnedRealFeatureDescriptor(column : Int, name : String, bins : Array[Double])
  extends FeatureDescriptor {
  def toFeatures(featureList : Array[String]) : Seq[String] = {
    val featureVal = if (featureList(column) == "inf" || featureList(column) == "nan") -1.0 else featureList(column).toDouble
    realToCategoricalBinnedFeatures(featureVal, name, bins)
  }
  def possibleFeatures() = {
    (0 to bins.size).map(name + FeatureDescriptor.FeatureNameSeparator+ _)
  }
  def print(writer : PrintWriter) = {
    writer.println("feature\binnedReal\t" + column + "\t" + name + "\t" + bins.mkString(","))
  }
}

class FeatureDescriptors(val features : Array[_ <:FeatureDescriptor]) {
  def print(writer : PrintWriter) {
    features.foreach(_.print(writer))
  }
}

