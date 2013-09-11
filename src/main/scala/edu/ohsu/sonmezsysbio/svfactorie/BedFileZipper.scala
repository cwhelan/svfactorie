package edu.ohsu.sonmezsysbio.svfactorie

import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import scala.io.Source

/**
 * Created by IntelliJ IDEA.
 * User: cwhelan
 * Date: 7/22/13
 * Time: 3:32 PM
 *
 * This class will "Zip" values from two BED files that have the same intervals
 * defined, in the same sort order.
 */
object BedFileZipper {

  class Location(val chr : String, val loc : Int, val end : Int) extends Ordered[Location] {
    def isEOF : Boolean = (chr.equals("0") && loc == 0)
    def compare(that : Location) : Int = {
      if (isEOF) {
        1
      } else {
        Ordering.Tuple2(Ordering.String, Ordering.Int).compare((chr, loc), (that.chr, that.loc))
      }
    }
    def overlaps(that : Location) : Boolean = {
      that.chr == this.chr && that.loc <= this.end && that.end >= this.loc
    }
    override def toString(): String = chr + "\t" + loc + "\t" + end
  }

  def parseLine(line : String) : (Location, Array[String]) = {
    if (line.isEmpty) {
      (new Location("AAA", 0, 0), Array("0"))
    } else {
      val fields = line.split("\t")
      val loc = new Location(fields(0), fields(1).toInt, fields(2).toInt)
      val values = if (fields.length == 3) { Array("1") } else { fields.slice(3,fields.length) }
      (loc, values)
    }
  }

  class FeatureFile(val fileLines:Iterator[String]) {
    var (curLoc, curVals) =
      if (fileLines.hasNext) { parseLine(fileLines.next()) }
      else { (new Location("AAA", 0, 0), Array("0")) }
    val numFields = curVals.length
    def getFeatures(loc : Location) : Array[String] = {
      if (curLoc < loc && ! curLoc.overlaps(loc) && fileLines.hasNext) {
        val filteredLines: Iterator[String] = fileLines.dropWhile(s => {
          parseLine(s)._1 < loc
        })
        if (filteredLines.hasNext) {
          val (lineLoc, lineVals) = parseLine(filteredLines.next())
          curLoc = lineLoc
          curVals = lineVals
        }
      }
      if (curLoc.overlaps(loc)) {
        curVals
      } else {
        Array.fill(numFields){"0"}
      }
    }
  }

  def zipLines(windowLines: Iterator[String], featureList: Array[FeatureFile]) : Iterator[String] = {
    for (windowLine <- windowLines) yield {
      val windowLoc = parseLine(windowLine)._1
      val features = featureList.map(f => f.getFeatures(windowLoc)).reduce(_ ++ _)
      windowLoc + "\t" + features.mkString("\t")
    }
  }

  def main(args:Array[String]): Unit = {

    if (args.length > 1) {

      val windowsFile = args(0)

      val featureFiles = args.slice(1,args.length)

      val windowLines = Source.fromInputStream(new GZIPInputStream(new FileInputStream(windowsFile))).getLines

      val featureList = featureFiles.map(f => new FeatureFile(Source.fromInputStream(new GZIPInputStream(new FileInputStream(f))).getLines))

      for (l <- zipLines(windowLines, featureList)) {
        println(l)
      }
    } else {
      println("usage: BedFileZipper windows feature_file1 feature_file2 ...")
    }

  }
}
