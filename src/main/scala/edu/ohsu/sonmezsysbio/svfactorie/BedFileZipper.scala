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
  def main(args:Array[String]): Unit = {
    class Location(val chr : String, val loc : Int, val end : Int) extends Ordered[Location] {
      def isEOF : Boolean = (chr.equals("0") && loc == 0)
      def compare(that : Location) : Int = {
        println("comparing " + this + " to " + that)
        if (isEOF) {
          1
        } else {
          println (Ordering.Tuple2(Ordering.String, Ordering.Int).compare((chr, loc), (that.chr, that.loc)))
          Ordering.Tuple2(Ordering.String, Ordering.Int).compare((chr, loc), (that.chr, that.loc))
        }
      }
      override def toString(): String = chr + "\t" + loc + "\t" + end;
    }

    def parseLine(line : String) : (Location, Array[String]) = {
      val fields = line.split("\t")
      val loc = new Location(fields(0), fields(1).toInt, fields(2).toInt)
      val values = if (fields.length == 3) { Array("1") } else { fields.slice(3,fields.length) }
      (loc, values)
    }

    if (args.length > 1) {

      val f1 = args(0)
      val f2 = args(1)

      val f1Lines = Source.fromInputStream(new GZIPInputStream(new FileInputStream(f1))).getLines
      val f2Lines = Source.fromInputStream(new GZIPInputStream(new FileInputStream(f2))).getLines ++ Stream.continually("0\t0\t0\t0").iterator

      for (f1Line <- f1Lines) {
        val (f1l : Location, f1v) = parseLine(f1Line)
        print("got loc:\n" + f1Line + "\n")
        val (f2l : Location, f2v) = parseLine(f2Lines.dropWhile(parseLine(_)._1 < f1l).next)
        println (f2l, f2v)
        if (! f2l.isEOF && (f2l compare f1l) == 0) {
          println(f1l + "\t" + (f1v ++ f2v).mkString("\t"))
        } else {
          println(f1l + "\t" + f1v.mkString("\t") + "\t0")
        }
      }
    } else {
      println("usage: BedFileZipper original additionalInfoFile")
    }

  }
}
