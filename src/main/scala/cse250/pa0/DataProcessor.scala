package cse250.pa0
/**
 * cse250.pa0.AssessmentDataProcessor
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */

import cse250.objects.SolarInstallation

import scala.collection.mutable

object DataProcessor {
  def splitArrayToRowArray(splitHeaderRow: Array[String]): Array[String] = {
//    val mutableSplitHeaderRow =
//    for(i <- splitHeaderRow.indices) {
//      if(splitHeaderRow(i).contains("\"\"")) {
//        splitHeaderRow(i) = splitHeaderRow(i).replace("\"\"", "\"")
//        splitHeaderRow(i) = splitHeaderRow(i).stripPrefix("\"").stripSuffix("\"")
//      } else if(splitHeaderRow(i).startsWith("\"")) {
////        println(s"${splitHeaderRow(i)} \t ${splitHeaderRow(i) + "," +  splitHeaderRow(i + 1)}")
//        splitHeaderRow(i) += "," + splitHeaderRow(i + 1)
//        splitHeaderRow(i) = splitHeaderRow(i).stripPrefix("\"").stripSuffix("\"")
//        for(j <- i + 1 until splitHeaderRow.length - 1) {
//          splitHeaderRow(j) = splitHeaderRow(j + 1)
//        }
//        splitHeaderRow(splitHeaderRow.length - 1) = ""
//      }
//    }

//      val rowArray = new Array[String](31)
      val rowArray = mutable.ArrayBuilder.make[String]
      var inQuote = false
      var newIndexVal = ""
      for(i <- splitHeaderRow.indices) {
        if(inQuote) {
          newIndexVal += "," + splitHeaderRow(i).replace("\"\"", "\"")
          if(splitHeaderRow(i).endsWith("\"")) {
            inQuote = false
            newIndexVal = newIndexVal.stripPrefix("\"").stripSuffix("\"")
            rowArray += newIndexVal
          }
        } else if(splitHeaderRow(i).startsWith("\"")) {
          newIndexVal = splitHeaderRow(i)
          newIndexVal = newIndexVal.replace("\"\"", "\"")
          newIndexVal = newIndexVal.stripPrefix("\"").stripSuffix("\"")
          if(splitHeaderRow(i).endsWith("\"")) {
            rowArray += newIndexVal
          } else {
            inQuote = true
          }
        } else {
          rowArray += splitHeaderRow(i)
        }
      }

//    println(splitHeaderRow.mkString("Array(", ", ", ")"))
//    val fixInlineCommas = splitHeaderRow
//      .filter(x => x.indexOf("\"") <= 0 && x.reverse.indexOf("\"") <= 0)
//      .map(_.stripPrefix("\"").stripSuffix("\""))
//    (fixInlineCommas ++ new Array[String]((31 - fixInlineCommas.length).max(0))
//    (splitHeaderRow ++ new Array[String]((31 - splitHeaderRow.length).max(0))
//      .map(_ => "")).slice(0, 31)
    (rowArray.result() ++
      new Array[String]((31 - rowArray.result().length).max(0))
      .map(_ => "")).slice(0, 31)
    //    splitHeaderRow.filter(x => x.contains("\""))
//    splitHeaderRow.filter(x => (x.startsWith("\"") && x.endsWith("\"")) ||
//      (!x.startsWith("\"") && !x.endsWith("\"")))
//    println(splitHeaderRow)
//    splitHeaderRow
//      new Array[String](31 - splitHeaderRow.length).map(_ => "")
//    splitHeaderRow ++ new Array[String](31 - splitHeaderRow.length).map(_ => "")
  }

//  private def stripQuotes(string: String) = {
//    string.stripPrefix("\"").stripSuffix("\"")
//  }

  def rowArrayToSolarInstallation(rowData: Array[String]): SolarInstallation = {
    val x = new SolarInstallation()
    for(i <- SolarInstallation.REQUIRED_HEADERS) {
      x.fields(i) = rowData(SolarInstallation.HEADERS.indexOf(i))
    }
    x
  }

  def computeUniqueInverterManufacturers(dataset: Array[SolarInstallation]): Int = {
    dataset.map(_.fields("PRIMARY_INVERTER_MANUFACTURER")).filter(_ != "").tail.toSet.size
//    dataset.filter(allManufacturers - _ != allManufacturers)
  }

  def computeTotalExpectedKWHAnnualProduction(dataset: Array[SolarInstallation]): Float = {
    dataset.map(_.fields("EXPECTED_KWH_ANNUAL_PRODUCTION")).filter(_ != "").tail.map(_.toFloat.abs).sum
  }
}
