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

object DataProcessor {
  def splitArrayToRowArray(splitHeaderRow: Array[String]): Array[String] = {
    for(i <- splitHeaderRow.indices) {
      if(splitHeaderRow(i).contains("\"")) splitHeaderRow(i) += splitHeaderRow(i + 1)
    }
    splitHeaderRow.filter(_.endsWith("\"")) ++
      new Array[String](31 - splitHeaderRow.length).map(_ => "")
//    splitHeaderRow ++ new Array[String](31 - splitHeaderRow.length).map(_ => "")
  }

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
    dataset.map(_.fields("EXPECTED_KWH_ANNUAL_PRODUCTION")).filter(_ != "").tail.map(_.toInt).sum
  }
}
