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
  def splitArrayToRowArray(splitHeaderRow: Array[String]): Array[String] = ???

  def rowArrayToSolarInstallation(rowData: Array[String]): SolarInstallation = ???

  def computeUniqueInverterManufacturers(dataset: Array[SolarInstallation]): Int = ???

  def computeTotalExpectedKWHAnnualProduction(dataset: Array[SolarInstallation]): Float = ???
}
