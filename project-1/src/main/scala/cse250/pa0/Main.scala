package cse250.pa0
/**
 * cse250.pa0.Main.scala
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */

import cse250.objects.SolarInstallation
import java.io.{ BufferedWriter, FileWriter }
import scala.io.Source
import scala.collection.mutable

object Main {

  /**
   * Entrypoint for the 250 project
   */
  def main(args: Array[String]): Unit =
  {
    val inputFilename = "data/Solar_Electric_Programs_Reported_by_NYSERDA__Beginning_2000.csv"
    val outputFilename = "data/Solar_Electric_Programs_Reported_by_NYSERDA__Beginning_2000.updated.csv"
    val inputFile = Source.fromFile(inputFilename)
    val outputFile = new BufferedWriter(new FileWriter(outputFilename))

    val dataset = mutable.ArrayBuffer[SolarInstallation]()

    try {
      /**
       * An iterator to the lines of the input file.  Becomes invalid as soon as inputFile is closed.
       * (do not close inputFile until after you finish reading from lines!)
       */
      val lines = inputFile.getLines()

      for (line <- lines) {
        val rowData = DataProcessor.splitArrayToRowArray(line.split(","))
        assert(rowData.length == 31, s"${rowData.length}, $line\n ${rowData.mkString("Array(", ", ", ")")}")
        val installation = DataProcessor.rowArrayToSolarInstallation(rowData)
        dataset.append(installation)
        outputFile.write(installation.toString)
        outputFile.write('\n')
      }
    } finally {
      inputFile.close()
      outputFile.close()
    }

    val inverterManufacturers = DataProcessor.computeUniqueInverterManufacturers(dataset.toArray)
    val totalKWHAnnually = DataProcessor.computeTotalExpectedKWHAnnualProduction(dataset.toArray)
    println(s"Number of Inverter Manufacturers: $inverterManufacturers")
    println(s"Expected Annual Production: $totalKWHAnnually KWH")
  }

}
