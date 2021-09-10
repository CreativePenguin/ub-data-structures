/**
 * AssessmentDataProcessorTests.scala
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
package cse250.pa0

import cse250.objects.SolarInstallation
import org.scalatest.flatspec.AnyFlatSpec


class DataProcessorTests extends AnyFlatSpec {
  behavior of "DataProcessor.splitArrayToRowArray"
  it should "return an Array with 32 entries when processing the header row" in {
    val headerRow = SolarInstallation.HEADERS.mkString(",")
    val splitHeaderRow = headerRow.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitHeaderRow)
    assert(result.length == 31)
    for (i <- splitHeaderRow.indices) assert(splitHeaderRow(i) == result(i))
  }


  it should "produces an array of correct length when processing the first entry (2nd row) of dataset" in {
    val splitSecondRow = SECOND_ROW.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitSecondRow)
    assert(result.length == 31)
    for (i <- EXPECTED_SECOND_ROW.indices) assert(result(i) == EXPECTED_SECOND_ROW(i))
  }

  behavior of "AssessmentDataProcessor.rowArrayToSolarInstallation"
  it should "return an exactly the required header fields" in {
    val headerRow = SolarInstallation.HEADERS.mkString(",")
    val splitHeaderRow = headerRow.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitHeaderRow)
    val result = DataProcessor.rowArrayToSolarInstallation(rowArray)
    assert(result.fields.size == SolarInstallation.REQUIRED_HEADERS.length)
    assert(result.toString == SolarInstallation.REQUIRED_HEADERS.mkString("",",",""))
  }

  it should "correctly process the first entry (2nd row) of file" in {
    val splitSecondRow = SECOND_ROW.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitSecondRow)
    assert(rowArray.length == 31)
    val result = DataProcessor.rowArrayToSolarInstallation(rowArray)
    assert(result.fields.size == SolarInstallation.REQUIRED_HEADERS.length)
    val expectedToString = EXPECTED_SECOND_ROW_REQUIRED.mkString(",")
    assert(result.toString == expectedToString)
  }

  val SECOND_ROW = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",Fronius USA,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,No,No,No,POINT (-76.497069 42.443738)"
  val EXPECTED_SECOND_ROW = Array(
    "07/31/2021",
    "0000000276",
    "01001-00018",
    "Ithaca",
    "Tompkins",
    "NY",
    "14850",
    "Non-Residential",
    "Residential/Small Commercial",
    "PON 1184",
    "",
    "",
    "08/02/2003",
    "03/11/2005",
    "Complete",
    "Solar Works, Inc.",
    "Fronius USA",
    "IG 2500-LV POS",
    "1",
    "Sharp",
    "NE-165U5",
    "12",
    "",
    "",
    "1.98",
    "",
    "",
    "No",
    "No",
    "No",
    "POINT (-76.497069 42.443738)"
  )
  val EXPECTED_SECOND_ROW_REQUIRED = Array(
    "07/31/2021",
    "0000000276",
    "Ithaca",
    "Tompkins",
    "NY",
    "14850",
    "Non-Residential",
    "",
    "08/02/2003",
    "03/11/2005",
    "Complete",
    "Solar Works, Inc.",
    "Fronius USA",
    "IG 2500-LV POS",
    "1",
    "Sharp",
    "NE-165U5",
    "12",
    "",
    "",
    "1.98",
    "",
    "No",
    "No",
    "No"
  )

}
