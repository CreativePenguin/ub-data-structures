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
  it should "return an Array with 31 entries when processing the header row" in {
    val headerRow = SolarInstallation.HEADERS.mkString(",")
    val splitHeaderRow = headerRow.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitHeaderRow)
    assert(result.length === 31)
    for (i <- splitHeaderRow.indices) assert(splitHeaderRow(i) == result(i))
  }


  it should "produces an array of correct length when processing the first entry (2nd row) of dataset" in {
    val splitSecondRow = SECOND_ROW.split(',')
    val result = DataProcessor.splitArrayToRowArray(splitSecondRow)
    assert(result.length === 31, result)
    println(EXPECTED_SECOND_ROW.mkString("Array(", ", ", ")"))
    println(result.mkString("Array(", ", ", ")"))
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

  it should "pad for short lines" in {
    val splitShortRow = SHORT_ROW.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitShortRow)
    assert(rowArray.length === 31)
  }

  "DUMB_ROW" should "be length 31" in {
    val splitDumbRow = DUMB_ROW.split(',')
    println(splitDumbRow.mkString("Array(", ", ", ")"))
    val rowArray = DataProcessor.splitArrayToRowArray(splitDumbRow)
    assert(rowArray.length === 31)
  }

  it should "properly translate the quotes in csv to data" in {
    val splitQuoteRow = QUOTE_ROW.split(',')
    val rowArray = DataProcessor.splitArrayToRowArray(splitQuoteRow)
    assert(rowArray.length === 31)
//    assert(rowArray.toString === EXPECTED_QUOTE_ROW.toString)
    println(rowArray.mkString("Array(", ", ", ")"))
    for(i <- rowArray.indices) assert(rowArray(i) === EXPECTED_QUOTE_ROW(i))
  }

  val SECOND_ROW = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",Fronius USA,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,No,No,No,POINT (-76.497069 42.443738)"
  val SHORT_ROW = "07/31/2021,0000000276,01001-00018,Ithaca,Tompkins,NY,14850,Non-Residential,Residential/Small Commercial,PON 1184,,,08/02/2003,03/11/2005,Complete,\"Solar Works, Inc.\",Fronius USA,IG 2500-LV POS,1,Sharp,NE-165U5,12,,,1.98,,,,,,"
  val DUMB_ROW = "08/31/2021,0000103762,,\"NEW ROCHELLE,\",Westchester,NY,10801,Residential,Residential/Small Commercial,PON 2112,Consolidated Edison,Purchase,03/27/2018,04/17/2018,Complete,Vivint Solar Development LLC,Enphase Energy Inc.,IQ6-60-x-US (240V),11,Hanwha Q CELLS,Q.PEAK BLK-G4.1 290,11,15940.50,676.00,3.19,2226.00,,No,No,No,POINT (-73.785079 40.917133)"
  val QUOTE_ROW = "\"\"\"Object, Oriented, Abstraction\"\"\",\"The \"\"Best\"\" Around\",\"Comma, cell, \"\"yay\"\" cell\""
  val EXPECTED_QUOTE_ROW = Array("\"Object, Oriented, Abstraction\"", "The \"Best\" Around", "Comma, cell, \"yay\" cell") ++
    new Array[String](28).map(_ => "")
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
