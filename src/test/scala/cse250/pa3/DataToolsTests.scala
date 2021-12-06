/**
 * cse250.pa3.DataToolsTests
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 */
package cse250.pa3

import cse250.objects.{HealthRecord, HealthRecordBirthday, HealthRecordZipCode, VoterRecord}
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

/**
 * These tests are provided **to get you started**.  Passing these tests
 * does not guarantee that you will get a good grade on the project.  It
 * doesn't even guarantee that you will get a non-zero grade.
 * 
 * **Add your own tests!**
 */
class DataToolsTests extends AnyFlatSpec
{
  /**
   * Method to compare doubles with a specified degree of precision.
   */
  val EPSILON: Double = 0.0001

  def compareDoubles(d1: Double, d2: Double): Boolean = {
    Math.abs(d1 - d2) < EPSILON
  }

  "loadHealthRecords" must "Load HealthRecords" in 
  {
    val records = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-10.csv")
    )

    println(records)
    //    assert(records(0).m_ZipCode != "Zip Code", "Accidentally loading the header row")
    //    assert(records.size == 10, "Not loading the right number of records")
    //
    //    assert(records.map { _.m_Birthday } contains DataTools.parseDate("11/17/1978"))
    //    assert(records.map { _.m_ZipCode } contains "14261")
    //    assert(records.filter { _.m_ZipCode == "14261" }
    //                  .head.m_DogAllergy == true)
    //    assert(records.filter { _.m_ZipCode == "14261" }
    //                  .head.m_BlueEyes == false)
  }

  "loadVoterRecords" must "Load VoterRecords" in 
  {
    val records = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    assert(records(0).m_FirstName != "First Name", "Accidentally loading the header row")
    assert(records.size == 10, "Not loading the right number of records")

    assert(records.map {
      _.m_FirstName
    } contains "LILY")
    assert(records.map {
      _.m_FirstName
    } contains "KARA")
    assert(records.map {
      _.m_FirstName
    } contains "DEWEY")
  }

  //  "loadFunctions" should "have null values" in {
  //    val health = DataTools.loadHealthRecords(
  //      new File("src/test/resources/Health-Records-10.csv"))
  //    assert(health(10).m_ZipCode == "")
  //    println(health)
  //  }

  "identifyPersons" must "Identify Persons" in {
    val health = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-10.csv")
    )
    val voter = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    val deanonymized = DataTools.identifyPersons(voter, health)

    /*  The 10-row test data has matches for **every** record.  This 
        will not usually be the case! */
    for (v <- voter) {
      assert(deanonymized.keySet contains v.fullName)
    }

    assert(deanonymized("NIA GONZALEZ").m_DogAllergy == false)
    assert(deanonymized("NIA GONZALEZ").m_BlueEyes == false)
    assert(deanonymized("NIA GONZALEZ").m_BrownHair == true)
  }

  //  "identifyPersons" must "exclude duplicate dates" in {
  //    val health = DataTools.loadHealthRecords(
  //      new File("src/test/resources/Health-Records-100.csv")
  //    )
  //
  //    val voter = DataTools.loadVoterRecords(
  //      new File("src/test/resources/Voter-Records-10.csv")
  //    )
  //
  //    val deanonymized = DataTools.identifyPersons(voter, health)
  //
  //    assert(deanonymized("SIMON DURAN").m_DogAllergy)
  //  }

  "identifyPersons" must "work under null or some shit" in {
    var health = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-10.csv")
    )

    health = health :+ HealthRecord(null, "11111",
      m_Glasses = true, m_DogAllergy = false, m_BrownHair = false, m_BlueEyes = true)

    //    health = health :+ HealthRecord(null, null,
    //      m_Glasses = true, m_DogAllergy = false, m_BrownHair = false, m_BlueEyes = true)

    var voter = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    voter = voter :+ VoterRecord("SNAIL", "MAIL", null, "1111")
    //    voter = voter :+ VoterRecord("Jeremy", "Beremy", null, null)

    var deanonymized = DataTools.identifyPersons(voter, health)

    assert(deanonymized.contains("SNAIL MAIL"))

    health = health :+ HealthRecord(null, "11111",
      m_Glasses = true, m_DogAllergy = false, m_BrownHair = false, m_BlueEyes = true)

    deanonymized = DataTools.identifyPersons(voter, health)

    assert(!deanonymized.contains("SNAIL MAIL"))
    //    assert(!deanonymized("Snail Mail").m_Glasses)
    //    assert(!deanonymized("Jeremy Beremy").m_Glasses)
  }

  "identifyPersons" must "not have dups" in {
    var health = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-10.csv")
    )
    var voter = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    health = health :+ HealthRecord(
      DataTools.parseDate("9/23/1996"), "14209",
      true, true, true, true
    )

    health = health :+ HealthRecord(
      DataTools.parseDate("9/23/1996"), "14209",
      true, true, true, true
    )

    val deanonymized = DataTools.identifyPersons(voter, health)

    assert(!deanonymized.contains("EMELIA STEWART"))
    println(deanonymized)
  }

  "computeHealthRecordDist" must "Compute Statistics for ZipCode" in {
    val records = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-100.csv")
    )

    val dist = DataTools.computeHealthRecordDist(records, HealthRecordZipCode)

    assert(!(dist contains DataTools.parseDate("09/07/1925").toString), "You're loading birthdays instead of zip codes")
    assert(dist contains "14214")
    assert(compareDoubles(dist("14214"), 0.03))
    assert(dist contains "14211")
    assert(compareDoubles(dist("14211"), 0.05))
  }

  "computeHealthRecordDist" must "Compute Statistics for Birthday" in 
  {
    val records = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-100.csv")
    )

    val dist = DataTools.computeHealthRecordDist(records, HealthRecordBirthday)

    assert(!(dist contains "14214"), "You're loading zip codes instead of birthdays")
    assert(dist contains DataTools.parseDate("09/07/1925").toString)
    assert(compareDoubles(dist(DataTools.parseDate("09/07/1925").toString), 0.01))
  }
}
