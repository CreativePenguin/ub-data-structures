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

import org.scalatest.flatspec.AnyFlatSpec
import cse250.objects.{ HealthRecord, VoterRecord }
import java.io.File
import cse250.objects.HealthRecordZipCode
import cse250.objects.HealthRecordBirthday

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

    assert(records(0).m_ZipCode != "Zip Code", "Accidentally loading the header row")
    assert(records.size == 10, "Not loading the right number of records")

    assert(records.map { _.m_Birthday } contains DataTools.parseDate("11/17/1978"))
    assert(records.map { _.m_ZipCode } contains "14261")
    assert(records.filter { _.m_ZipCode == "14261" }
                  .head.m_DogAllergy == true)
    assert(records.filter { _.m_ZipCode == "14261" }
                  .head.m_BlueEyes == false)
  }

  "loadVoterRecords" must "Load VoterRecords" in 
  {
    val records = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    assert(records(0).m_FirstName != "First Name", "Accidentally loading the header row")
    assert(records.size == 10, "Not loading the right number of records")

    assert(records.map { _.m_FirstName} contains "LILY")
    assert(records.map { _.m_FirstName} contains "KARA")
    assert(records.map { _.m_FirstName} contains "DEWEY")
  }

  "identifyPersons" must "Identify Persons" in 
  {
    val health = DataTools.loadHealthRecords(
      new File("src/test/resources/Health-Records-10.csv")
    )
    val voter = DataTools.loadVoterRecords(
      new File("src/test/resources/Voter-Records-10.csv")
    )

    val deanonymized = DataTools.identifyPersons(voter, health)

    /*  The 10-row test data has matches for **every** record.  This 
        will not usually be the case! */
    for(v <- voter) {
      assert(deanonymized.keySet contains v.fullName)
    }

    assert(deanonymized("NIA GONZALEZ").m_DogAllergy == false)
    assert(deanonymized("NIA GONZALEZ").m_BlueEyes == false)
    assert(deanonymized("NIA GONZALEZ").m_BrownHair == true)
  }

  "computeHealthRecordDist" must "Compute Statistics for ZipCode" in 
  {
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
