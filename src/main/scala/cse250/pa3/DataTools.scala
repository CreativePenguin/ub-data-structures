/**
 * cse250.pa3.Join.scala
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
 package cse250.pa3

 import cse250.objects._

 import java.io.File
 import java.util.{Date, GregorianCalendar}
 import scala.collection.mutable
 import scala.io.Source

 object DataTools {
   /**
    * Convert date string (e.g., MM/DD/YYYY) to a [[Date]]
    *
    * @param dateString A date string in a standard format like MM/DD/YYYY
    * @return A [[Date]] encoding dateString
    */
   def parseDate(dateString: String): Date = {
     val dateParts = dateString.split(" ")(0).split("/")
     assert(dateParts.size == 3, s"'$dateString' is not a valid date string")
     new GregorianCalendar(
       dateParts(2).toInt, // YYYY
       dateParts(0).toInt - 1, // MM
       dateParts(1).toInt // DD
     ).getTime
   }

   def formatDate(date: Date): String = {
     //     val str = new SimpleDateFormat("MM/dd/yyyy").format(date)
     //     if (str.length <= 9) "0" + str else str
     //     new SimpleDateFormat("MM/dd/yyyy").format(date)
     date.toString
   }


   /**
    * Load a sequence of "anonymized" [[HealthRecord]]s from a CSV file
    *
    * @param filename The path to a CSV file.
    * @return The [[HealthRecord]] objects loaded from the file.
    *
    *         This function should make the following assumptions about the CSV file:
    *         1. The first line of the CSV file is a header.
   *         2. Header fields are
   *         * "Birthday"
   *         * "Zip Code"
   *         * "Wear Glasses?"
   *         * "Allergic to Dogs?"
   *         * "Brown Hair?"
   *         * "Blue Eyes?"
   *         3. Every subsequent line may be split into fields with [[String]]'s split method
   *         4. Columns containing dates are in a format interpretable by [[parseDate]]
   *
   *         Examples of valid CSV files can be found in `src/test/resources/`
   */
  def loadHealthRecords(filename: File): Seq[HealthRecord] = {
    val filebuf = Source.fromFile(filename)
    filebuf.getLines.next()
    var seq: Seq[HealthRecord] = Seq()
    for (line <- filebuf.getLines) {
      val cols = line.split(",")
      //      val cols = line.split(",").map(a => if(a == "Yes" || a == "No") a == "Yes" else a)
      val date = parseDate(cols(0))
      seq = seq :+ HealthRecord(date, cols(1), cols(2) == "Yes", cols(3) == "Yes",
        cols(4) == "Yes", cols(5) == "Yes")
    }
    filebuf.close()
    seq
  }

  /**
   * Load a sequence of [[VoterRecord]]s from a CSV file
   *
   * @param    filename    The path to a CSV file.
   * @return The [[VoterRecord]] objects loaded from the file.
   *
   *         This function should make the following assumptions:
   *         1. The first line of the CSV file is a header.
   *         2. Header fields are
   *         * "First Name"
   *         * "Last Name"
   *         * "Birthday"
   *         * "Zip Code"
   *         3. Every subsequent line may be split into fields with [[String]]'s split method
   *         4. Columns containing dates are in a format interpretable by [[parseDate]]
   *
   *         Examples of valid CSV files can be found in `src/test/resources/`
   */
  def loadVoterRecords(filename: File): Seq[VoterRecord] = {
    val filebuf = Source.fromFile(filename)
    filebuf.getLines.next()
    var seq: Seq[VoterRecord] = Seq()
    for (line <- filebuf.getLines) {
      val col: Array[String] = line.split(",")
      //      val date = DateFormat.getDateInstance(DateFormat.SHORT).parse(col(2))
      //      val date = col(2).split("/")
      val date = parseDate(col(2))
      seq = seq :+ VoterRecord(col(0), col(1), date, col(3))
    }
    filebuf.close()
    seq
  }

  /**
   * De-anonymize a collection of "anonymized" [[HealthRecord]] objects using [[VoterRecord]]s
   *
   * @param   voterRecords   A [[Seq]]uence of [[VoterRecord]]s containing names.
   * @param healthRecords    A [[Seq]]uence of "anonymized" [[HealthRecord]]s.
   * @return A [[Map]] of Full Names associated with their [[HealthRecord]]s.
   *
   *         For every [[HealthRecord]] that can be **uniquely** linked to a
   *         [[VoterRecord]], this function should return a key-value pair.
   *         The key should be the return value of the [[VoterRecord]]'s `fullName`
   *         method.  The value should be the (**unique**) associated
   *         [[HealthRecord]].
   *
   *         If a [[VoterRecord]] can not be associated to any [[HealthRecord]],
   *         or if it can not be **uniquely** associated to just one
   *         [[HealthRecord]], it should not be present in the result set.
   *
   *         This function **must** run in O(voterRecords.size + healthRecords.size)
   */
  def identifyPersons(
                       voterRecords: Seq[VoterRecord],
                       healthRecords: Seq[HealthRecord]
                     ): mutable.Map[String, HealthRecord] = {
    //    removeDup(voterRecords)
    //    val healthMap: Map[(Date, String), HealthRecord] = healthRecords
    //      .map(a => ((a.m_Birthday, a.m_ZipCode), a)).toMap
    val healthMap: mutable.Map[(Date, String), HealthRecord] = mutable.Map()
    val dupMap: mutable.Map[(Date, String), Boolean] = mutable.Map()
    for (i <- healthRecords) {
      //      if (i.m_Birthday.toString == "" || i.m_ZipCode == "" ||
      if (dupMap.contains((i.m_Birthday, i.m_ZipCode))) {}
      else if (healthMap.contains((i.m_Birthday, i.m_ZipCode))) {
        healthMap.remove((i.m_Birthday, i.m_ZipCode))
        dupMap((i.m_Birthday, i.m_ZipCode)) = true
        dupMap((i.m_Birthday, null)) = true
        dupMap((null, i.m_ZipCode)) = true
      } else {
        healthMap((i.m_Birthday, i.m_ZipCode)) = i
      }
    }

    val identifyMap: mutable.Map[String, HealthRecord] = mutable.HashMap()
    for (voter <- voterRecords) {
      //      if (voter.m_Birthday.toString == "" || voter.m_ZipCode == "") {}
      {
        val key = (voter.m_Birthday, voter.m_ZipCode)
        if (healthMap.contains(key)) {
          identifyMap(voter.fullName) = healthMap(key)
        } else if (healthMap.contains((voter.m_Birthday, null))) {
          identifyMap(voter.fullName) = healthMap((voter.m_Birthday, null))
        } else if (healthMap.contains((null, voter.m_ZipCode))) {
          identifyMap(voter.fullName) = healthMap((null, voter.m_ZipCode))
        }
      }
    }
    identifyMap
  }

   //   def removeDup(bdaynzip: Seq[(String, String)]): Seq[(String, String)] = {
   //     val bdaynzipdistinct = bdaynzip.distinct
   //     var noDupList: Seq[(String, String)] = Seq()
   //     //    val noDupList: mutable.Seq[(String, String)] = mutable.Seq()
   //     var offset = 0
   //     for (i <- bdaynzip.slice(0, bdaynzip.length - 1).indices) {
   //       if (bdaynzip(i + 1) != bdaynzipdistinct(i - offset + 1)) {
   //         offset += 1
   //       }
   //       noDupList
   //     }
   //     noDupList
   //   }

   /**
    * Compute a histogram over one of the attributes of HealthRecord
    *
    * @param records   A [[Seq]]uence of [[HealthRecord]]s
    * @param attribute Either [[HealthRecordBirthday]]
    *                  or [[HealthRecordZipCode]]
    * @return A key-value pair of each stringified attribute
    *         value and the percentage of the records that
    *         have this value.  The percentage should be
    *         a value in the range (0, 1]
    *
    *         If attribute == HealthRecordBirthday, use [[HealthRecord]]'s
    *         `m_Birthday` field.
    *
    *         If attribute == HealthRecordZipCode, use [[HealthRecord]]'s
    *         `m_ZipCode` field.
    *
    *         This function **must** run in O(healthRecords.size)
    */
   def computeHealthRecordDist(
                                records: Seq[HealthRecord],
                                attribute: HealthRecordAttribute
                              ): mutable.Map[String, Double] = {
     val map: mutable.Map[String, Double] = mutable.Map()
     val recordAttributes: Seq[String] = records.map(a =>
       if (attribute == HealthRecordBirthday)
         formatDate(a.m_Birthday)
       //        DateFormat.getDateInstance(DateFormat.SHORT).format(a.m_Birthday)
       else if (attribute == HealthRecordZipCode)
         a.m_ZipCode
       else
         return mutable.Map()
     )
     for (record <- recordAttributes) {
       if (map.contains(record)) {
         map(record) = map(record) + 1
       } else {
         map(record) = 1
       }
     }
     for (key <- map.keys) {
       map(key) = map(key) / records.size
     }
     map
   }
}
