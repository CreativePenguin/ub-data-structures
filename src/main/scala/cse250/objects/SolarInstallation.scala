/**
 * cse250.objects.HealthRecord.scala
 *
 * Copyright 2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.objects

import scala.collection.mutable

/**
 * One specific solar installation site.
 */
class SolarInstallation
{
  /**
   * Key-value pairs representing data about the solar site.  See [[SolarInstallation.HEADERS]] for a list of
   * allowable keys, and [[SolarInstallation.REQUIRED_HEADERS]] for a list of mandatory keys.
   */
  val fields: mutable.Map[String, String] = new mutable.HashMap[String, String]

  /**
   * Generate a human-readable representation of the installation
   * @return A human (but not machine)-readable representation of the [[SolarInstallation.REQUIRED_HEADERS]] of this site.
   */
  override def toString: String =
  {
    SolarInstallation.REQUIRED_HEADERS
      .map { h => fields.getOrElse(h, "**MISSING HEADER**") }
      .mkString(",")
  }
  /**
   * Generate a human-readable representation of the installation
   * @return A CSV encoding of the [[SolarInstallation.REQUIRED_HEADERS]] for this site.
   */
  def toCSV: String =
  {
    SolarInstallation.REQUIRED_HEADERS
      .map { h => fields.getOrElse(h, "**MISSING HEADER**") }
      .mkString("\"", "\",\"", "\"")
  }

  // Scala Cookbook: Equals
  // https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch04s16.html

  /**
   * Fast check for potential equivalence
   * @param a   An object instance to test for potential equivalence
   * @return    True if the object can potentially be equal, False if the object is definitely not equal
   */
  def canEqual(a: Any): Boolean = a.isInstanceOf[SolarInstallation]

  /**
   * Test for exact equality
   * @param that  An object to test for equivalence
   * @return    True if the object is definitely equal.
   */
  override def equals(that: Any): Boolean =
    that match {
      case that: SolarInstallation => that.canEqual(this) && this.hashCode.equals(that.hashCode) && this.fields.equals(that.fields)
      case _ => false
    }

  /**
   * Generate a hash code for this site (used by HashMap)
   * @return    A deterministic, pseudorandom integer based on this site's data.
   */
  override def hashCode: Int = fields.hashCode
}

object SolarInstallation
{
  /**
   * Headers of the base SolarInstallation dataset.
   */
  val HEADERS = Seq(
    "REPORTING_PERIOD",
    "PROJECT_NUMBER",
    "LEGACY_PROJECT_NUMBER",
    "CITY",
    "COUNTY",
    "STATE",
    "ZIP_CODE",
    "SECTOR",
    "PROGRAM_TYPE",
    "SOLICITATION",
    "ELECTRIC_UTILITY",
    "PURCHASE_TYPE",
    "DATE_APPLICATION_RECEIVED",
    "DATE_COMPLETED",
    "PROJECT_STATUS",
    "CONTRACTOR",
    "PRIMARY_INVERTER_MANUFACTURER",
    "PRIMARY_INVERTER_MODEL_NUMBER",
    "TOTAL_INVERTER_QUANTITY",
    "PRIMARY_PV_MODULE_MANUFACTURER",
    "PV_MODULE_MODEL_NUMBER",
    "TOTAL_PV_MODULE_QUANTITY",
    "PROJECT_COST",
    "INCENTIVE",
    "TOTAL_NAMEPLATE_KW_DC",
    "EXPECTED_KWH_ANNUAL_PRODUCTION",
    "REMOTE_NET_METERING",
    "AFFORDABLE_SOLAR",
    "COMMUNITY_DISTRIBUTED_GENERATION",
    "GREEN_JOBS_GREEN_NEW_YORK_PARTICIPANT",
    "GEOREFERENCE",
  )

  /**
   * Headers that must be present in a [[SolarInstallation]] instance.
   */
  val REQUIRED_HEADERS = Seq(
    "REPORTING_PERIOD",
    "PROJECT_NUMBER",
    "CITY",
    "COUNTY",
    "STATE",
    "ZIP_CODE",
    "SECTOR",
    "PURCHASE_TYPE",
    "DATE_APPLICATION_RECEIVED",
    "DATE_COMPLETED",
    "PROJECT_STATUS",
    "CONTRACTOR",
    "PRIMARY_INVERTER_MANUFACTURER",
    "PRIMARY_INVERTER_MODEL_NUMBER",
    "TOTAL_INVERTER_QUANTITY",
    "PRIMARY_PV_MODULE_MANUFACTURER",
    "PV_MODULE_MODEL_NUMBER",
    "TOTAL_PV_MODULE_QUANTITY",
    "PROJECT_COST",
    "INCENTIVE",
    "TOTAL_NAMEPLATE_KW_DC",
    "EXPECTED_KWH_ANNUAL_PRODUCTION",
    "AFFORDABLE_SOLAR",
    "COMMUNITY_DISTRIBUTED_GENERATION",
    "GREEN_JOBS_GREEN_NEW_YORK_PARTICIPANT",
  )

}

