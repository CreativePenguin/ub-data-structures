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

import java.util.Date

case class HealthRecord(m_Birthday: Date,
                        m_ZipCode: String,
                        m_Glasses: Boolean,
                        m_DogAllergy: Boolean,
                        m_BrownHair: Boolean,
                        m_BlueEyes: Boolean)

sealed trait HealthRecordAttribute

case object HealthRecordBirthday extends HealthRecordAttribute
case object HealthRecordZipCode extends HealthRecordAttribute