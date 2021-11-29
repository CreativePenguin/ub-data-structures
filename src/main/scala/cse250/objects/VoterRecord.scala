package cse250.objects

import java.util.Date

case class VoterRecord(m_FirstName: String,
                       m_LastName: String,
                       m_Birthday: Date,
                       m_ZipCode: String)
{
  def fullName: String = m_FirstName + " " + m_LastName
}