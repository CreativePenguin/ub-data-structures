/**
 * Main.scala
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa1

import cse250.objects.SolarInstallation

object Main {
  def main(args: Array[String]): Unit = {
    val solarInstallationStore = new LinkedListBuffer[SolarInstallation](10)

    val exampleElements = new Array[SolarInstallation](10);
    for(i <- 0 until exampleElements.size){
      val element = new SolarInstallation()
      exampleElements(i) = element
      element.fields(SolarInstallation.REQUIRED_HEADERS(0)) = i.toString
      for(j <- SolarInstallation.REQUIRED_HEADERS.drop(1)){ element.fields(j) = "" }
    }

    var numLines = 5
    for (entry <- exampleElements.take(numLines)) {
      solarInstallationStore.append(entry)
    }
    println(s"Storage after $numLines additions:")
    println("-----")
    println(solarInstallationStore)
    println("-----")

    numLines = 10
    for (entry <- exampleElements.take(numLines)) {
      solarInstallationStore.append(entry)
    }
    println(s"Storage after $numLines more additions:")
    println("-----")
    println(solarInstallationStore)
    println("-----")

    numLines = 5
    for (entry <- exampleElements.take(numLines)) {
      solarInstallationStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(solarInstallationStore)
    println("-----")

    numLines = 10
    for (entry <- exampleElements.take(numLines)) {
      solarInstallationStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(solarInstallationStore)
    println("-----")

    numLines = 1
    for (entry <- exampleElements.take(numLines); _ <- 1 to 5) {
      solarInstallationStore.append(entry)
    }
    println(s"Storage after adding 5 copies of of first line:")
    println("-----")
    println(solarInstallationStore)
    println("-----")

    numLines = 1
    for (entry <- exampleElements.take(numLines)) {
      solarInstallationStore.remove(entry)
    }
    println(s"Storage after removing first line:")
    println("-----")
    println(solarInstallationStore)
    println("-----")
  }
}
