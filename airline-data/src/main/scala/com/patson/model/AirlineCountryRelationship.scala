package com.patson.model

import com.patson.data.{CountrySource, CycleSource, DelegateSource}
import com.patson.model.AirlineCountryRelationship.countryMap

import scala.collection.mutable

case class AirlineCountryRelationship(airline : Airline, country : Country, factors : Map[RelationshipFactor, Int]) {
  val relationship = factors.values.sum
}

abstract class RelationshipFactor {
  val getDescription : String
}

object AirlineCountryRelationship {
  val countryRelationships = CountrySource.getCountryMutualRelationships()
  val countryMap = CountrySource.loadAllCountries().map(country => (country.countryCode, country)).toMap
  val HOME_COUNTRY = (homeCountry : Country, targetCountry : Country, relationship : Int) => new RelationshipFactor {
    override val getDescription: String = {
      val relationshipString = relationship match {
        case x if x >= 5 => "Home Country"
        case 4 => "Alliance"
        case 3 => "Close"
        case 2 => "Friendly"
        case 1 => "Warm"
        case 0 => "Neutral"
        case -1 => "Cold"
        case -2 => "Hostile"
        case -3 => "In Conflict"
        case _ => "War"
      }
      if (homeCountry.countryCode == targetCountry.countryCode) {
        s"Your home country ${homeCountry.name}"
      } else {
        s"Relationship between your home country ${homeCountry.name} and ${targetCountry.name} : ${relationshipString}"
      }
    }
  }


  val MARKET_SHARE = (percentage : BigDecimal) => new RelationshipFactor {
    override val getDescription: String = {
      s"${percentage}% of market share"
    }
  }

  val TITLE = (title : CountryAirlineTitle) => new RelationshipFactor {
    override val getDescription: String = {
      title.description
    }
  }

  val DELEGATE = (delegateLevel : Int) => new RelationshipFactor {
    override val getDescription: String = {
      s"Total delegate level ${delegateLevel}"
    }
  }

  val HOME_COUNTRY_RELATIONSHIP_MULTIPLIER = 5
  def getAirlineCountryRelationship(countryCode : String, airline : Airline) : AirlineCountryRelationship = {
    val factors = mutable.LinkedHashMap[RelationshipFactor, Int]()
    val targetCountry = countryMap(countryCode)
    airline.getCountryCode() match {
      case Some(homeCountryCode) =>
        val relationship = countryRelationships.getOrElse((homeCountryCode, countryCode), 0)
        factors.put(HOME_COUNTRY(countryMap(homeCountryCode), targetCountry, relationship), relationship * HOME_COUNTRY_RELATIONSHIP_MULTIPLIER)
      case None =>
    }

    CountrySource.loadCountryAirlineTitlesByAirlineAndCountry(airline.id, countryCode).foreach {
      title => {
        val relationshipBonus = Title.relationshipBonus(title.title)
        factors.put(TITLE(title), relationshipBonus)
      }
    }

    CountrySource.loadMarketSharesByCountryCode(countryCode).foreach {
      marketShares => {
        marketShares.airlineShares.get(airline.id).foreach {
          marketShareOfThisAirline => {
            var percentage = BigDecimal(marketShareOfThisAirline.toDouble / marketShares.airlineShares.values.sum * 100)
            percentage = percentage.setScale(2, BigDecimal.RoundingMode.HALF_UP)
            val relationshipBonus : Int = percentage match {
              case x if x >= 50 => 40
              case x if x >= 25 => 30
              case x if x >= 10 => 25
              case x if x >= 5 => 20
              case x if x >= 2 => 15
              case x if x >= 1 => 10
              case x if x >= 0.5 => 8
              case x if x >= 0.1 => 6
              case x if x >= 0.02 => (x * 50).toInt
              case _ => 1
            }
            factors.put(MARKET_SHARE(percentage), relationshipBonus)
          }
        }
      }
    }
    val currentCycle = CycleSource.loadCycle()
    val totalLevel : Int = DelegateSource.loadCountryDelegateByAirlineAndCountry(airline.id, countryCode).map(_.assignedTask.asInstanceOf[CountryDelegateTask].level(currentCycle)).sum


    val levelMultiplier = getDelegateBonusMultiplier(targetCountry)
    factors.put(DELEGATE(totalLevel), Math.round(totalLevel * levelMultiplier).toInt)
    AirlineCountryRelationship(airline, targetCountry, factors.toMap)
  }

  val getDelegateBonusMultiplier = (country : Country) => {
    //US: pop 97499995 income 54629
    val modelPower : Long = 97499995L * 54629L
    val ratioToModelPower = (country.airportPopulation * country.income.toDouble).toLong / modelPower
    val logRatio = Math.min(0.1, Math.log10(ratioToModelPower * 100) / 2) //0.1 to 1
    val levelMultiplier = 1 / logRatio * 0.5 // >= 0.5, inverse of logRatio : lower multiplier for more powerful country
    Math.min(2, BigDecimal(levelMultiplier).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
  }
}
