package controllers

import com.patson.data.{AirportSource, CountrySource, LinkStatisticsSource}
import com.patson.model.Computation.reputationBoostTop10
import com.patson.model._
import com.patson.util.CountryCache

object AirportUtil {
  val modelAirportPower : Long = AirportSource.loadAllAirports().map(_.power).sorted.last
  val modelCountryPower : Long = CountrySource.loadAllCountries().map(country => country.airportPopulation.toLong * country.income).sorted.last
  val MAX_COMPETITION_RATIO = 0.0001 //ratio of departingPassenger / airport power. If the ratio reaches this ratio, competition rating is considered 100
  val ECONOMIC_POWER_WEIGHT = 0.8
  val COUNTRY_POWER_WEIGHT = 0.2
  val COMPETITION_WEIGHT = 0.5

  def rateAirport(airport : Airport) : AirportRating = {
    val flightsFromThisAirport = LinkStatisticsSource.loadLinkStatisticsByFromAirport(airport.id, LinkStatisticsSource.SIMPLE_LOAD)
    val departurePassenger = flightsFromThisAirport.map(_.passengers).sum
    val country = CountryCache.getCountry(airport.countryCode).get
    val ratioToModelAirportPower = airport.power.toDouble / modelAirportPower
    val economicPowerRating = Math.max(0, (math.log10(ratioToModelAirportPower * 100) / 2 * 100).toInt)
    val ratioToModelCountryPower = country.airportPopulation * country.income.toDouble / modelCountryPower
    val countryPowerRating = (math.log10(ratioToModelCountryPower * 100) / 2 * 100).toInt
    val competitionRating = (Math.min(MAX_COMPETITION_RATIO, departurePassenger.toDouble / airport.power) / MAX_COMPETITION_RATIO * 10000).toInt
    val overallDifficulty = Math.min(100, (100 - economicPowerRating) * ECONOMIC_POWER_WEIGHT + (100 - countryPowerRating) * COUNTRY_POWER_WEIGHT + competitionRating * COMPETITION_WEIGHT).toInt


    AirportRating(economicPowerRating, competitionRating, countryPowerRating, airport.getFeatures(), overallDifficulty)
  }
}

//from 0 to 100
case class AirportRating(economicPowerRating : Int, competitionRating : Int, countryPowerRating : Int, features : List[AirportFeature], overallDifficulty : Int)