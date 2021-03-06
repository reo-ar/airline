package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject._
import views._
import models._
import com.patson.data.UserSource
import com.patson.model._
import com.patson.Authentication
import java.util.Calendar
import com.patson.data.AirlineSource
import play.api.libs.ws.WSClient
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import play.api.libs.json.Writes
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString

class SignUp @Inject()(cc: ControllerComponents)(ws: WSClient) extends AbstractController(cc) with play.api.i18n.I18nSupport {
  /**
   * Sign Up Form definition.
   *
   * Once defined it handle automatically, ,
   * validation, submission, errors, redisplaying, ...
   */
  val signupForm: Form[NewUser] = Form(
    
    // Define a mapping that will handle User values
    mapping(
      "username" -> text(minLength = 4, maxLength = 20).verifying(
        "username can only contain alphanumeric characters",
        userName => userName.forall(_.isLetterOrDigit)).verifying(
        "This username is not available",
        userName => !UserSource.loadUsersByCriteria(List.empty).map { _.userName.toLowerCase() }.contains(userName.toLowerCase())    
      ),
      "email" -> email,
      // Create a tuple mapping for the password/confirm
      "password" -> tuple(
        "main" -> text(minLength = 4),
        "confirm" -> text
      ).verifying(
        // Add an additional constraint: both passwords must match
        "Passwords don't match", passwords => passwords._1 == passwords._2
      ),
      "airlineName" -> text(minLength = 1, maxLength = 50).verifying(
        "Airline name can only contain space and characters",
        airlineName => airlineName.forall(char => char.isLetter || char == ' ') && !"".equals(airlineName.trim())).verifying(
        "This airline name is not available",
        airlineName => !AirlineSource.loadAllAirlines(false).map { _.name.toLowerCase().replaceAll("\\s", "") }.contains(airlineName.replaceAll("\\s", "").toLowerCase())
      ),
      "profileId" -> number
    )
    // The mapping signature doesn't match the User case class signature,
    // so we have to define custom binding/unbinding functions
    {
      // Binding: Create a User from the mapping result (ignore the second password and the accept field)
      (username, email, passwords, airlineName, profileId) => NewUser(username.trim, passwords._1, email.trim, airlineName.trim, profileId) 
    } 
    {
      // Unbinding: Create the mapping values from an existing User value
      user => Some(user.username, user.email, (user.password, ""), "", user.airlineName, 1)
    }
  )
  
  /**
   * Display an empty form.
   */
  def form = Action { implicit request =>
    Ok(html.signup(signupForm))
  }
  
  /**
   * Display a form pre-filled with an existing User.
   */
//  def editForm = Action {
//    val existingUser = NewUser("fakeuser", "secret", "fake@gmail.com") 
//    Ok(html.signup(signupForm.fill(existingUser)))
//  }
  
  implicit object ProfileWrites extends Writes[StartupProfile] {
    def writes(profile: StartupProfile): JsValue = {
          JsObject(List(
      "id" -> JsNumber(profile.id),
      "title" -> JsString(profile.title),
      "description" -> JsString(profile.description),
      "outlines" -> Json.toJson(profile.outlines)))
    }
  }
  
  def profiles = Action {
    Ok(Json.toJson(StartupProfile.profiles))
  }
  
  /**
   * Handle form submission.
   */
  def submit = Action { implicit request =>
    signupForm.bindFromRequest.fold(
      // Form has errors, redisplay it
      errors => BadRequest(html.signup(errors)), { userInput =>
                  // We got a valid User value, display the summary
          val user = User(userInput.username, userInput.email, Calendar.getInstance, Calendar.getInstance, UserStatus.ACTIVE, level = 0)
          UserSource.saveUser(user)
          Authentication.createUserSecret(userInput.username, userInput.password)
          
          val newAirline = Airline(userInput.airlineName)
          newAirline.setMaintenanceQuality(100)
          newAirline.setAirlineCode(newAirline.getDefaultAirlineCode())
          AirlineSource.saveAirlines(List(newAirline))
          UserSource.setUserAirline(user, newAirline)

          SearchUtil.addAirline(newAirline)
          
          val profile = StartupProfile.profilesById(userInput.profileId)
          profile.initializeAirline(newAirline)
          
          Redirect("/")

        Ok(html.index("User " + user.userName + " created! Please log in"))
      }
    )
  }
}