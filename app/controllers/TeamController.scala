package controllers

import javax.inject._

import play.api.libs.json.{JsError, Json}
import play.api.mvc._

case class Position(title: String)

case class Player(name: String, positions: Seq[Position])

case class Team(name: String, players: Seq[Player])

case class FillPosition(position: Position, player: Player)

case class Roster(team: Option[Team], filledPositions : Set[FillPosition])

trait JsonFormats {
  implicit val positionFormat = Json.format[Position]
  implicit val playerFormat = Json.format[Player]
  implicit val teamFormat = Json.format[Team]
  implicit val fillPositionFormat = Json.format[FillPosition]
  implicit val rosterFormat = Json.format[Roster]
}

@Singleton
class TeamController @Inject() extends Controller with JsonFormats {

  var defaultTeam = Team("Yankees", Seq.empty)
  var roster = Roster(Some(defaultTeam), Set.empty)

  def team = Action {
    Ok(Json.obj("status" -> "OK", "team" -> Json.toJson(defaultTeam)))
  }

  def addPlayer = Action(parse.json) { request =>
    request.body.validate[Player].fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      player => {

        val players = defaultTeam.players.+:(player)
        defaultTeam = defaultTeam.copy(players = players)
        Ok(Json.obj("status" -> "OK", "team" -> Json.toJson(defaultTeam)))
      }
    )
  }

  def fillRoster = Action(parse.json){ request =>
    request.body.validate[Roster].fold(
      errors => {
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toJson(errors)))
      },
      newRoster => {

        val fixedFilledPositions = newRoster.filledPositions.map { filledPosition =>
          filledPosition.copy(player = filledPosition.player.copy(positions = Seq(filledPosition.position)))
        }.toSeq

        defaultTeam = defaultTeam.copy(players = fixedFilledPositions.map(_.player))

        roster = Roster(Some(defaultTeam), fixedFilledPositions.toSet)

        Ok(Json.obj("status" -> "OK", "team" -> Json.toJson(roster)))
      }
    )
  }
}
