# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' ScheduleGameTeamsHome Class
#'
#' @field leagueRecord 
#' @field score 
#' @field team 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ScheduleGameTeamsHome <- R6::R6Class(
  'ScheduleGameTeamsHome',
  public = list(
    leagueRecord = NULL,
    score = NULL,
    team = NULL,
    initialize = function(leagueRecord, score, team){
      if (!missing(leagueRecord)) {
        stopifnot(R6::is.R6(leagueRecord))
        self$leagueRecord <- leagueRecord
      }
      if (!missing(score)) {
        stopifnot(is.numeric(score), length(score) == 1)
        stopifnot(R6::is.R6(score))
        self$score <- score
      }
      if (!missing(team)) {
        stopifnot(R6::is.R6(team))
        self$team <- team
      }
    },
    toJSON = function() {
      ScheduleGameTeamsHomeObject <- list()
      if (!is.null(self$leagueRecord)) {
        ScheduleGameTeamsHomeObject[['leagueRecord']] <- self$leagueRecord$toJSON()
      }
      if (!is.null(self$score)) {
        ScheduleGameTeamsHomeObject[['score']] <- self$score$toJSON()
      }
      if (!is.null(self$team)) {
        ScheduleGameTeamsHomeObject[['team']] <- self$team$toJSON()
      }

      ScheduleGameTeamsHomeObject
    },
    fromJSON = function(ScheduleGameTeamsHomeJson) {
      ScheduleGameTeamsHomeObject <- jsonlite::fromJSON(ScheduleGameTeamsHomeJson)
      if (!is.null(ScheduleGameTeamsHomeObject$leagueRecord)) {
        leagueRecordObject <- ScheduleGameTeamsHomeLeagueRecord$new()
        leagueRecordObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$leagueRecord, auto_unbox = TRUE))
        self$leagueRecord <- leagueRecordObject
      }
      if (!is.null(ScheduleGameTeamsHomeObject$score)) {
        scoreObject <- BigDecimal$new()
        scoreObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$score, auto_unbox = TRUE))
        self$score <- scoreObject
      }
      if (!is.null(ScheduleGameTeamsHomeObject$team)) {
        teamObject <- PlayerCurrentTeam$new()
        teamObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$team, auto_unbox = TRUE))
        self$team <- teamObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "leagueRecord": %s,
           "score": %s,
           "team": %s
        }',
        self$leagueRecord$toJSON(),
        self$score$toJSON(),
        self$team$toJSON()
      )
    },
    fromJSONString = function(ScheduleGameTeamsHomeJson) {
      ScheduleGameTeamsHomeObject <- jsonlite::fromJSON(ScheduleGameTeamsHomeJson)
      ScheduleGameTeamsHomeLeagueRecordObject <- ScheduleGameTeamsHomeLeagueRecord$new()
      self$leagueRecord <- ScheduleGameTeamsHomeLeagueRecordObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$leagueRecord, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$score <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$score, auto_unbox = TRUE))
      PlayerCurrentTeamObject <- PlayerCurrentTeam$new()
      self$team <- PlayerCurrentTeamObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsHomeObject$team, auto_unbox = TRUE))
    }
  )
)