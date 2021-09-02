# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' ScheduleGameTeamsAwayTeam Class
#'
#' @field id 
#' @field name 
#' @field link 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ScheduleGameTeamsAwayTeam <- R6::R6Class(
  'ScheduleGameTeamsAwayTeam',
  public = list(
    `id` = NULL,
    `name` = NULL,
    `link` = NULL,
    initialize = function(`id`, `name`, `link`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        stopifnot(R6::is.R6(`id`))
        self$`id` <- `id`
      }
      if (!missing(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!missing(`link`)) {
        stopifnot(is.character(`link`), length(`link`) == 1)
        self$`link` <- `link`
      }
    },
    toJSON = function() {
      ScheduleGameTeamsAwayTeamObject <- list()
      if (!is.null(self$`id`)) {
        ScheduleGameTeamsAwayTeamObject[['id']] <- self$`id`$toJSON()
      }
      if (!is.null(self$`name`)) {
        ScheduleGameTeamsAwayTeamObject[['name']] <- self$`name`
      }
      if (!is.null(self$`link`)) {
        ScheduleGameTeamsAwayTeamObject[['link']] <- self$`link`
      }

      ScheduleGameTeamsAwayTeamObject
    },
    fromJSON = function(ScheduleGameTeamsAwayTeamJson) {
      ScheduleGameTeamsAwayTeamObject <- jsonlite::fromJSON(ScheduleGameTeamsAwayTeamJson)
      if (!is.null(ScheduleGameTeamsAwayTeamObject$`id`)) {
        idObject <- BigDecimal$new()
        idObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsAwayTeamObject$id, auto_unbox = TRUE))
        self$`id` <- idObject
      }
      if (!is.null(ScheduleGameTeamsAwayTeamObject$`name`)) {
        self$`name` <- ScheduleGameTeamsAwayTeamObject$`name`
      }
      if (!is.null(ScheduleGameTeamsAwayTeamObject$`link`)) {
        self$`link` <- ScheduleGameTeamsAwayTeamObject$`link`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "name": %s,
           "link": %s
        }',
        self$`id`$toJSON(),
        self$`name`,
        self$`link`
      )
    },
    fromJSONString = function(ScheduleGameTeamsAwayTeamJson) {
      ScheduleGameTeamsAwayTeamObject <- jsonlite::fromJSON(ScheduleGameTeamsAwayTeamJson)
      BigDecimalObject <- BigDecimal$new()
      self$`id` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleGameTeamsAwayTeamObject$id, auto_unbox = TRUE))
      self$`name` <- ScheduleGameTeamsAwayTeamObject$`name`
      self$`link` <- ScheduleGameTeamsAwayTeamObject$`link`
    }
  )
)
