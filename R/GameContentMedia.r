# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameContentMedia Class
#'
#' @field epg 
#' @field milestones 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameContentMedia <- R6::R6Class(
  'GameContentMedia',
  public = list(
    epg = NULL,
    milestones = NULL,
    initialize = function(epg, milestones){
      if (!missing(epg)) {
        stopifnot(is.list(epg), length(epg) != 0)
        lapply(epg, function(x) stopifnot(R6::is.R6(x)))
        self$epg <- epg
      }
      if (!missing(milestones)) {
        stopifnot(R6::is.R6(milestones))
        self$milestones <- milestones
      }
    },
    toJSON = function() {
      GameContentMediaObject <- list()
      if (!is.null(self$epg)) {
        GameContentMediaObject[['epg']] <- lapply(self$epg, function(x) x$toJSON())
      }
      if (!is.null(self$milestones)) {
        GameContentMediaObject[['milestones']] <- self$milestones$toJSON()
      }

      GameContentMediaObject
    },
    fromJSON = function(GameContentMediaJson) {
      GameContentMediaObject <- jsonlite::fromJSON(GameContentMediaJson)
      if (!is.null(GameContentMediaObject$epg)) {
        self$epg <- lapply(GameContentMediaObject$epg, function(x) {
          epgObject <- AnyOfGameContentMediaEpgItems$new()
          epgObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          epgObject
        })
      }
      if (!is.null(GameContentMediaObject$milestones)) {
        milestonesObject <- GameContentMediaMilestones$new()
        milestonesObject$fromJSON(jsonlite::toJSON(GameContentMediaObject$milestones, auto_unbox = TRUE))
        self$milestones <- milestonesObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "epg": [%s],
           "milestones": %s
        }',
        lapply(self$epg, function(x) paste(x$toJSON(), sep=",")),
        self$milestones$toJSON()
      )
    },
    fromJSONString = function(GameContentMediaJson) {
      GameContentMediaObject <- jsonlite::fromJSON(GameContentMediaJson)
      self$epg <- lapply(GameContentMediaObject$epg, function(x) AnyOfGameContentMediaEpgItems$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      GameContentMediaMilestonesObject <- GameContentMediaMilestones$new()
      self$milestones <- GameContentMediaMilestonesObject$fromJSON(jsonlite::toJSON(GameContentMediaObject$milestones, auto_unbox = TRUE))
    }
  )
)