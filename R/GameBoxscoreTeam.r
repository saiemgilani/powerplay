# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameBoxscoreTeam Class
#'
#' @field team 
#' @field teamStats 
#' @field players 
#' @field goalies 
#' @field skaters 
#' @field onIce 
#' @field onIcePlus 
#' @field scratches 
#' @field penaltyBox 
#' @field coaches 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameBoxscoreTeam <- R6::R6Class(
  'GameBoxscoreTeam',
  public = list(
    `team` = NULL,
    `teamStats` = NULL,
    `players` = NULL,
    `goalies` = NULL,
    `skaters` = NULL,
    `onIce` = NULL,
    `onIcePlus` = NULL,
    `scratches` = NULL,
    `penaltyBox` = NULL,
    `coaches` = NULL,
    initialize = function(`team`, `teamStats`, `players`, `goalies`, `skaters`, `onIce`, `onIcePlus`, `scratches`, `penaltyBox`, `coaches`){
      if (!missing(`team`)) {
        stopifnot(R6::is.R6(`team`))
        self$`team` <- `team`
      }
      if (!missing(`teamStats`)) {
        stopifnot(R6::is.R6(`teamStats`))
        self$`teamStats` <- `teamStats`
      }
      if (!missing(`players`)) {
        stopifnot(R6::is.R6(`players`))
        self$`players` <- `players`
      }
      if (!missing(`goalies`)) {
        stopifnot(is.list(`goalies`), length(`goalies`) != 0)
        lapply(`goalies`, function(x) stopifnot(R6::is.R6(x)))
        self$`goalies` <- `goalies`
      }
      if (!missing(`skaters`)) {
        stopifnot(is.list(`skaters`), length(`skaters`) != 0)
        lapply(`skaters`, function(x) stopifnot(R6::is.R6(x)))
        self$`skaters` <- `skaters`
      }
      if (!missing(`onIce`)) {
        stopifnot(is.list(`onIce`), length(`onIce`) != 0)
        lapply(`onIce`, function(x) stopifnot(R6::is.R6(x)))
        self$`onIce` <- `onIce`
      }
      if (!missing(`onIcePlus`)) {
        stopifnot(is.list(`onIcePlus`), length(`onIcePlus`) != 0)
        lapply(`onIcePlus`, function(x) stopifnot(R6::is.R6(x)))
        self$`onIcePlus` <- `onIcePlus`
      }
      if (!missing(`scratches`)) {
        stopifnot(is.list(`scratches`), length(`scratches`) != 0)
        lapply(`scratches`, function(x) stopifnot(R6::is.R6(x)))
        self$`scratches` <- `scratches`
      }
      if (!missing(`penaltyBox`)) {
        stopifnot(is.list(`penaltyBox`), length(`penaltyBox`) != 0)
        lapply(`penaltyBox`, function(x) stopifnot(R6::is.R6(x)))
        self$`penaltyBox` <- `penaltyBox`
      }
      if (!missing(`coaches`)) {
        stopifnot(is.list(`coaches`), length(`coaches`) != 0)
        lapply(`coaches`, function(x) stopifnot(R6::is.R6(x)))
        self$`coaches` <- `coaches`
      }
    },
    toJSON = function() {
      GameBoxscoreTeamObject <- list()
      if (!is.null(self$`team`)) {
        GameBoxscoreTeamObject[['team']] <- self$`team`$toJSON()
      }
      if (!is.null(self$`teamStats`)) {
        GameBoxscoreTeamObject[['teamStats']] <- self$`teamStats`$toJSON()
      }
      if (!is.null(self$`players`)) {
        GameBoxscoreTeamObject[['players']] <- self$`players`$toJSON()
      }
      if (!is.null(self$`goalies`)) {
        GameBoxscoreTeamObject[['goalies']] <- lapply(self$`goalies`, function(x) x$toJSON())
      }
      if (!is.null(self$`skaters`)) {
        GameBoxscoreTeamObject[['skaters']] <- lapply(self$`skaters`, function(x) x$toJSON())
      }
      if (!is.null(self$`onIce`)) {
        GameBoxscoreTeamObject[['onIce']] <- lapply(self$`onIce`, function(x) x$toJSON())
      }
      if (!is.null(self$`onIcePlus`)) {
        GameBoxscoreTeamObject[['onIcePlus']] <- lapply(self$`onIcePlus`, function(x) x$toJSON())
      }
      if (!is.null(self$`scratches`)) {
        GameBoxscoreTeamObject[['scratches']] <- lapply(self$`scratches`, function(x) x$toJSON())
      }
      if (!is.null(self$`penaltyBox`)) {
        GameBoxscoreTeamObject[['penaltyBox']] <- lapply(self$`penaltyBox`, function(x) x$toJSON())
      }
      if (!is.null(self$`coaches`)) {
        GameBoxscoreTeamObject[['coaches']] <- lapply(self$`coaches`, function(x) x$toJSON())
      }

      GameBoxscoreTeamObject
    },
    fromJSON = function(GameBoxscoreTeamJson) {
      GameBoxscoreTeamObject <- jsonlite::fromJSON(GameBoxscoreTeamJson)
      if (!is.null(GameBoxscoreTeamObject$`team`)) {
        teamObject <- GameBoxscoreTeamTeam$new()
        teamObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$team, auto_unbox = TRUE))
        self$`team` <- teamObject
      }
      if (!is.null(GameBoxscoreTeamObject$`teamStats`)) {
        teamStatsObject <- GameBoxscoreTeamTeamStats$new()
        teamStatsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$teamStats, auto_unbox = TRUE))
        self$`teamStats` <- teamStatsObject
      }
      if (!is.null(GameBoxscoreTeamObject$`players`)) {
        playersObject <- GameBoxscoreTeamPlayers$new()
        playersObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$players, auto_unbox = TRUE))
        self$`players` <- playersObject
      }
      if (!is.null(GameBoxscoreTeamObject$`goalies`)) {
        self$`goalies` <- lapply(GameBoxscoreTeamObject$`goalies`, function(x) {
          goaliesObject <- BigDecimal$new()
          goaliesObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          goaliesObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`skaters`)) {
        self$`skaters` <- lapply(GameBoxscoreTeamObject$`skaters`, function(x) {
          skatersObject <- BigDecimal$new()
          skatersObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          skatersObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`onIce`)) {
        self$`onIce` <- lapply(GameBoxscoreTeamObject$`onIce`, function(x) {
          onIceObject <- BigDecimal$new()
          onIceObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          onIceObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`onIcePlus`)) {
        self$`onIcePlus` <- lapply(GameBoxscoreTeamObject$`onIcePlus`, function(x) {
          onIcePlusObject <- GameBoxscoreTeamOnIcePlus$new()
          onIcePlusObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          onIcePlusObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`scratches`)) {
        self$`scratches` <- lapply(GameBoxscoreTeamObject$`scratches`, function(x) {
          scratchesObject <- BigDecimal$new()
          scratchesObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          scratchesObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`penaltyBox`)) {
        self$`penaltyBox` <- lapply(GameBoxscoreTeamObject$`penaltyBox`, function(x) {
          penaltyBoxObject <- BigDecimal$new()
          penaltyBoxObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          penaltyBoxObject
        })
      }
      if (!is.null(GameBoxscoreTeamObject$`coaches`)) {
        self$`coaches` <- lapply(GameBoxscoreTeamObject$`coaches`, function(x) {
          coachesObject <- GameBoxscoreTeamCoaches$new()
          coachesObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          coachesObject
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "team": %s,
           "teamStats": %s,
           "players": %s,
           "goalies": [%s],
           "skaters": [%s],
           "onIce": [%s],
           "onIcePlus": [%s],
           "scratches": [%s],
           "penaltyBox": [%s],
           "coaches": [%s]
        }',
        self$`team`$toJSON(),
        self$`teamStats`$toJSON(),
        self$`players`$toJSON(),
        lapply(self$`goalies`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`skaters`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`onIce`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`onIcePlus`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`scratches`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`penaltyBox`, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$`coaches`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(GameBoxscoreTeamJson) {
      GameBoxscoreTeamObject <- jsonlite::fromJSON(GameBoxscoreTeamJson)
      GameBoxscoreTeamTeamObject <- GameBoxscoreTeamTeam$new()
      self$`team` <- GameBoxscoreTeamTeamObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$team, auto_unbox = TRUE))
      GameBoxscoreTeamTeamStatsObject <- GameBoxscoreTeamTeamStats$new()
      self$`teamStats` <- GameBoxscoreTeamTeamStatsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$teamStats, auto_unbox = TRUE))
      GameBoxscoreTeamPlayersObject <- GameBoxscoreTeamPlayers$new()
      self$`players` <- GameBoxscoreTeamPlayersObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamObject$players, auto_unbox = TRUE))
      self$`goalies` <- lapply(GameBoxscoreTeamObject$`goalies`, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`skaters` <- lapply(GameBoxscoreTeamObject$`skaters`, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`onIce` <- lapply(GameBoxscoreTeamObject$`onIce`, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`onIcePlus` <- lapply(GameBoxscoreTeamObject$`onIcePlus`, function(x) GameBoxscoreTeamOnIcePlus$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`scratches` <- lapply(GameBoxscoreTeamObject$`scratches`, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`penaltyBox` <- lapply(GameBoxscoreTeamObject$`penaltyBox`, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`coaches` <- lapply(GameBoxscoreTeamObject$`coaches`, function(x) GameBoxscoreTeamCoaches$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)
