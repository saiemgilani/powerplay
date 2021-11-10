# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameBoxscoreTeamTeamStatsTeamSkaterStats Class
#'
#' @field goals 
#' @field pim 
#' @field shots 
#' @field powerPlayPercentage 
#' @field powerPlayGoals 
#' @field powerPlayOpportunities 
#' @field faceOffWinPercentage 
#' @field blocked 
#' @field takeaways 
#' @field giveaways 
#' @field hits 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameBoxscoreTeamTeamStatsTeamSkaterStats <- R6::R6Class(
  'GameBoxscoreTeamTeamStatsTeamSkaterStats',
  public = list(
    goals = NULL,
    pim = NULL,
    shots = NULL,
    powerPlayPercentage = NULL,
    powerPlayGoals = NULL,
    powerPlayOpportunities = NULL,
    faceOffWinPercentage = NULL,
    blocked = NULL,
    takeaways = NULL,
    giveaways = NULL,
    hits = NULL,
    initialize = function(goals, pim, shots, powerPlayPercentage, powerPlayGoals, powerPlayOpportunities, faceOffWinPercentage, blocked, takeaways, giveaways, hits){
      if (!missing(goals)) {
        stopifnot(is.numeric(goals), length(goals) == 1)
        stopifnot(R6::is.R6(goals))
        self$goals <- goals
      }
      if (!missing(pim)) {
        stopifnot(is.numeric(pim), length(pim) == 1)
        stopifnot(R6::is.R6(pim))
        self$pim <- pim
      }
      if (!missing(shots)) {
        stopifnot(is.numeric(shots), length(shots) == 1)
        stopifnot(R6::is.R6(shots))
        self$shots <- shots
      }
      if (!missing(powerPlayPercentage)) {
        stopifnot(is.character(powerPlayPercentage), length(powerPlayPercentage) == 1)
        self$powerPlayPercentage <- powerPlayPercentage
      }
      if (!missing(powerPlayGoals)) {
        stopifnot(is.numeric(powerPlayGoals), length(powerPlayGoals) == 1)
        stopifnot(R6::is.R6(powerPlayGoals))
        self$powerPlayGoals <- powerPlayGoals
      }
      if (!missing(powerPlayOpportunities)) {
        stopifnot(is.numeric(powerPlayOpportunities), length(powerPlayOpportunities) == 1)
        stopifnot(R6::is.R6(powerPlayOpportunities))
        self$powerPlayOpportunities <- powerPlayOpportunities
      }
      if (!missing(faceOffWinPercentage)) {
        stopifnot(is.character(faceOffWinPercentage), length(faceOffWinPercentage) == 1)
        self$faceOffWinPercentage <- faceOffWinPercentage
      }
      if (!missing(blocked)) {
        stopifnot(is.numeric(blocked), length(blocked) == 1)
        stopifnot(R6::is.R6(blocked))
        self$blocked <- blocked
      }
      if (!missing(takeaways)) {
        stopifnot(is.numeric(takeaways), length(takeaways) == 1)
        stopifnot(R6::is.R6(takeaways))
        self$takeaways <- takeaways
      }
      if (!missing(giveaways)) {
        stopifnot(is.numeric(giveaways), length(giveaways) == 1)
        stopifnot(R6::is.R6(giveaways))
        self$giveaways <- giveaways
      }
      if (!missing(hits)) {
        stopifnot(is.numeric(hits), length(hits) == 1)
        stopifnot(R6::is.R6(hits))
        self$hits <- hits
      }
    },
    toJSON = function() {
      GameBoxscoreTeamTeamStatsTeamSkaterStatsObject <- list()
      if (!is.null(self$goals)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['goals']] <- self$goals$toJSON()
      }
      if (!is.null(self$pim)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['pim']] <- self$pim$toJSON()
      }
      if (!is.null(self$shots)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['shots']] <- self$shots$toJSON()
      }
      if (!is.null(self$powerPlayPercentage)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['powerPlayPercentage']] <- self$powerPlayPercentage
      }
      if (!is.null(self$powerPlayGoals)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['powerPlayGoals']] <- self$powerPlayGoals$toJSON()
      }
      if (!is.null(self$powerPlayOpportunities)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['powerPlayOpportunities']] <- self$powerPlayOpportunities$toJSON()
      }
      if (!is.null(self$faceOffWinPercentage)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['faceOffWinPercentage']] <- self$faceOffWinPercentage
      }
      if (!is.null(self$blocked)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['blocked']] <- self$blocked$toJSON()
      }
      if (!is.null(self$takeaways)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['takeaways']] <- self$takeaways$toJSON()
      }
      if (!is.null(self$giveaways)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['giveaways']] <- self$giveaways$toJSON()
      }
      if (!is.null(self$hits)) {
        GameBoxscoreTeamTeamStatsTeamSkaterStatsObject[['hits']] <- self$hits$toJSON()
      }

      GameBoxscoreTeamTeamStatsTeamSkaterStatsObject
    },
    fromJSON = function(GameBoxscoreTeamTeamStatsTeamSkaterStatsJson) {
      GameBoxscoreTeamTeamStatsTeamSkaterStatsObject <- jsonlite::fromJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsJson)
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$goals)) {
        goalsObject <- BigDecimal$new()
        goalsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$goals, auto_unbox = TRUE))
        self$goals <- goalsObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$pim)) {
        pimObject <- BigDecimal$new()
        pimObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$pim, auto_unbox = TRUE))
        self$pim <- pimObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$shots)) {
        shotsObject <- BigDecimal$new()
        shotsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$shots, auto_unbox = TRUE))
        self$shots <- shotsObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayPercentage)) {
        self$powerPlayPercentage <- GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayPercentage
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayGoals)) {
        powerPlayGoalsObject <- BigDecimal$new()
        powerPlayGoalsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayGoals, auto_unbox = TRUE))
        self$powerPlayGoals <- powerPlayGoalsObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayOpportunities)) {
        powerPlayOpportunitiesObject <- BigDecimal$new()
        powerPlayOpportunitiesObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayOpportunities, auto_unbox = TRUE))
        self$powerPlayOpportunities <- powerPlayOpportunitiesObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$faceOffWinPercentage)) {
        self$faceOffWinPercentage <- GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$faceOffWinPercentage
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$blocked)) {
        blockedObject <- BigDecimal$new()
        blockedObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$blocked, auto_unbox = TRUE))
        self$blocked <- blockedObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$takeaways)) {
        takeawaysObject <- BigDecimal$new()
        takeawaysObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$takeaways, auto_unbox = TRUE))
        self$takeaways <- takeawaysObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$giveaways)) {
        giveawaysObject <- BigDecimal$new()
        giveawaysObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$giveaways, auto_unbox = TRUE))
        self$giveaways <- giveawaysObject
      }
      if (!is.null(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$hits)) {
        hitsObject <- BigDecimal$new()
        hitsObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$hits, auto_unbox = TRUE))
        self$hits <- hitsObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "goals": %s,
           "pim": %s,
           "shots": %s,
           "powerPlayPercentage": %s,
           "powerPlayGoals": %s,
           "powerPlayOpportunities": %s,
           "faceOffWinPercentage": %s,
           "blocked": %s,
           "takeaways": %s,
           "giveaways": %s,
           "hits": %s
        }',
        self$goals$toJSON(),
        self$pim$toJSON(),
        self$shots$toJSON(),
        self$powerPlayPercentage,
        self$powerPlayGoals$toJSON(),
        self$powerPlayOpportunities$toJSON(),
        self$faceOffWinPercentage,
        self$blocked$toJSON(),
        self$takeaways$toJSON(),
        self$giveaways$toJSON(),
        self$hits$toJSON()
      )
    },
    fromJSONString = function(GameBoxscoreTeamTeamStatsTeamSkaterStatsJson) {
      GameBoxscoreTeamTeamStatsTeamSkaterStatsObject <- jsonlite::fromJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsJson)
      BigDecimalObject <- BigDecimal$new()
      self$goals <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$goals, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$pim <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$pim, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$shots <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$shots, auto_unbox = TRUE))
      self$powerPlayPercentage <- GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayPercentage
      BigDecimalObject <- BigDecimal$new()
      self$powerPlayGoals <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayGoals, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$powerPlayOpportunities <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$powerPlayOpportunities, auto_unbox = TRUE))
      self$faceOffWinPercentage <- GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$faceOffWinPercentage
      BigDecimalObject <- BigDecimal$new()
      self$blocked <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$blocked, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$takeaways <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$takeaways, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$giveaways <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$giveaways, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$hits <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamTeamStatsTeamSkaterStatsObject$hits, auto_unbox = TRUE))
    }
  )
)