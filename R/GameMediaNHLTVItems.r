# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameMediaNHLTVItems Class
#'
#' @field guid 
#' @field mediaState 
#' @field mediaPlaybackId 
#' @field mediaFeedType 
#' @field callLetters 
#' @field eventId 
#' @field language 
#' @field freeGame 
#' @field feedName 
#' @field gamePlus 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameMediaNHLTVItems <- R6::R6Class(
  'GameMediaNHLTVItems',
  public = list(
    guid = NULL,
    mediaState = NULL,
    mediaPlaybackId = NULL,
    mediaFeedType = NULL,
    callLetters = NULL,
    eventId = NULL,
    language = NULL,
    freeGame = NULL,
    feedName = NULL,
    gamePlus = NULL,
    initialize = function(guid, mediaState, mediaPlaybackId, mediaFeedType, callLetters, eventId, language, freeGame, feedName, gamePlus){
      if (!missing(guid)) {
        stopifnot(is.character(guid), length(guid) == 1)
        self$guid <- guid
      }
      if (!missing(mediaState)) {
        stopifnot(is.character(mediaState), length(mediaState) == 1)
        self$mediaState <- mediaState
      }
      if (!missing(mediaPlaybackId)) {
        stopifnot(is.character(mediaPlaybackId), length(mediaPlaybackId) == 1)
        self$mediaPlaybackId <- mediaPlaybackId
      }
      if (!missing(mediaFeedType)) {
        stopifnot(is.character(mediaFeedType), length(mediaFeedType) == 1)
        self$mediaFeedType <- mediaFeedType
      }
      if (!missing(callLetters)) {
        stopifnot(is.character(callLetters), length(callLetters) == 1)
        self$callLetters <- callLetters
      }
      if (!missing(eventId)) {
        stopifnot(is.character(eventId), length(eventId) == 1)
        self$eventId <- eventId
      }
      if (!missing(language)) {
        stopifnot(is.character(language), length(language) == 1)
        self$language <- language
      }
      if (!missing(freeGame)) {
        self$freeGame <- freeGame
      }
      if (!missing(feedName)) {
        stopifnot(is.character(feedName), length(feedName) == 1)
        self$feedName <- feedName
      }
      if (!missing(gamePlus)) {
        self$gamePlus <- gamePlus
      }
    },
    toJSON = function() {
      GameMediaNHLTVItemsObject <- list()
      if (!is.null(self$guid)) {
        GameMediaNHLTVItemsObject[['guid']] <- self$guid
      }
      if (!is.null(self$mediaState)) {
        GameMediaNHLTVItemsObject[['mediaState']] <- self$mediaState
      }
      if (!is.null(self$mediaPlaybackId)) {
        GameMediaNHLTVItemsObject[['mediaPlaybackId']] <- self$mediaPlaybackId
      }
      if (!is.null(self$mediaFeedType)) {
        GameMediaNHLTVItemsObject[['mediaFeedType']] <- self$mediaFeedType
      }
      if (!is.null(self$callLetters)) {
        GameMediaNHLTVItemsObject[['callLetters']] <- self$callLetters
      }
      if (!is.null(self$eventId)) {
        GameMediaNHLTVItemsObject[['eventId']] <- self$eventId
      }
      if (!is.null(self$language)) {
        GameMediaNHLTVItemsObject[['language']] <- self$language
      }
      if (!is.null(self$freeGame)) {
        GameMediaNHLTVItemsObject[['freeGame']] <- self$freeGame
      }
      if (!is.null(self$feedName)) {
        GameMediaNHLTVItemsObject[['feedName']] <- self$feedName
      }
      if (!is.null(self$gamePlus)) {
        GameMediaNHLTVItemsObject[['gamePlus']] <- self$gamePlus
      }

      GameMediaNHLTVItemsObject
    },
    fromJSON = function(GameMediaNHLTVItemsJson) {
      GameMediaNHLTVItemsObject <- jsonlite::fromJSON(GameMediaNHLTVItemsJson)
      if (!is.null(GameMediaNHLTVItemsObject$guid)) {
        self$guid <- GameMediaNHLTVItemsObject$guid
      }
      if (!is.null(GameMediaNHLTVItemsObject$mediaState)) {
        self$mediaState <- GameMediaNHLTVItemsObject$mediaState
      }
      if (!is.null(GameMediaNHLTVItemsObject$mediaPlaybackId)) {
        self$mediaPlaybackId <- GameMediaNHLTVItemsObject$mediaPlaybackId
      }
      if (!is.null(GameMediaNHLTVItemsObject$mediaFeedType)) {
        self$mediaFeedType <- GameMediaNHLTVItemsObject$mediaFeedType
      }
      if (!is.null(GameMediaNHLTVItemsObject$callLetters)) {
        self$callLetters <- GameMediaNHLTVItemsObject$callLetters
      }
      if (!is.null(GameMediaNHLTVItemsObject$eventId)) {
        self$eventId <- GameMediaNHLTVItemsObject$eventId
      }
      if (!is.null(GameMediaNHLTVItemsObject$language)) {
        self$language <- GameMediaNHLTVItemsObject$language
      }
      if (!is.null(GameMediaNHLTVItemsObject$freeGame)) {
        self$freeGame <- GameMediaNHLTVItemsObject$freeGame
      }
      if (!is.null(GameMediaNHLTVItemsObject$feedName)) {
        self$feedName <- GameMediaNHLTVItemsObject$feedName
      }
      if (!is.null(GameMediaNHLTVItemsObject$gamePlus)) {
        self$gamePlus <- GameMediaNHLTVItemsObject$gamePlus
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "guid": %s,
           "mediaState": %s,
           "mediaPlaybackId": %s,
           "mediaFeedType": %s,
           "callLetters": %s,
           "eventId": %s,
           "language": %s,
           "freeGame": %s,
           "feedName": %s,
           "gamePlus": %s
        }',
        self$guid,
        self$mediaState,
        self$mediaPlaybackId,
        self$mediaFeedType,
        self$callLetters,
        self$eventId,
        self$language,
        self$freeGame,
        self$feedName,
        self$gamePlus
      )
    },
    fromJSONString = function(GameMediaNHLTVItemsJson) {
      GameMediaNHLTVItemsObject <- jsonlite::fromJSON(GameMediaNHLTVItemsJson)
      self$guid <- GameMediaNHLTVItemsObject$guid
      self$mediaState <- GameMediaNHLTVItemsObject$mediaState
      self$mediaPlaybackId <- GameMediaNHLTVItemsObject$mediaPlaybackId
      self$mediaFeedType <- GameMediaNHLTVItemsObject$mediaFeedType
      self$callLetters <- GameMediaNHLTVItemsObject$callLetters
      self$eventId <- GameMediaNHLTVItemsObject$eventId
      self$language <- GameMediaNHLTVItemsObject$language
      self$freeGame <- GameMediaNHLTVItemsObject$freeGame
      self$feedName <- GameMediaNHLTVItemsObject$feedName
      self$gamePlus <- GameMediaNHLTVItemsObject$gamePlus
    }
  )
)