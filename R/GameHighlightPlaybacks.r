# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameHighlightPlaybacks Class
#'
#' @field name 
#' @field width 
#' @field height 
#' @field url 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameHighlightPlaybacks <- R6::R6Class(
  'GameHighlightPlaybacks',
  public = list(
    name = NULL,
    width = NULL,
    height = NULL,
    url = NULL,
    initialize = function(name, width, height, url){
      if (!missing(name)) {
        stopifnot(is.character(name), length(name) == 1)
        self$name <- name
      }
      if (!missing(width)) {
        stopifnot(is.character(width), length(width) == 1)
        self$width <- width
      }
      if (!missing(height)) {
        stopifnot(is.character(height), length(height) == 1)
        self$height <- height
      }
      if (!missing(url)) {
        stopifnot(is.character(url), length(url) == 1)
        self$url <- url
      }
    },
    toJSON = function() {
      GameHighlightPlaybacksObject <- list()
      if (!is.null(self$name)) {
        GameHighlightPlaybacksObject[['name']] <- self$name
      }
      if (!is.null(self$width)) {
        GameHighlightPlaybacksObject[['width']] <- self$width
      }
      if (!is.null(self$height)) {
        GameHighlightPlaybacksObject[['height']] <- self$height
      }
      if (!is.null(self$url)) {
        GameHighlightPlaybacksObject[['url']] <- self$url
      }

      GameHighlightPlaybacksObject
    },
    fromJSON = function(GameHighlightPlaybacksJson) {
      GameHighlightPlaybacksObject <- jsonlite::fromJSON(GameHighlightPlaybacksJson)
      if (!is.null(GameHighlightPlaybacksObject$name)) {
        self$name <- GameHighlightPlaybacksObject$name
      }
      if (!is.null(GameHighlightPlaybacksObject$width)) {
        self$width <- GameHighlightPlaybacksObject$width
      }
      if (!is.null(GameHighlightPlaybacksObject$height)) {
        self$height <- GameHighlightPlaybacksObject$height
      }
      if (!is.null(GameHighlightPlaybacksObject$url)) {
        self$url <- GameHighlightPlaybacksObject$url
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "name": %s,
           "width": %s,
           "height": %s,
           "url": %s
        }',
        self$name,
        self$width,
        self$height,
        self$url
      )
    },
    fromJSONString = function(GameHighlightPlaybacksJson) {
      GameHighlightPlaybacksObject <- jsonlite::fromJSON(GameHighlightPlaybacksJson)
      self$name <- GameHighlightPlaybacksObject$name
      self$width <- GameHighlightPlaybacksObject$width
      self$height <- GameHighlightPlaybacksObject$height
      self$url <- GameHighlightPlaybacksObject$url
    }
  )
)