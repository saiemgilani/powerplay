# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameContentEditorial Class
#'
#' @field preview 
#' @field articles 
#' @field recap 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameContentEditorial <- R6::R6Class(
  'GameContentEditorial',
  public = list(
    preview = NULL,
    articles = NULL,
    recap = NULL,
    initialize = function(preview, articles, recap){
      if (!missing(preview)) {
        stopifnot(R6::is.R6(preview))
        self$preview <- preview
      }
      if (!missing(articles)) {
        stopifnot(R6::is.R6(articles))
        self$articles <- articles
      }
      if (!missing(recap)) {
        stopifnot(R6::is.R6(recap))
        self$recap <- recap
      }
    },
    toJSON = function() {
      GameContentEditorialObject <- list()
      if (!is.null(self$preview)) {
        GameContentEditorialObject[['preview']] <- self$preview$toJSON()
      }
      if (!is.null(self$articles)) {
        GameContentEditorialObject[['articles']] <- self$articles$toJSON()
      }
      if (!is.null(self$recap)) {
        GameContentEditorialObject[['recap']] <- self$recap$toJSON()
      }

      GameContentEditorialObject
    },
    fromJSON = function(GameContentEditorialJson) {
      GameContentEditorialObject <- jsonlite::fromJSON(GameContentEditorialJson)
      if (!is.null(GameContentEditorialObject$preview)) {
        previewObject <- GameEditorials$new()
        previewObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$preview, auto_unbox = TRUE))
        self$preview <- previewObject
      }
      if (!is.null(GameContentEditorialObject$articles)) {
        articlesObject <- GameEditorials$new()
        articlesObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$articles, auto_unbox = TRUE))
        self$articles <- articlesObject
      }
      if (!is.null(GameContentEditorialObject$recap)) {
        recapObject <- GameEditorials$new()
        recapObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$recap, auto_unbox = TRUE))
        self$recap <- recapObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "preview": %s,
           "articles": %s,
           "recap": %s
        }',
        self$preview$toJSON(),
        self$articles$toJSON(),
        self$recap$toJSON()
      )
    },
    fromJSONString = function(GameContentEditorialJson) {
      GameContentEditorialObject <- jsonlite::fromJSON(GameContentEditorialJson)
      GameEditorialsObject <- GameEditorials$new()
      self$preview <- GameEditorialsObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$preview, auto_unbox = TRUE))
      GameEditorialsObject <- GameEditorials$new()
      self$articles <- GameEditorialsObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$articles, auto_unbox = TRUE))
      GameEditorialsObject <- GameEditorials$new()
      self$recap <- GameEditorialsObject$fromJSON(jsonlite::toJSON(GameContentEditorialObject$recap, auto_unbox = TRUE))
    }
  )
)