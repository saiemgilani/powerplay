# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Photo Class
#'
#' @field title 
#' @field altText 
#' @field cuts 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Photo <- R6::R6Class(
  'Photo',
  public = list(
    `title` = NULL,
    `altText` = NULL,
    `cuts` = NULL,
    initialize = function(`title`, `altText`, `cuts`){
      if (!missing(`title`)) {
        stopifnot(is.character(`title`), length(`title`) == 1)
        self$`title` <- `title`
      }
      if (!missing(`altText`)) {
        stopifnot(is.character(`altText`), length(`altText`) == 1)
        self$`altText` <- `altText`
      }
      if (!missing(`cuts`)) {
        stopifnot(R6::is.R6(`cuts`))
        self$`cuts` <- `cuts`
      }
    },
    toJSON = function() {
      PhotoObject <- list()
      if (!is.null(self$`title`)) {
        PhotoObject[['title']] <- self$`title`
      }
      if (!is.null(self$`altText`)) {
        PhotoObject[['altText']] <- self$`altText`
      }
      if (!is.null(self$`cuts`)) {
        PhotoObject[['cuts']] <- self$`cuts`$toJSON()
      }

      PhotoObject
    },
    fromJSON = function(PhotoJson) {
      PhotoObject <- jsonlite::fromJSON(PhotoJson)
      if (!is.null(PhotoObject$`title`)) {
        self$`title` <- PhotoObject$`title`
      }
      if (!is.null(PhotoObject$`altText`)) {
        self$`altText` <- PhotoObject$`altText`
      }
      if (!is.null(PhotoObject$`cuts`)) {
        cutsObject <- PhotoCuts$new()
        cutsObject$fromJSON(jsonlite::toJSON(PhotoObject$cuts, auto_unbox = TRUE))
        self$`cuts` <- cutsObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "title": %s,
           "altText": %s,
           "cuts": %s
        }',
        self$`title`,
        self$`altText`,
        self$`cuts`$toJSON()
      )
    },
    fromJSONString = function(PhotoJson) {
      PhotoObject <- jsonlite::fromJSON(PhotoJson)
      self$`title` <- PhotoObject$`title`
      self$`altText` <- PhotoObject$`altText`
      PhotoCutsObject <- PhotoCuts$new()
      self$`cuts` <- PhotoCutsObject$fromJSON(jsonlite::toJSON(PhotoObject$cuts, auto_unbox = TRUE))
    }
  )
)
