# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' StatTypes Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
StatTypes <- R6::R6Class(
  'StatTypes',
  public = list(
    initialize = function(){
    },
    toJSON = function() {
      StatTypesObject <- list()

      StatTypesObject
    },
    fromJSON = function(StatTypesJson) {
      StatTypesObject <- jsonlite::fromJSON(StatTypesJson)
    },
    toJSONString = function() {
       sprintf(
        '{
        }',
      )
    },
    fromJSONString = function(StatTypesJson) {
      StatTypesObject <- jsonlite::fromJSON(StatTypesJson)
    }
  )
)
