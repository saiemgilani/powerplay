# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameEditorial Class
#'
#' @field type 
#' @field state 
#' @field date 
#' @field id 
#' @field headline 
#' @field subhead 
#' @field seoTitle 
#' @field seoDescription 
#' @field seoKeywords 
#' @field slug 
#' @field commenting 
#' @field tagline 
#' @field tokenData 
#' @field contributor 
#' @field keywordsDisplay 
#' @field keywordsAll 
#' @field approval 
#' @field url 
#' @field dataURI 
#' @field primaryKeyword 
#' @field media 
#' @field preview 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameEditorial <- R6::R6Class(
  'GameEditorial',
  public = list(
    type = NULL,
    state = NULL,
    date = NULL,
    id = NULL,
    headline = NULL,
    subhead = NULL,
    seoTitle = NULL,
    seoDescription = NULL,
    seoKeywords = NULL,
    slug = NULL,
    commenting = NULL,
    tagline = NULL,
    tokenData = NULL,
    contributor = NULL,
    keywordsDisplay = NULL,
    keywordsAll = NULL,
    approval = NULL,
    url = NULL,
    dataURI = NULL,
    primaryKeyword = NULL,
    media = NULL,
    preview = NULL,
    initialize = function(type, state, date, id, headline, subhead, seoTitle, seoDescription, seoKeywords, slug, commenting, tagline, tokenData, contributor, keywordsDisplay, keywordsAll, approval, url, dataURI, primaryKeyword, media, preview){
      if (!missing(type)) {
        stopifnot(is.character(type), length(type) == 1)
        self$type <- type
      }
      if (!missing(state)) {
        stopifnot(is.character(state), length(state) == 1)
        self$state <- state
      }
      if (!missing(date)) {
        stopifnot(is.character(date), length(date) == 1)
        self$date <- date
      }
      if (!missing(id)) {
        stopifnot(is.character(id), length(id) == 1)
        self$id <- id
      }
      if (!missing(headline)) {
        stopifnot(is.character(headline), length(headline) == 1)
        self$headline <- headline
      }
      if (!missing(subhead)) {
        stopifnot(is.character(subhead), length(subhead) == 1)
        self$subhead <- subhead
      }
      if (!missing(seoTitle)) {
        stopifnot(is.character(seoTitle), length(seoTitle) == 1)
        self$seoTitle <- seoTitle
      }
      if (!missing(seoDescription)) {
        stopifnot(is.character(seoDescription), length(seoDescription) == 1)
        self$seoDescription <- seoDescription
      }
      if (!missing(seoKeywords)) {
        stopifnot(is.character(seoKeywords), length(seoKeywords) == 1)
        self$seoKeywords <- seoKeywords
      }
      if (!missing(slug)) {
        stopifnot(is.character(slug), length(slug) == 1)
        self$slug <- slug
      }
      if (!missing(commenting)) {
        self$commenting <- commenting
      }
      if (!missing(tagline)) {
        stopifnot(is.character(tagline), length(tagline) == 1)
        self$tagline <- tagline
      }
      if (!missing(tokenData)) {
        stopifnot(R6::is.R6(tokenData))
        self$tokenData <- tokenData
      }
      if (!missing(contributor)) {
        stopifnot(R6::is.R6(contributor))
        self$contributor <- contributor
      }
      if (!missing(keywordsDisplay)) {
        stopifnot(is.list(keywordsDisplay), length(keywordsDisplay) != 0)
        lapply(keywordsDisplay, function(x) stopifnot(R6::is.R6(x)))
        self$keywordsDisplay <- keywordsDisplay
      }
      if (!missing(keywordsAll)) {
        stopifnot(is.list(keywordsAll), length(keywordsAll) != 0)
        lapply(keywordsAll, function(x) stopifnot(R6::is.R6(x)))
        self$keywordsAll <- keywordsAll
      }
      if (!missing(approval)) {
        stopifnot(is.character(approval), length(approval) == 1)
        self$approval <- approval
      }
      if (!missing(url)) {
        stopifnot(is.character(url), length(url) == 1)
        self$url <- url
      }
      if (!missing(dataURI)) {
        stopifnot(is.character(dataURI), length(dataURI) == 1)
        self$dataURI <- dataURI
      }
      if (!missing(primaryKeyword)) {
        stopifnot(R6::is.R6(primaryKeyword))
        self$primaryKeyword <- primaryKeyword
      }
      if (!missing(media)) {
        stopifnot(R6::is.R6(media))
        self$media <- media
      }
      if (!missing(preview)) {
        stopifnot(is.character(preview), length(preview) == 1)
        self$preview <- preview
      }
    },
    toJSON = function() {
      GameEditorialObject <- list()
      if (!is.null(self$type)) {
        GameEditorialObject[['type']] <- self$type
      }
      if (!is.null(self$state)) {
        GameEditorialObject[['state']] <- self$state
      }
      if (!is.null(self$date)) {
        GameEditorialObject[['date']] <- self$date
      }
      if (!is.null(self$id)) {
        GameEditorialObject[['id']] <- self$id
      }
      if (!is.null(self$headline)) {
        GameEditorialObject[['headline']] <- self$headline
      }
      if (!is.null(self$subhead)) {
        GameEditorialObject[['subhead']] <- self$subhead
      }
      if (!is.null(self$seoTitle)) {
        GameEditorialObject[['seoTitle']] <- self$seoTitle
      }
      if (!is.null(self$seoDescription)) {
        GameEditorialObject[['seoDescription']] <- self$seoDescription
      }
      if (!is.null(self$seoKeywords)) {
        GameEditorialObject[['seoKeywords']] <- self$seoKeywords
      }
      if (!is.null(self$slug)) {
        GameEditorialObject[['slug']] <- self$slug
      }
      if (!is.null(self$commenting)) {
        GameEditorialObject[['commenting']] <- self$commenting
      }
      if (!is.null(self$tagline)) {
        GameEditorialObject[['tagline']] <- self$tagline
      }
      if (!is.null(self$tokenData)) {
        GameEditorialObject[['tokenData']] <- self$tokenData$toJSON()
      }
      if (!is.null(self$contributor)) {
        GameEditorialObject[['contributor']] <- self$contributor$toJSON()
      }
      if (!is.null(self$keywordsDisplay)) {
        GameEditorialObject[['keywordsDisplay']] <- lapply(self$keywordsDisplay, function(x) x$toJSON())
      }
      if (!is.null(self$keywordsAll)) {
        GameEditorialObject[['keywordsAll']] <- lapply(self$keywordsAll, function(x) x$toJSON())
      }
      if (!is.null(self$approval)) {
        GameEditorialObject[['approval']] <- self$approval
      }
      if (!is.null(self$url)) {
        GameEditorialObject[['url']] <- self$url
      }
      if (!is.null(self$dataURI)) {
        GameEditorialObject[['dataURI']] <- self$dataURI
      }
      if (!is.null(self$primaryKeyword)) {
        GameEditorialObject[['primaryKeyword']] <- self$primaryKeyword$toJSON()
      }
      if (!is.null(self$media)) {
        GameEditorialObject[['media']] <- self$media$toJSON()
      }
      if (!is.null(self$preview)) {
        GameEditorialObject[['preview']] <- self$preview
      }

      GameEditorialObject
    },
    fromJSON = function(GameEditorialJson) {
      GameEditorialObject <- jsonlite::fromJSON(GameEditorialJson)
      if (!is.null(GameEditorialObject$type)) {
        self$type <- GameEditorialObject$type
      }
      if (!is.null(GameEditorialObject$state)) {
        self$state <- GameEditorialObject$state
      }
      if (!is.null(GameEditorialObject$date)) {
        self$date <- GameEditorialObject$date
      }
      if (!is.null(GameEditorialObject$id)) {
        self$id <- GameEditorialObject$id
      }
      if (!is.null(GameEditorialObject$headline)) {
        self$headline <- GameEditorialObject$headline
      }
      if (!is.null(GameEditorialObject$subhead)) {
        self$subhead <- GameEditorialObject$subhead
      }
      if (!is.null(GameEditorialObject$seoTitle)) {
        self$seoTitle <- GameEditorialObject$seoTitle
      }
      if (!is.null(GameEditorialObject$seoDescription)) {
        self$seoDescription <- GameEditorialObject$seoDescription
      }
      if (!is.null(GameEditorialObject$seoKeywords)) {
        self$seoKeywords <- GameEditorialObject$seoKeywords
      }
      if (!is.null(GameEditorialObject$slug)) {
        self$slug <- GameEditorialObject$slug
      }
      if (!is.null(GameEditorialObject$commenting)) {
        self$commenting <- GameEditorialObject$commenting
      }
      if (!is.null(GameEditorialObject$tagline)) {
        self$tagline <- GameEditorialObject$tagline
      }
      if (!is.null(GameEditorialObject$tokenData)) {
        tokenDataObject <- GameEditorialTokenData$new()
        tokenDataObject$fromJSON(jsonlite::toJSON(GameEditorialObject$tokenData, auto_unbox = TRUE))
        self$tokenData <- tokenDataObject
      }
      if (!is.null(GameEditorialObject$contributor)) {
        contributorObject <- GameEditorialContributor$new()
        contributorObject$fromJSON(jsonlite::toJSON(GameEditorialObject$contributor, auto_unbox = TRUE))
        self$contributor <- contributorObject
      }
      if (!is.null(GameEditorialObject$keywordsDisplay)) {
        self$keywordsDisplay <- lapply(GameEditorialObject$keywordsDisplay, function(x) {
          keywordsDisplayObject <- GameEditorialKeyword$new()
          keywordsDisplayObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          keywordsDisplayObject
        })
      }
      if (!is.null(GameEditorialObject$keywordsAll)) {
        self$keywordsAll <- lapply(GameEditorialObject$keywordsAll, function(x) {
          keywordsAllObject <- GameEditorialKeyword$new()
          keywordsAllObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          keywordsAllObject
        })
      }
      if (!is.null(GameEditorialObject$approval)) {
        self$approval <- GameEditorialObject$approval
      }
      if (!is.null(GameEditorialObject$url)) {
        self$url <- GameEditorialObject$url
      }
      if (!is.null(GameEditorialObject$dataURI)) {
        self$dataURI <- GameEditorialObject$dataURI
      }
      if (!is.null(GameEditorialObject$primaryKeyword)) {
        primaryKeywordObject <- GameEditorialKeyword$new()
        primaryKeywordObject$fromJSON(jsonlite::toJSON(GameEditorialObject$primaryKeyword, auto_unbox = TRUE))
        self$primaryKeyword <- primaryKeywordObject
      }
      if (!is.null(GameEditorialObject$media)) {
        mediaObject <- GameEditorialMedia$new()
        mediaObject$fromJSON(jsonlite::toJSON(GameEditorialObject$media, auto_unbox = TRUE))
        self$media <- mediaObject
      }
      if (!is.null(GameEditorialObject$preview)) {
        self$preview <- GameEditorialObject$preview
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "type": %s,
           "state": %s,
           "date": %s,
           "id": %s,
           "headline": %s,
           "subhead": %s,
           "seoTitle": %s,
           "seoDescription": %s,
           "seoKeywords": %s,
           "slug": %s,
           "commenting": %s,
           "tagline": %s,
           "tokenData": %s,
           "contributor": %s,
           "keywordsDisplay": [%s],
           "keywordsAll": [%s],
           "approval": %s,
           "url": %s,
           "dataURI": %s,
           "primaryKeyword": %s,
           "media": %s,
           "preview": %s
        }',
        self$type,
        self$state,
        self$date,
        self$id,
        self$headline,
        self$subhead,
        self$seoTitle,
        self$seoDescription,
        self$seoKeywords,
        self$slug,
        self$commenting,
        self$tagline,
        self$tokenData$toJSON(),
        self$contributor$toJSON(),
        lapply(self$keywordsDisplay, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$keywordsAll, function(x) paste(x$toJSON(), sep=",")),
        self$approval,
        self$url,
        self$dataURI,
        self$primaryKeyword$toJSON(),
        self$media$toJSON(),
        self$preview
      )
    },
    fromJSONString = function(GameEditorialJson) {
      GameEditorialObject <- jsonlite::fromJSON(GameEditorialJson)
      self$type <- GameEditorialObject$type
      self$state <- GameEditorialObject$state
      self$date <- GameEditorialObject$date
      self$id <- GameEditorialObject$id
      self$headline <- GameEditorialObject$headline
      self$subhead <- GameEditorialObject$subhead
      self$seoTitle <- GameEditorialObject$seoTitle
      self$seoDescription <- GameEditorialObject$seoDescription
      self$seoKeywords <- GameEditorialObject$seoKeywords
      self$slug <- GameEditorialObject$slug
      self$commenting <- GameEditorialObject$commenting
      self$tagline <- GameEditorialObject$tagline
      GameEditorialTokenDataObject <- GameEditorialTokenData$new()
      self$tokenData <- GameEditorialTokenDataObject$fromJSON(jsonlite::toJSON(GameEditorialObject$tokenData, auto_unbox = TRUE))
      GameEditorialContributorObject <- GameEditorialContributor$new()
      self$contributor <- GameEditorialContributorObject$fromJSON(jsonlite::toJSON(GameEditorialObject$contributor, auto_unbox = TRUE))
      self$keywordsDisplay <- lapply(GameEditorialObject$keywordsDisplay, function(x) GameEditorialKeyword$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$keywordsAll <- lapply(GameEditorialObject$keywordsAll, function(x) GameEditorialKeyword$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$approval <- GameEditorialObject$approval
      self$url <- GameEditorialObject$url
      self$dataURI <- GameEditorialObject$dataURI
      GameEditorialKeywordObject <- GameEditorialKeyword$new()
      self$primaryKeyword <- GameEditorialKeywordObject$fromJSON(jsonlite::toJSON(GameEditorialObject$primaryKeyword, auto_unbox = TRUE))
      GameEditorialMediaObject <- GameEditorialMedia$new()
      self$media <- GameEditorialMediaObject$fromJSON(jsonlite::toJSON(GameEditorialObject$media, auto_unbox = TRUE))
      self$preview <- GameEditorialObject$preview
    }
  )
)