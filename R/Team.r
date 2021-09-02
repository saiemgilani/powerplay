# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Team Class
#'
#' @field id 
#' @field name 
#' @field link 
#' @field venue 
#' @field abbreviation 
#' @field triCode 
#' @field teamName 
#' @field locationName 
#' @field firstYearOfPlay 
#' @field division 
#' @field conference 
#' @field franchise 
#' @field roster 
#' @field nextGameSchedule 
#' @field shortName 
#' @field officialSiteUrl 
#' @field franchiseId 
#' @field active 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Team <- R6::R6Class(
  'Team',
  public = list(
    `id` = NULL,
    `name` = NULL,
    `link` = NULL,
    `venue` = NULL,
    `abbreviation` = NULL,
    `triCode` = NULL,
    `teamName` = NULL,
    `locationName` = NULL,
    `firstYearOfPlay` = NULL,
    `division` = NULL,
    `conference` = NULL,
    `franchise` = NULL,
    `roster` = NULL,
    `nextGameSchedule` = NULL,
    `shortName` = NULL,
    `officialSiteUrl` = NULL,
    `franchiseId` = NULL,
    `active` = NULL,
    initialize = function(`id`, `name`, `link`, `venue`, `abbreviation`, `triCode`, `teamName`, `locationName`, `firstYearOfPlay`, `division`, `conference`, `franchise`, `roster`, `nextGameSchedule`, `shortName`, `officialSiteUrl`, `franchiseId`, `active`){
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
      if (!missing(`venue`)) {
        stopifnot(R6::is.R6(`venue`))
        self$`venue` <- `venue`
      }
      if (!missing(`abbreviation`)) {
        stopifnot(is.character(`abbreviation`), length(`abbreviation`) == 1)
        self$`abbreviation` <- `abbreviation`
      }
      if (!missing(`triCode`)) {
        stopifnot(is.character(`triCode`), length(`triCode`) == 1)
        self$`triCode` <- `triCode`
      }
      if (!missing(`teamName`)) {
        stopifnot(is.character(`teamName`), length(`teamName`) == 1)
        self$`teamName` <- `teamName`
      }
      if (!missing(`locationName`)) {
        stopifnot(is.character(`locationName`), length(`locationName`) == 1)
        self$`locationName` <- `locationName`
      }
      if (!missing(`firstYearOfPlay`)) {
        stopifnot(is.numeric(`firstYearOfPlay`), length(`firstYearOfPlay`) == 1)
        stopifnot(R6::is.R6(`firstYearOfPlay`))
        self$`firstYearOfPlay` <- `firstYearOfPlay`
      }
      if (!missing(`division`)) {
        stopifnot(R6::is.R6(`division`))
        self$`division` <- `division`
      }
      if (!missing(`conference`)) {
        stopifnot(R6::is.R6(`conference`))
        self$`conference` <- `conference`
      }
      if (!missing(`franchise`)) {
        stopifnot(R6::is.R6(`franchise`))
        self$`franchise` <- `franchise`
      }
      if (!missing(`roster`)) {
        stopifnot(R6::is.R6(`roster`))
        self$`roster` <- `roster`
      }
      if (!missing(`nextGameSchedule`)) {
        stopifnot(R6::is.R6(`nextGameSchedule`))
        self$`nextGameSchedule` <- `nextGameSchedule`
      }
      if (!missing(`shortName`)) {
        stopifnot(is.character(`shortName`), length(`shortName`) == 1)
        self$`shortName` <- `shortName`
      }
      if (!missing(`officialSiteUrl`)) {
        stopifnot(is.character(`officialSiteUrl`), length(`officialSiteUrl`) == 1)
        self$`officialSiteUrl` <- `officialSiteUrl`
      }
      if (!missing(`franchiseId`)) {
        stopifnot(is.numeric(`franchiseId`), length(`franchiseId`) == 1)
        stopifnot(R6::is.R6(`franchiseId`))
        self$`franchiseId` <- `franchiseId`
      }
      if (!missing(`active`)) {
        self$`active` <- `active`
      }
    },
    toJSON = function() {
      TeamObject <- list()
      if (!is.null(self$`id`)) {
        TeamObject[['id']] <- self$`id`$toJSON()
      }
      if (!is.null(self$`name`)) {
        TeamObject[['name']] <- self$`name`
      }
      if (!is.null(self$`link`)) {
        TeamObject[['link']] <- self$`link`
      }
      if (!is.null(self$`venue`)) {
        TeamObject[['venue']] <- self$`venue`$toJSON()
      }
      if (!is.null(self$`abbreviation`)) {
        TeamObject[['abbreviation']] <- self$`abbreviation`
      }
      if (!is.null(self$`triCode`)) {
        TeamObject[['triCode']] <- self$`triCode`
      }
      if (!is.null(self$`teamName`)) {
        TeamObject[['teamName']] <- self$`teamName`
      }
      if (!is.null(self$`locationName`)) {
        TeamObject[['locationName']] <- self$`locationName`
      }
      if (!is.null(self$`firstYearOfPlay`)) {
        TeamObject[['firstYearOfPlay']] <- self$`firstYearOfPlay`$toJSON()
      }
      if (!is.null(self$`division`)) {
        TeamObject[['division']] <- self$`division`$toJSON()
      }
      if (!is.null(self$`conference`)) {
        TeamObject[['conference']] <- self$`conference`$toJSON()
      }
      if (!is.null(self$`franchise`)) {
        TeamObject[['franchise']] <- self$`franchise`$toJSON()
      }
      if (!is.null(self$`roster`)) {
        TeamObject[['roster']] <- self$`roster`$toJSON()
      }
      if (!is.null(self$`nextGameSchedule`)) {
        TeamObject[['nextGameSchedule']] <- self$`nextGameSchedule`$toJSON()
      }
      if (!is.null(self$`shortName`)) {
        TeamObject[['shortName']] <- self$`shortName`
      }
      if (!is.null(self$`officialSiteUrl`)) {
        TeamObject[['officialSiteUrl']] <- self$`officialSiteUrl`
      }
      if (!is.null(self$`franchiseId`)) {
        TeamObject[['franchiseId']] <- self$`franchiseId`$toJSON()
      }
      if (!is.null(self$`active`)) {
        TeamObject[['active']] <- self$`active`
      }

      TeamObject
    },
    fromJSON = function(TeamJson) {
      TeamObject <- jsonlite::fromJSON(TeamJson)
      if (!is.null(TeamObject$`id`)) {
        idObject <- BigDecimal$new()
        idObject$fromJSON(jsonlite::toJSON(TeamObject$id, auto_unbox = TRUE))
        self$`id` <- idObject
      }
      if (!is.null(TeamObject$`name`)) {
        self$`name` <- TeamObject$`name`
      }
      if (!is.null(TeamObject$`link`)) {
        self$`link` <- TeamObject$`link`
      }
      if (!is.null(TeamObject$`venue`)) {
        venueObject <- Venue$new()
        venueObject$fromJSON(jsonlite::toJSON(TeamObject$venue, auto_unbox = TRUE))
        self$`venue` <- venueObject
      }
      if (!is.null(TeamObject$`abbreviation`)) {
        self$`abbreviation` <- TeamObject$`abbreviation`
      }
      if (!is.null(TeamObject$`triCode`)) {
        self$`triCode` <- TeamObject$`triCode`
      }
      if (!is.null(TeamObject$`teamName`)) {
        self$`teamName` <- TeamObject$`teamName`
      }
      if (!is.null(TeamObject$`locationName`)) {
        self$`locationName` <- TeamObject$`locationName`
      }
      if (!is.null(TeamObject$`firstYearOfPlay`)) {
        firstYearOfPlayObject <- BigDecimal$new()
        firstYearOfPlayObject$fromJSON(jsonlite::toJSON(TeamObject$firstYearOfPlay, auto_unbox = TRUE))
        self$`firstYearOfPlay` <- firstYearOfPlayObject
      }
      if (!is.null(TeamObject$`division`)) {
        divisionObject <- StandingsDivision$new()
        divisionObject$fromJSON(jsonlite::toJSON(TeamObject$division, auto_unbox = TRUE))
        self$`division` <- divisionObject
      }
      if (!is.null(TeamObject$`conference`)) {
        conferenceObject <- DivisionConference$new()
        conferenceObject$fromJSON(jsonlite::toJSON(TeamObject$conference, auto_unbox = TRUE))
        self$`conference` <- conferenceObject
      }
      if (!is.null(TeamObject$`franchise`)) {
        franchiseObject <- Franchise$new()
        franchiseObject$fromJSON(jsonlite::toJSON(TeamObject$franchise, auto_unbox = TRUE))
        self$`franchise` <- franchiseObject
      }
      if (!is.null(TeamObject$`roster`)) {
        rosterObject <- TeamRoster$new()
        rosterObject$fromJSON(jsonlite::toJSON(TeamObject$roster, auto_unbox = TRUE))
        self$`roster` <- rosterObject
      }
      if (!is.null(TeamObject$`nextGameSchedule`)) {
        nextGameScheduleObject <- TeamNextGameSchedule$new()
        nextGameScheduleObject$fromJSON(jsonlite::toJSON(TeamObject$nextGameSchedule, auto_unbox = TRUE))
        self$`nextGameSchedule` <- nextGameScheduleObject
      }
      if (!is.null(TeamObject$`shortName`)) {
        self$`shortName` <- TeamObject$`shortName`
      }
      if (!is.null(TeamObject$`officialSiteUrl`)) {
        self$`officialSiteUrl` <- TeamObject$`officialSiteUrl`
      }
      if (!is.null(TeamObject$`franchiseId`)) {
        franchiseIdObject <- BigDecimal$new()
        franchiseIdObject$fromJSON(jsonlite::toJSON(TeamObject$franchiseId, auto_unbox = TRUE))
        self$`franchiseId` <- franchiseIdObject
      }
      if (!is.null(TeamObject$`active`)) {
        self$`active` <- TeamObject$`active`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "name": %s,
           "link": %s,
           "venue": %s,
           "abbreviation": %s,
           "triCode": %s,
           "teamName": %s,
           "locationName": %s,
           "firstYearOfPlay": %s,
           "division": %s,
           "conference": %s,
           "franchise": %s,
           "roster": %s,
           "nextGameSchedule": %s,
           "shortName": %s,
           "officialSiteUrl": %s,
           "franchiseId": %s,
           "active": %s
        }',
        self$`id`$toJSON(),
        self$`name`,
        self$`link`,
        self$`venue`$toJSON(),
        self$`abbreviation`,
        self$`triCode`,
        self$`teamName`,
        self$`locationName`,
        self$`firstYearOfPlay`$toJSON(),
        self$`division`$toJSON(),
        self$`conference`$toJSON(),
        self$`franchise`$toJSON(),
        self$`roster`$toJSON(),
        self$`nextGameSchedule`$toJSON(),
        self$`shortName`,
        self$`officialSiteUrl`,
        self$`franchiseId`$toJSON(),
        self$`active`
      )
    },
    fromJSONString = function(TeamJson) {
      TeamObject <- jsonlite::fromJSON(TeamJson)
      BigDecimalObject <- BigDecimal$new()
      self$`id` <- BigDecimalObject$fromJSON(jsonlite::toJSON(TeamObject$id, auto_unbox = TRUE))
      self$`name` <- TeamObject$`name`
      self$`link` <- TeamObject$`link`
      VenueObject <- Venue$new()
      self$`venue` <- VenueObject$fromJSON(jsonlite::toJSON(TeamObject$venue, auto_unbox = TRUE))
      self$`abbreviation` <- TeamObject$`abbreviation`
      self$`triCode` <- TeamObject$`triCode`
      self$`teamName` <- TeamObject$`teamName`
      self$`locationName` <- TeamObject$`locationName`
      BigDecimalObject <- BigDecimal$new()
      self$`firstYearOfPlay` <- BigDecimalObject$fromJSON(jsonlite::toJSON(TeamObject$firstYearOfPlay, auto_unbox = TRUE))
      StandingsDivisionObject <- StandingsDivision$new()
      self$`division` <- StandingsDivisionObject$fromJSON(jsonlite::toJSON(TeamObject$division, auto_unbox = TRUE))
      DivisionConferenceObject <- DivisionConference$new()
      self$`conference` <- DivisionConferenceObject$fromJSON(jsonlite::toJSON(TeamObject$conference, auto_unbox = TRUE))
      FranchiseObject <- Franchise$new()
      self$`franchise` <- FranchiseObject$fromJSON(jsonlite::toJSON(TeamObject$franchise, auto_unbox = TRUE))
      TeamRosterObject <- TeamRoster$new()
      self$`roster` <- TeamRosterObject$fromJSON(jsonlite::toJSON(TeamObject$roster, auto_unbox = TRUE))
      TeamNextGameScheduleObject <- TeamNextGameSchedule$new()
      self$`nextGameSchedule` <- TeamNextGameScheduleObject$fromJSON(jsonlite::toJSON(TeamObject$nextGameSchedule, auto_unbox = TRUE))
      self$`shortName` <- TeamObject$`shortName`
      self$`officialSiteUrl` <- TeamObject$`officialSiteUrl`
      BigDecimalObject <- BigDecimal$new()
      self$`franchiseId` <- BigDecimalObject$fromJSON(jsonlite::toJSON(TeamObject$franchiseId, auto_unbox = TRUE))
      self$`active` <- TeamObject$`active`
    }
  )
)
