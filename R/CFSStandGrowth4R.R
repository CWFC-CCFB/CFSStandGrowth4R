#############################################################
# This file is part of the CFSStandGrowth4R library located at :
# https://github.com/CWFC-CCFB/CFSStandGrowth4R
#
# Copyright (C) 2022-2024 His Majesty the King in Right of Canada
# Authors: Jean-François Lavoie and Mathieu Fortin, Canadian Forest Service.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed with the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# Please see the license at http://www.gnu.org/copyleft/lesser.html.
#############################################################

serverAddress <- "http://repicea.dynu.net/standgrowth/"
#serverAddress <- "http://localhost:50101/"

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFSStandGrowth4R !")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onDetach <- function(libname) {
  print("CFSStandGrowth4R shutting down")
}

#'
#' Returns the available fields of the metamodels in the database that can be
#' used in queries
#'
#' @return a list of field names
#'
#' @export
SGGetMetaModelQueryFields <- function() {

  res <- httr::GET(paste(serverAddress, "metamodels/1", sep = ""))

  jsonstr <- rawToChar(res$content)

  json <- jsonlite::fromJSON(jsonstr)

  # remove fields that cannot be used in queries
  json$'dataSourceYears'= NULL
  json$'nbPlots'= NULL
  json$'_links' = NULL

  return (names(json))
}

#' Returns the AND predicate for use in query construction
#' @export
SGAnd <- function() {
  return (",")
}

#' Returns the OR predicate for use in query construction
#' @export
SGOr <- function() {
  return ("'")
}

#' Returns the Contains term for use in query construction
#' This corresponds to the LIKE SQL operator which is case sensitive.
#' @param field one of the fields listed by the SGGetMetaModelQueryFields function
#' @param value the value for this field
#' @export
SGContains <- function(field, value) {
  return (paste(field, ":", value, sep=""))
}

#' Returns the LowerThan term for use in query construction
#' @param field one of the fields listed by the SGGetMetaModelQueryFields function
#' @param value the value for this field
#' @export
SGLowerThan <- function(field, value) {
  return (paste(field, "<", value, sep=""))
}

#' Returns the GreaterThan term for use in query construction
#' @param field one of the fields listed by the SGGetMetaModelQueryFields function
#' @param value the value for this field
#' @export
SGGreaterThan <- function(field, value) {
  return (paste(field, ">", value, sep=""))
}

#' Constructs a query using the provided terms and predicates
#' A query is a series of one or more terms separated by predicates.
#' Example : mmid contains "foo" AND outputType contains "All" OR nbRealizations > 1000
#' @param ... the multiple operators of the query
#' @export
SGQuery <- function(...) {
  return (paste(..., sep = ""))
}

#' Searches the metamodel database for matches to the specified query
#'
#'
#' @param query a string containing the query.  Null will return all metamodels in the database.
#' Queries should be created using the
#' query utility methods.
#'
#' example : SGQuery(SGContains("mmid", "FMU02664"), SGAnd(), SGContains("outputType", "Coniferous"))
#'
#' Note : Groups / associativity is not supported by queries.  To perform a query of type :
#' (TermA AND TermB) OR TermC you will have to create a query for (TermA AND TermB),
#' then a second query for TermC and then combine the results together.
#'
#' @return a dataframe containing all metamodels matching the query
#' @seealso SGQuery, SGContains, SGGreaterThan, SGLowerThan, SGAnd, SGOr
#' @export
SGFilterMetaModels <- function(query = NULL) {
  if (is.null(query)) {
	  res <- httr::GET(paste(serverAddress, "metamodels", sep = ""))
	  jsonstr <- rawToChar(res$content)
	  json <- jsonlite::fromJSON(jsonstr)
	  return (json$'_embedded'$'metamodels')
  } else {
    res <- httr::GET(paste(serverAddress, "metamodels/filter?query=", query, sep = ""))
    jsonstr <- rawToChar(res$content)
    json <- jsonlite::fromJSON(jsonstr)
    return (json)
  }
}

.sendQueryAndRetrieveResult <- function(query) {
  res <- httr::GET(query)
  succes <-  res$status_code == 200
  jsonstr <- rawToChar(res$content)
  json <- jsonlite::fromJSON(jsonstr)
  if (!succes) {
    stop(.getErrorMessage(json))
  }
  return(json)
}

#' Gets a set of predictions from the specified metamodel
#' @param mmid A string containing the mmid of the MetaModel#'
#' @param ageyrmin The minimum age year to get the prediction set for
#' @param ageyrmax The maximum age year to get the prediction set for (ageyrmax will be included in output values)
#' @param step (optional int) The number of years to use from ageyrmin to ageyrmax for predictions (default is 1 if not specified)
#' @param varout (optional) A string specifying the variance output mode from the following :
#'           NONE : no variance output
#'           PARAMEST (default) : variance output for parameter estimation
#'           PARAMESTRE : variance output for parameter estimation including random effect
#' @param includeRegLag a logical; TRUE to include the regeneration lag or false otherwise (default is TRUE)
#' @return a data.frame object with two columns. "age" is the age of the stand (yr) and "v" is the prediction (e.g. m3/ha)
#' @seealso SGPredictMC
#' @export
SGPredict <- function(mmid, ageyrmin, ageyrmax, step=NULL, varout="PARAMEST", includeRegLag = T) {
  query <- paste0(serverAddress, "api/predict?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax)
  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }
  if (!is.null(varout)) {
    query <- paste(query, "&varout=", varout, sep = "")
  }
  pred <- .sendQueryAndRetrieveResult(query)

  if (includeRegLag) {
    query <- paste0(serverAddress, "api/reglag?mmid=", mmid)
    regLag <- .sendQueryAndRetrieveResult(query)
    pred$AgeYr <- pred$AgeYr + regLag$regLagYr
  }
  return (pred)
}

.getErrorMessage <- function(jsonList) {
  if (!is.null(jsonList$error)) {
    return(jsonList$error)
  } else {
    return(jsonList$message)
  }
}

#' Gets Predictions using Monte-Carlo Simulated Parameters.
#'
#' The function simulates deviates for the parameter estimates. The variability of the
#' random effects and the parameter estimates can be disabled by setting the nbsub and
#' nbreal arguments to zero, respectively.
#' @param mmid A string containing the mmid of the MetaModel
#' @param ageyrmin The minimum age year to get the prediction set for
#' @param ageyrmax The maximum age year to get the prediction set for (ageyrmax will be included in output values)
#' @param step (optional int) The number of years to use from ageyrmin to ageyrmax for predictions (default is 1 if not specified)
#' @param nbsub The number of subjects to simulate parameters for in range (0,1000). Zero disables the
#' variability due to the random effect.
#' @param nbreal The number of realizations to simulate parameters for in range (0,1000). Zero disables
#' the variability due to the parameter estimates.
#' @param includeRegLag a logical; TRUE to include the regeneration lag or false otherwise (default is TRUE)
#' @return a data.frame object with four columns (RealizationID, SubjectID, AgeYr, Pred).
#' @seealso SGPredict
#' @export
SGPredictMC <- function(mmid, ageyrmin, ageyrmax, step=NULL, nbsub=1, nbreal=1, includeRegLag = T) {
  query <- paste(serverAddress, "api/predictmc?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax, "&nbsub=", nbsub, "&nbreal=", nbreal, sep = "")
  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }
  pred <- .sendQueryAndRetrieveResult(query)

  if (includeRegLag) {
    query <- paste0(serverAddress, "api/reglag?mmid=", mmid)
    regLag <- .sendQueryAndRetrieveResult(query)
    pred$AgeYr <- pred$AgeYr + regLag$regLagYr
  }

  return (pred)
}

#' Provide the Status of the Web API.
#'
#' @return a data.frame object with four columns (RealizationID, SubjectID, AgeYr, Pred).
#'
#' @export
SGStatus <- function() {
  query <- paste0(serverAddress, "api/status")
  json <- .sendQueryAndRetrieveResult(query)
  return (json)
}

#'
#' Provide the Final Sample of Parameter Estimates.
#'
#' The final sample is the one provided by a selection
#' rate of the Markov chain, This function does not rely
#' on a cache on the server end. The full meta-model must
#' be deserialized and this may take a few seconds.
#'
#' @param mmid A string containing the mmid of the MetaModel
#'
#' @return a data.frame object
#' @export
SGGetFinalSample <- function(mmid) {
  query <- paste0(serverAddress, "api/finalsample?mmid=", mmid)
  dataset <- .sendQueryAndRetrieveResult(query)
  return (dataset)
}

#'
#' Provide the Meta-Data of a particular Meta-Model.
#'
#' @param mmid A string containing the mmid of the meta-model
#'
#' @return a list object
#' @export
SGGetMetaData <- function(mmid) {
  query <- paste0(serverAddress, "api/metadata?mmid=", mmid)
  dataset <- .sendQueryAndRetrieveResult(query)
  return (dataset)
}

#'
#' Provide a Goodness-of-Fit Graph for a Particular MetaModel.
#'
#' The graph displays the simulations and the predictions of
#' the metamodel instance.
#'
#' @param mmid A string containing the mmid of the MetaModel
#' @param textsize the font size (20 by default)
#' @param title an optional title for the graph (the mmid by default)
#' @param ymax the upper bound of the y axis (250 by default)
#'
#' @return a ggplot2 graph
#' @export
SGGOFGraph <- function(mmid, textsize = 20, title = mmid, ymax = 250) {
  query <- paste0(serverAddress, "api/fitdata?mmid=", mmid)
  dataset <- .sendQueryAndRetrieveResult(query)

  maxX <- max(dataset$timeSinceInitialDateYr + dataset$initialAgeYr)
  predictions <- SGPredict(mmid, 1, maxX, includeRegLag = F)

  plot <- CFSMetaModelCommons4R::createGOFplot(dataObject = dataset,
                                                  predictions = predictions,
                                                  title = title,
                                                  textsize = textsize)

  return(plot)

}




