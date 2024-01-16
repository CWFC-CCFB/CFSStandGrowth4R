#############################################################
# This file is part of the CFSStandGrowth4R library located at :
# https://github.com/CWFC-CCFB/CFSStandGrowth4R
#
# Copyright (C) 2022 Jean-François Lavoie and Mathieu Fortin
# for Canadian Forest Service.
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

#'
#' The cache environment of this package
#'
#' This environment contains the objects that enable the connection to
#' the gateway server.
#'
#'@export

serverAddress <- "http://repicea.dynu.net/standgrowth/"
#serverAddress <- "http://localhost:8080/"

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFSStandGrowth4R !")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onDetach <- function(libname) {
  print("CFSStandGrowth4R shutting down")
}

#' Returns the available fields of the metamodels in the database that can be
#' used in queries
#' @return a list of field names
#' @export
SGGetMetaModelQueryFields <- function() {

  res <- GET(paste(serverAddress, "metamodels/1", sep = ""))

  jsonstr <- rawToChar(res$content)

  json <- fromJSON(jsonstr)

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
#' Note : This corresponds to the LIKE SQL operator which is case sensitive
#' @export
SGContains <- function(field, value) {
  return (paste(field, ":", value, sep=""))
}

#' Returns the LowerThan term for use in query construction
#' @export
SGLowerThan <- function(field, value) {
  return (paste(field, "<", value, sep=""))
}

#' Returns the GreaterThan term for use in query construction
#' @export
SGGreaterThan <- function(field, value) {
  return (paste(field, ">", value, sep=""))
}

#' Constructs a query using the provided terms and predicates
#' A query is a series of one or more terms separated by predicates.
#' Example : mmid contains "foo" AND outputType contains "All" OR nbRealizations > 1000
#' @export
SGQuery <- function(...) {
  return (paste(..., sep = ""))
}

#' Searches the metamodel database for matches to the specified query and returns
#' the matching metamodels
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
	  res <- GET(paste(serverAddress, "metamodels", sep = ""))
	  jsonstr <- rawToChar(res$content)
	  json <- fromJSON(jsonstr)
	  return (json$'_embedded'$'metamodels')
  }
  else {
    res <- GET(paste(serverAddress, "metamodels/filter?query=", query, sep = ""))
    jsonstr <- rawToChar(res$content)
    json <- fromJSON(jsonstr)
    return (json)
  }
}

#' Gets a set of predictions from the specified metamodel
#' @param mmid A string containing the mmid of the MetaModel#'
#' @param ageyrmin The minimum age year to get the prediction set for
#' @param ageyrmax The maximum age year to get the prediction set for (ageyrmax will be included in output values)
#' @param step (optional int) The number of years to use from ageyrmin to ageyrmax for predictions (default is 1 if not specified)
#' @param varout (optional) A string specifying the variance output mode from the following :
#'           NONE (default) : no variance output
#'           PARAMEST : variance output for parameter estimation
#'           PARAMESTRE : variance output for parameter estimation including random effect
#' @return a data.frame object with two columns. "age" is the age of the stand (yr) and "v" is the prediction (e.g. m3/ha)
#' @seealso SGPredictMC
#' @export
SGPredict <- function(mmid, ageyrmin, ageyrmax, step=NULL, varout=NULL) {
  query <- paste(serverAddress, "api/predict?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax, sep = "")

  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }

  if (!is.null(varout)) {
    query <- paste(query, "&varout=", varout, sep = "")
  }

  res <- GET(query)
  succes <-  res$status_code == 200

  jsonstr <- rawToChar(res$content)

  json <- fromJSON(jsonstr)

  if (succes) {
    df <- data.frame(age=as.numeric(names(json$predictions)), v=unlist(json$predictions))
    return (df)
  } else {
    stop(.getErrorMessage(json))
  }
}

.getErrorMessage <- function(jsonList) {
  if (!is.null(jsonList$error)) {
    return(jsonList$error)
  } else {
    return(jsonList$message)
  }
}

#' Gets predictions using Monte-Carlo simulated parameters
#' @param mmid A string containing the mmid of the MetaModel#'
#' @param ageyrmin The minimum age year to get the prediction set for
#' @param ageyrmax The maximum age year to get the prediction set for (ageyrmax will be included in output values)
#' @param step (optional int) The number of years to use from ageyrmin to ageyrmax for predictions (default is 1 if not specified)
#' @param nbsub The number of subjects to simulate parameters for in range (1,1000)
#' @param nbreal The number of realizations to simulate parameters for in range (1,1000)
#' @return a data.frame object with four columns. "real" is the realization id
#' "sub" is the subject id,  "age" is the age of the stand (yr) and "v" is the prediction (e.g. m3/ha)
#' @seealso SGPredict
#' @export
SGPredictMC <- function(mmid, ageyrmin, ageyrmax, step=NULL, nbsub=1, nbreal=1) {

  query <- paste(serverAddress, "api/predictmc?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax, "&nbsub=", nbsub, "&nbreal=", nbreal, sep = "")

  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }

  res <- GET(query)
  succes <-  res$status_code == 200

  jsonstr <- rawToChar(res$content)

  json <- fromJSON(jsonstr)
  nbages <- NULL

  if (succes) {
    i <- 1
    for (real in 1:nbreal) {
      for (sub in 1:nbsub) {
        innerList <- json[[real]][[sub]]
        if (is.null(nbages)) {
          nbages <- length(innerList)
          size <- nbreal * nbsub * nbages
          realId <- rep(NA,size)
          subId <- rep(NA,size)
          ageId <- rep(NA,size)
          pred <- rep(NA,size)
        }
        for (j in 1:length(innerList)) {
          realId[i] <- real
          subId[i] <- sub
          ageId[i] <- as.integer(names(innerList)[j])
          pred[i] <- as.numeric(innerList[[j]])
          i <- i + 1
        }
      }
    }
    df <- data.frame(real = realId, sub = subId, age=ageId, v=pred)
    return (df)
  } else {
    stop(.getErrorMessage(json))
  }

}


