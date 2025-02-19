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
VersionWarningStr <- "VersionWarning"
MessageToClientStr <- "MessageToClient"


.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFSStandGrowth4R !")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
  tryCatch(
    {
      status <- SGStatus()
      if (VersionWarningStr %in% names(status)) {
        warning(status[[VersionWarningStr]])
      }
      if (MessageToClientStr %in% names(status)) {
        packageStartupMessage(status[[MessageToClientStr]])
      }
    },
    error = function(cond) {
      warning("R is unable to connect to CFSStandGrowth Web API! \n It may be that your internet connection is not working or your firewall does not allow for http requests.")
    }
  )
}

.onDetach <- function(libname) {
  print("CFSStandGrowth4R shutting down")
}

#'
#' Return the available fields of the metamodels in the database that can be
#' used in queries.
#'
#' The possible values are also returned.
#'
#' @return a data.frame object
#' @param nchar the number of characters to be kept in the 'values' field (100 by default)
#'
#' @export
SGGetMetaModelQueryFields <- function(nchar = 100) {

  query <- paste(serverAddress, "metamodels/fields", sep = "")

  json <- .sendQueryAndRetrieveResult(query)

  output <- NULL
  for (name in names(json)) {
    possibleValues = paste(json[[name]], collapse = ", ")
    if (nchar(possibleValues) > nchar) {
      possibleValues <- paste(substr(possibleValues,1,nchar), "...")
    }
    output <- rbind(output, data.frame(field = name, values = possibleValues))
  }

  return (output)
}

#'
#' Provide a Selection of Meta-Models
#'
#' Each field is queried using OR as logical operator between the
#' different values. The queries are then linked using an AND operator.
#'
#' For instance, if the geoDomain argument is the vector c("6OUEST", "6EST")
#' and the dataSource argument is the vector c("PET3", "PET4"), then all the
#' meta models fitted in either geoDomain 6OUEST or 6EST and fitted to either
#' dataSource PET3 or PET4 are selected.
#'
#' @param geoDomain the geographic domain of the meta-model (e.g. the
#' bioclimatic subdomain in Quebec)
#' @param dataSource the source of data used to fit the meta-model (e.g. PET4
#' which is the fourth campaign of the provincial inventory in Quebec)
#' @param climateChangeOption the climate change scenario
#' @param growthModel the complex growth model used to run the simulation
#' (e.g. artemis2009)
#' @param outputType the response variable (e.g. AliveVolume_AllSpecies)
#' @param fitModel the formulation of the meta-model (e.g. ChapmanRichards)
#' @param stratumGroup the name of the stratum group that is represented
#' by the meta-model
#'
#' @return a data.frame object
#'
#' @seealso the SGGetMetaModelQueryFields function for the values of the different
#' fields
#'
#' @examples
#' myMetaModels <- SGSelectMetaModels(geoDomain = c("6OUEST", "6EST"), dataSource = c("PET3", "PET4"))
#'
#' @export
SGSelectMetaModels <- function(geoDomain = NULL,
                               dataSource = NULL,
                               climateChangeOption = NULL,
                               growthModel = NULL,
                               outputType = NULL,
                               fitModel = NULL,
                               stratumGroup = NULL) {

  valueList <- list()
  if (!is.null(geoDomain)) {
    valueList[["geoDomain"]] <- geoDomain
  }
  if (!is.null(dataSource)) {
    valueList[["dataSource"]] <- dataSource
  }
  if (!is.null(climateChangeOption)) {
    valueList[["climateChangeOption"]] <- climateChangeOption
  }
  if (!is.null(growthModel)){
    valueList[["growthModel"]] <- growthModel
  }
  if (!is.null(outputType)) {
    valueList[["outputType"]] <- outputType
  }
  if (!is.null(fitModel)) {
    valueList[["fitModel"]] <- fitModel
  }
  if (!is.null(stratumGroup)) {
    valueList[["stratumGroup"]] <- stratumGroup
  }

  if (length(valueList) == 0) {
    stop("At least one argument of the function must be non null!")
  }

  selectedMetaModels <- NULL
  for (name in names(valueList)) {
    myValues <- valueList[[name]]
    query <- .SGConstructOrQuery(myValues, name)
    metaModels <- .SGFilterMetaModels(query)
    if (is.null(selectedMetaModels)) {
      selectedMetaModels <- metaModels
    }
    common <- intersect(selectedMetaModels$mmid, metaModels$mmid)
    if(is.null(common)) {
      print("No matches found.")
      return(list())
    }
    selectedMetaModels <- selectedMetaModels[which(selectedMetaModels$mmid %in% common),]
  }
  return(selectedMetaModels)
}





#'
#' Return the field combinations and the count of each combination.
#'
#' @return a data.frame object
#' @param fields a vector of field names
#'
#' @examples
#' SGGetMetaModelFieldCombinations(c("geoDomain", "dataSource"))
#'
#'
#' @seealso [SGGetMetaModelQueryFields()] for the values of the different
#' fields
#'
#' @export
SGGetMetaModelFieldCombinations <- function(fields) {
  query <- paste(serverAddress, "metamodels/fieldCombinations?fields=", paste(fields, collapse=","), sep = "")
  json <- .sendQueryAndRetrieveResult(query)
  return(json)
}

.SGAnd <- function() {
  return(",")
}

#'
#' Return the AND predicate for use in query construction
#'
#' THIS FUNCTION IS DEPRECATED. PLEASE USE THE SGSelectMetaModels FUNCTION
#' INSTEAD.
#'
#' @name SGAnd-deprecated
#'
#' @export
SGAnd <- function() {
  .Deprecated("SGSelectMetaModels")
  return (.SGAnd())
}

.SGConstructOrQuery <- function(vValues, fieldName) {
  output <- c()
  for (v in vValues) {
    if (length(output) > 0) {
      output <- c(output, .SGOr(), .SGContains(fieldName, v))
    } else {
      output <- .SGContains(fieldName, v)
    }
  }
  return(paste(output, collapse=""))
}

#'
#' Produce a Query with all the values separated by OR operators.
#'
#' THIS FUNCTION IS DEPRECATED. PLEASE USE THE SGSelectMetaModels FUNCTION
#' INSTEAD.
#'
#' @return a character string that can be used with the SGQuery function
#' @param vValues a vector of values
#' @param fieldName the name of the field
#'
#'
#' @name SGConstructOrQuery-deprecated
#'
#' @export
SGConstructOrQuery <- function(vValues, fieldName) {
  .Deprecated("SGSelectMetaModels")
  return(.SGConstructOrQuery(vValues, fieldName))
}


.SGOr <- function() {
  return("'")
}

#' Returns the OR predicate for use in query construction
#'
#' THIS FUNCTION IS DEPRECATED. PLEASE USE THE SGSelectMetaModels FUNCTION
#' INSTEAD.
#'
#' @name SGOr-deprecated
#'
#' @export
SGOr <- function() {
  .Deprecated("SGSelectMetaModels")
  return (.SGOr())
}

.SGContains <- function(field, value) {
  return (paste(field, ":", value, sep=""))
}


#' Returns the Contains term for use in query construction
#'
#' This corresponds to the LIKE SQL operator which is case sensitive.
#' #'
#' @param field one of the fields listed by the SGGetMetaModelQueryFields function
#' @param value the value for this field
#'
#' THIS FUNCTION IS DEPRECATED. PLEASE USE THE SGSelectMetaModels FUNCTION
#' INSTEAD.
#'
#' @name SGContains-deprecated
#'
#' @export
SGContains <- function(field, value) {
  .Deprecated("SGSelectMetaModels")
  return (.SGContains(field, value))
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
#'
#' THIS FUNCTION IS DEPRECATED. PLEASE USE THE SGSelectMetaModels FUNCTION
#' INSTEAD.
#'
#' @name SGQuery-deprecated
#'
#' @export
SGQuery <- function(...) {
  .Deprecated("SGSelectMetaModels")
  return (paste(..., sep = ""))
}

.SGFilterMetaModels <- function(query = NULL) {
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
#'
#' @export
SGFilterMetaModels <- function(query = NULL) {
  .Deprecated("SGSelectMetaModels")
  .SGFilterMetaModels(query)
}

.sendQueryAndRetrieveResult <- function(query, toJSON = TRUE) {
  res <- httr::GET(query)
  succes <-  res$status_code == 200
  jsonstr <- rawToChar(res$content)
  if (toJSON) {
    json <- jsonlite::fromJSON(jsonstr)
    if (!succes) {
      stop(.getErrorMessage(json))
    }
    return(json)
  } else {
    return(jsonstr)
  }
}

#'
#' Gets a set of predictions from the specified metamodel
#'
#' @param mmid A string containing the mmid of the MetaModel#'
#' @param ageyrmin The minimum age year to get the prediction set for
#' @param ageyrmax The maximum age year to get the prediction set for (ageyrmax will be included in output values)
#' @param step (optional int) The number of years to use from ageyrmin to ageyrmax for predictions (default is 1 if not specified)
#' @param varout (optional) A string specifying the variance output mode from the following :
#'           NONE : no variance output
#'           PARAMEST (default) : variance output for parameter estimation
#'           PARAMESTRE : variance output for parameter estimation including random effect
#' @return a data.frame object with two columns. "age" is the age of the stand (yr) and "v" is the prediction (e.g. m3/ha)
#' @seealso SGPredictMC
#' @export
SGPredict <- function(mmid, ageyrmin, ageyrmax, step=NULL, varout="PARAMEST") {
  query <- paste0(serverAddress, "api/predict?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax)
  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }
  if (!is.null(varout)) {
    query <- paste(query, "&varout=", varout, sep = "")
  }
  pred <- .sendQueryAndRetrieveResult(query)
  return (pred)
}

#'
#' Provide the regeneration lag if any.
#'
#' @param mmid A string containing the mmid of the MetaModel#'
#' @return a numeric
#'
#' @export
SGGetRegenerationLagIfAny <- function(mmid) {
  query <- paste0(serverAddress, "api/reglag?mmid=", mmid)
  regLag <- .sendQueryAndRetrieveResult(query)
  return(regLag[[1]])
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
#' @return a data.frame object with four columns (RealizationID, SubjectID, AgeYr, Pred).
#' @seealso SGPredict
#' @export
SGPredictMC <- function(mmid, ageyrmin, ageyrmax, step=NULL, nbsub=1, nbreal=1) {
  query <- paste(serverAddress, "api/predictmc?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax, "&nbsub=", nbsub, "&nbreal=", nbreal, sep = "")
  if (!is.null(step)) {
    query <- paste(query, "&step=", step, sep = "")
  }
  pred <- .sendQueryAndRetrieveResult(query)

  # if (includeRegLag) {
  #   query <- paste0(serverAddress, "api/reglag?mmid=", mmid)
  #   regLag <- .sendQueryAndRetrieveResult(query)
  #   pred$AgeYr <- pred$AgeYr + regLag$regLagYr
  # }

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
#' Provide the summary of a particular Meta-Model.
#'
#' @param mmid A string containing the parameter estimates and
#' other information.
#'
#' @return a list object
#' @export
SGGetSummary <- function(mmid) {
  query <- paste0(serverAddress, "api/summary?mmid=", mmid)
  summary <- .sendQueryAndRetrieveResult(query, toJSON = F)
  cat(summary)
  return (invisible(NULL))
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
#'
#' @return a ggplot2 graph
#' @export
SGGOFGraph <- function(mmid, textsize = 20, title = mmid) {
  query <- paste0(serverAddress, "api/fitdata?mmid=", mmid)
  dataset <- .sendQueryAndRetrieveResult(query)

  maxX <- max(dataset$timeSinceInitialDateYr + dataset$initialAgeYr)
  predictions <- SGPredict(mmid, 1, maxX)

  plot <- CFSMetaModelCommons4R::createGOFplot(dataObject = dataset,
                                                  predictions = predictions,
                                                  title = title,
                                                  textsize = textsize)

  return(plot)

}


#'
#' Find the Metamodel(s) that Best Fit a Particular Context.
#'
#' The function can process multiple queries if all the arguments
#' are of the same length.
#'
#' @param geoRegion the geoRegion (e.g., QC)
#' @param geoDomain the geoDomain (e.g., 2OUEST)
#' @param outputType the outputType (e.g., Volume)
#' @param ecoType the ecotype (e.g., MS22)
#' @param leadingSpecies an option character string that stands
#' for the leading species (e.g., EN)
#' @return a data.frame instance
#'
#' @export
SGFindBest <- function(geoRegion, geoDomain, outputType, ecoType, leadingSpecies = NULL) {
  if (is.null(geoRegion) | is.null(geoDomain) | is.null(outputType) | is.null(ecoType)) {
    stop("The geoRegion, geoDomain, outputType and ecoType arguments must be non null!")
  }
  if (length(geoRegion) != length(geoDomain) |
      length(geoRegion) != length(outputType) |
      length(geoRegion) != length(ecoType) |
      (!is.null(leadingSpecies) & length(geoRegion) != length(leadingSpecies))) {
    stop("The geoRegion, geoDomain, outputType, ecoType and (optionally) leadingSpecies arguments must be of the same length!")
  }
  nbContexts <- length(geoRegion)
  overallDataset <- NULL
  for (i in 1:nbContexts) {
    query <- paste0(serverAddress, "metamodels/findBest?georegion=", geoRegion[i],
                    "&geodomain=", geoDomain[i],
                    "&outputtype=", outputType[i],
                    "&ecotype=", ecoType[i])
    if (!is.null(leadingSpecies)) {
      query <- paste0(query, "&leadingspecies=", leadingSpecies[i])
    }
    dataset <- .sendQueryAndRetrieveResult(query)
    dataset <- dataset[,colnames(dataset)[which(!colnames(dataset) %in% c("dataSourceYears", "nbPlots"))]]
    newColnames <- paste0("bestFit_", colnames(dataset))
    colnames(dataset) <- newColnames
    dataset$queryId <- i
    dataset$geoRegion <- geoRegion[i]
    dataset$geoDomain <- geoDomain[i]
    dataset$outputType <- outputType[i]
    dataset$ecoType <- ecoType[i]
    if (!is.null(leadingSpecies)) {
      dataset$leadingSpecies <- leadingSpecies[i]
    } else {
      dataset$leadingSpecies <- "None"
    }
    overallDataset <- rbind(overallDataset, dataset[,c("queryId", "geoRegion", "geoDomain", "outputType", "ecoType", "leadingSpecies", newColnames)])
  }
  return(overallDataset)
}



