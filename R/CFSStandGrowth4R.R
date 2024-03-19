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

#'
#' Returns the available fields of the metamodels in the database that can be
#' used in queries
#'
#' @return a list of field names
#'
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
  query <- paste0(serverAddress, "api/predict?mmid=", mmid, "&ageyrmin=", ageyrmin, "&ageyrmax=", ageyrmax)

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
    return (json)
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
#' @param mmid A string containing the mmid of the MetaModel
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

  if (succes) {
    return (json)
  } else {
    stop(.getErrorMessage(json))
  }

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
  res <- GET(query)
  succes <-  res$status_code == 200
  jsonstr <- rawToChar(res$content)
  dataset <- fromJSON(jsonstr)

  if (succes) {
    return (dataset)
  } else {
    stop(.getErrorMessage(dataset))
  }
}


#'
#' Provide a Goodness-of-Fit Graph for a Particular MetaModel.
#'
#' The graph displays the simulations and the predictions of
#' the metamodel instance.
#'
#' @param mmid A string containing the mmid of the MetaModel
#' @param textSize the font size (20 by default)
#' @param plotPred a boolean true to enable the plot of predicted values
#' @param title an optional title for the graph (the mmid by default)
#' @param ymax the upper bound of the y axis (250 by default)
#'
#' @return a ggplot2 graph
#' @export
SGGOFGraph <- function(mmid, textsize = 20, plotPred = T, title = mmid, ymax = 250) {
  query <- paste0(serverAddress, "api/fitdata?mmid=", mmid)
  res <- GET(query)
  succes <-  res$status_code == 200
  jsonstr <- rawToChar(res$content)
  dataset <- fromJSON(jsonstr)

  if (!succes) {
    stop(.getErrorMessage(dataset))
  }

  isVarianceAvailable <- "TotalVariance" %in% colnames(dataset)

  if (isVarianceAvailable)
  {
    dataset$lower95 <- dataset$Estimate - dataset$TotalVariance^.5 * qnorm(0.975)
    dataset[which(dataset$lower95 < 0), "lower95"] <- 0
    dataset$upper95 <- dataset$Estimate + dataset$TotalVariance^.5 * qnorm(0.975)
  }

  dataset$age <- dataset$initialAgeYr + dataset$timeSinceInitialDateYr
  dataset$stratum <- paste(dataset$OutputType,dataset$initialAgeYr,sep="_")
  dataset$predL95 <- dataset$pred - dataset$predVar^.5 * qnorm(0.975)
  dataset$predU95 <- dataset$pred + dataset$predVar^.5 * qnorm(0.975)
  dataset[which(dataset$predL95 < 0), "predL95"] <- 0

  datasetPred <- NULL
  uniqueAge <- c()
  for (i in 1:length(dataset[,1])) {
    if (!dataset[i,"age"] %in% uniqueAge) {
      datasetPred <- rbind(datasetPred, dataset[i,])
      uniqueAge <- c(uniqueAge, dataset[i,"age"])
    }
  }
  plot <- ggplot2::ggplot()
  if (isVarianceAvailable)
  {
    plot <- plot +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
  }

  plot <- plot +
    ggplot2::geom_line(ggplot2::aes(y=Estimate, x=age, group=stratum), dataset, lty = "dashed") +
    ggplot2::xlab("Age (yr)") +
    ggplot2::ylab(bquote('Volume'~(m^3~ha^{-1}))) +
    ggplot2::ylim(0,ymax) +
    ggplot2::xlim(0, 220) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=textsize),
                   axis.text.x = ggplot2::element_text(size=textsize, color = "black"),
                   axis.text.y = ggplot2::element_text(size=textsize, color = "black"),
                   axis.line = ggplot2::element_line(color = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.ticks.length = ggplot2::unit(3,"mm"),
                   panel.border = ggplot2::element_blank())
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (plotPred) {
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=age), datasetPred, alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(y=pred, x=age), datasetPred, lty = "solid", size = 1.5)
  }
  return(plot)

}




