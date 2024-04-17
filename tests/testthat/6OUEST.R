#'
#' Code sample for CFSStandGrowth Web API
#'

library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("geoDomain", "6OUEST"))

listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)

SGGetMetaData(listMetaModels$mmid[1])

SGGOFGraph(listMetaModels$mmid[1], ymax = 300)

predictions <- SGPredict(listMetaModels$mmid[1], 1, 150, 1)

