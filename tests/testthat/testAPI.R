########################################################
# Tests for CFSStandGrowth4R client
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2024
########################################################


library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()

test_that("Check query fields", {
  expect_equal(length(fields), 10)
  expect_equal(fields[1], "mmid")
})

query <- SGQuery(SGContains("geoDomain", "6OUEST"))
listMetaModels <- SGFilterMetaModels(query)

test_that("Check model list", {
  expect_equal("QC_6OUEST_STR_ME1_6OUEST_NoChange_AliveVolume_AllSpecies" %in% listMetaModels$mmid, TRUE)
})

metaData <- SGGetMetaData(listMetaModels$mmid[1])
test_that("Check meta data", {
  expect_equal(length(metaData), 13)
})

prediction <- SGPredict(listMetaModels$mmid[1], 1, 150, 1)
test_that("Check predictions", {
  expect_equal(nrow(prediction), 150)
  expect_equal(ncol(prediction), 2)
})

predictionMC <- SGPredictMC(listMetaModels$mmid[1], 1, 150, 1, nbsub = 2, nbreal = 4)
test_that("Check MC predictions", {
  expect_equal(nrow(predictionMC), 2 * 4 * 150)
  expect_equal(ncol(predictionMC), 4)
})

