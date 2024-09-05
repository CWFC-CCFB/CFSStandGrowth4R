########################################################
# Tests for CFSStandGrowth4R client
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2024
########################################################


library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()

test_that("Check query fields", {
  expect_equal(nrow(fields), 10)
  expect_equal(fields[1,"field"], "mmid")
})

query <- SGQuery(SGConstructOrQuery(c("6OUEST", "6EST"), "geoDomain"))
listMetaModels <- SGFilterMetaModels(query)
test_that("Check model list", {
  expect_equal("QC_6OUEST_ME13_NoChange_AliveVolume_AllSpecies" %in% listMetaModels$mmid, TRUE)
  expect_equal("QC_6EST_MS22_NoChange_AliveVolume_AllSpecies" %in% listMetaModels$mmid, TRUE)
})

result <- SGGetMetaModelFieldCombinations(c("geoDomain", "growthModel"))
test_that("Check model list", {
  expect_equal(nrow(result), 11)
  expect_equal(ncol(result), 3)
})

query <- SGQuery(SGContains("geoDomain", "6OUEST"))
listMetaModels <- SGFilterMetaModels(query)

test_that("Check model list", {
  expect_equal("QC_6OUEST_ME13_NoChange_AliveVolume_AllSpecies" %in% listMetaModels$mmid, TRUE)
})

metaData <- SGGetMetaData(listMetaModels$mmid[1])
test_that("Check meta data", {
  expect_equal(length(metaData), 13)
})

prediction <- SGPredict(listMetaModels$mmid[1], 0, 150, 1)
test_that("Check predictions", {
  expect_equal(nrow(prediction), 151)
  expect_equal(ncol(prediction), 3)
})

predictionMC <- SGPredictMC(listMetaModels$mmid[1], 1, 150, 1, nbsub = 2, nbreal = 4)
test_that("Check MC predictions", {
  expect_equal(nrow(predictionMC), 2 * 4 * 150)
  expect_equal(ncol(predictionMC), 4)
})

isThereAnyLag <- SGGetRegenerationLagIfAny(listMetaModels$mmid[1])
test_that("Check regeneration lag", {
  expect_true(is.numeric(isThereAnyLag))
})

prediction <- SGPredict("QC_5EST_MS22_NoChange_AliveVolume_AllSpecies", 0, 150, 10)
isThereAnyLag <- SGGetRegenerationLagIfAny("QC_5EST_MS22_NoChange_AliveVolume_AllSpecies")
test_that("Check predictions", {
  expect_equal(nrow(prediction), 16)
  expect_equal(ncol(prediction), 3)
  expect_true(prediction[2,"Pred"] < 0.4)
})





