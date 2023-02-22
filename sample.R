library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("stratumGroup", "MS2"), SGAnd(), SGContains("outputType", "AllSpecies"))
listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)

predMS2 <- SGPredict("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
predMS2_3Ouest <- SGPredict("QC_3OUEST_MS2_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
plot(v ~ t, predMS2, ylim = c(0,250), main="MS2_FMU02664", type="l", lwd=3)
plot(v ~ t, predMS2_3Ouest, ylim = c(0,250), main="MS2_3Ouest", type="l", lwd=3)


res <- SGPredictMC("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 10, 50, 10, 1, 500)

require(ggplot2)
ggplot() +
  geom_line(aes(y=v, x=age, group=real), res)


#
# print("First Monte-Carlo prediction : ")
# print(res[[1]][[1]])
#
# print("Terminated")
