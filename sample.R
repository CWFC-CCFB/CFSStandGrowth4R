library("CFSStandGrowth4R")

res <- SGGetMetaModelQueryFields()

query <- SGQuery(SGContains("mmid", "FMU02664"), SGAnd(), SGContains("outputType", "Coniferous"))

res <- SGSearchMetaModels(query)

mmid <- res$mmid[1]

res <- SGPredict(mmid, 10, 50, 10)

print("Deterministic predictions : ")
print(res$predictions)

res <- SGPredictMC(mmid, 10, 50, 10, 100, 200)

print("First Monte-Carlo prediction : ")
print(res[[1]][[1]])

print("Terminated")
