library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("geoDomain", "3OUEST"), SGAnd(), SGContains("outputType", "AllSpecies"))
listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)

predv12 <- SGPredict("QC_3OUEST_STR_3O_BjR_MS_BpFx_NA_v12_NoChange_AliveVolume_AllSpecies", 1, 140, 1)
predv12$type <- "v12"
predv34 <- SGPredict("QC_3OUEST_STR_3O_BjR_MS_BpFx_NA_v34_NoChange_AliveVolume_AllSpecies", 1, 140, 1)
predv34$type <- "v34"

pred <- rbind(predv12, predv34)

require(ggplot2)
textsize <- 18
ggplot() +
  geom_line(aes(x=age, y=v, group = type, col = type), pred, size = 2) +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  theme_bw() +
  theme(text = element_text(size=textsize),
        axis.text = element_text(size=textsize),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length = unit(3,"mm"),
        legend.title = element_blank(),
        legend.text.align = 0,
        legend.text = element_text(size = textsize),
        legend.position = c(0.70,0.25),
        legend.background = element_blank(),
        panel.border = element_blank())


res <- SGPredictMC("QC_3OUEST_STR_3O_BjR_MS_BpFx_NA_v12_NoChange_AliveVolume_AllSpecies", 1, 140, 10, 1, 100)

require(ggplot2)
ggplot() +
  geom_line(aes(y=v, x=age, group=real), res, color= "gray50") +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  theme_bw() +
  theme(text = element_text(size=textsize),
        axis.text = element_text(size=textsize),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length = unit(3,"mm"),
        legend.title = element_blank(),
        legend.text.align = 0,
        legend.text = element_text(size = textsize),
        legend.position = c(0.70,0.25),
        legend.background = element_blank(),
        panel.border = element_blank())


