library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("stratumGroup", "MS2"), SGAnd(), SGContains("outputType", "AllSpecies"))
listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)

predMS2 <- SGPredict("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 1, 140, 1)
predMS2$location <- "FMU02664"

predMS2_3Ouest <- SGPredict("QC_3OUEST_MS2_NoChange_AliveVolume_AllSpecies", 1, 140, 1)
predMS2_3Ouest$location <- "3West"

pred <- rbind(predMS2, predMS2_3Ouest)

require(ggplot2)
textsize <- 18
ggplot() +
  geom_line(aes(x=age, y=v, group = location, col = location), pred, size = 2) +
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


res <- SGPredictMC("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 1, 121, 10, 1, 500)

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


