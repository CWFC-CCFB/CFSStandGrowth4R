#'
#' Code sample for CFSStandGrowth Web API
#'

library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

#query <- SGQuery(SGContains("geoDomain", "5a"), SGAnd(), SGContains("outputType", "AllSpecies"))
query <- SGQuery(SGContains("geoDomain", "5a"))

listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)

SGGOFGraph(listMetaModels$mmid[5], ymax = 150)

predv5a <- SGPredict("QC_5a_6ab_STR_RE3_5a_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
predv5a$type <- "5a"
predv6ab <- SGPredict("QC_5a_6ab_STR_RE3_6ab_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
predv6ab$type <- "6ab"

pred <- rbind(predv5a, predv6ab)

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


res <- SGPredictMC("QC_3OUEST_STR_3O_BjR_MS_BpFx_NA_v12_NoChange_AliveVolume_AllSpecies", 1, 140, 10, 1, 500)

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


