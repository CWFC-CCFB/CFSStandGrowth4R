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

SGGetMetaData(listMetaModels$mmid[1])

#SGGOFGraph(listMetaModels$mmid[1], ymax = 200)
#a<- SGGetFinalSample(listMetaModels$mmid[1])

predv5a_RE38 <- SGPredict(listMetaModels$mmid[1], 1, 150, 1)
predv5a_RE38$ecotype <- "RE38"
predv5a_RE38$ecoreg <- "5a"

predv5a_RE39 <- SGPredict(listMetaModels$mmid[3], 1, 150, 1)
predv5a_RE39$ecotype <- "RE39"
predv5a_RE39$ecoreg <- "5a"

predv6ab_RE38 <- SGPredict(listMetaModels$mmid[2], 1, 150, 1)
predv6ab_RE38$ecotype <- "RE38"
predv6ab_RE38$ecoreg <- "6ab"

predv6ab_RE39 <- SGPredict(listMetaModels$mmid[4], 1, 150, 1)
predv6ab_RE39$ecotype <- "RE39"
predv6ab_RE39$ecoreg <- "6ab"

predv5a_RS38 <- SGPredict(listMetaModels$mmid[5], 1, 150, 1)
predv5a_RS38$ecotype <- "RS38"
predv5a_RS38$ecoreg <- "5a"




pred <- rbind(predv5a_RE38, predv5a_RE39, predv6ab_RE38, predv6ab_RE39, predv5a_RS38)

require(ggplot2)
require(Cairo)
textsize <- 18
Cairo(file="Ecoregion5a.png")
ggplot() +
  geom_line(aes(x=AgeYr, y=Pred, group = ecotype, col = ecotype), pred[which(pred$ecoreg == "5a"),], size = 2) +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylim(0,100) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  ggtitle("Ecoregion 5a") +
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
dev.off()

Cairo(file="Ecoregion6ab.png")
ggplot() +
  geom_line(aes(x=AgeYr, y=Pred, group = ecotype, col = ecotype), pred[which(pred$ecoreg == "6ab"),], size = 2) +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylim(0,100) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  ggtitle("Ecoregion 6ab") +
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
dev.off()


Cairo(file="EcotypeRE38.png")
ggplot() +
  geom_line(aes(x=AgeYr, y=Pred, group = ecoreg, col = ecoreg), pred[which(pred$ecotype == "RE38"),], size = 2) +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylim(0,100) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  ggtitle("Ecotype RE38") +
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
dev.off()

Cairo(file="EcotypeRE39.png")
ggplot() +
  geom_line(aes(x=AgeYr, y=Pred, group = ecoreg, col = ecoreg), pred[which(pred$ecotype == "RE39"),], size = 2) +
  xlab("Age (yr)") +
  xlim(0,150) +
  ylim(0,100) +
  ylab(bquote("Volume"~(m^3~ha^-1))) +
  ggtitle("Ecotype RE39") +
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
dev.off()


res <- SGPredictMC("QC_3OUEST_STR_3O_BjR_MS_BpFx_NA_v12_NoChange_AliveVolume_AllSpecies", 10, 40, 10, 1, 3)

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


