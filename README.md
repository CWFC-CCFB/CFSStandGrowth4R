# CFSStandGrowth4R

An R client for the CFSStandGrowth Web API

## Introduction

The CFSStandGrowth4R package makes it possible to
- query the repository of meta-models through the CFSStandGrowth Web API
- obtain predictions of a particular meta-model

## License

The CFSStandGrowth4R package is licensed under the GNU Lesser General Public License 2.1.

## Developers

- Jean-François Lavoie, Effixa inc.
- Mathieu Fortin, Canadian Wood Fibre Center

## Contact

Mathieu Fortin (mathieu.fortin@nrcan-rncan.gc.ca)

## Bug reporting

Please report any bug at https://github.com/CWFC-CCFB/CFSStandGrowth4R/issues.

## How to use it

The package can be installed using the remotes package:

~~~R
library(remotes)
remotes::install_github("CWFC-CCFB/CFSStandGrowth4R")
~~~

## Example of code

The code below provides the list of meta-models for the potential vegetation type "MS2" (balsam fir-white birch) whose response variable is the all-species volume of living trees.

~~~R
library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("stratumGroup", "MS2"), SGAnd(), SGContains("outputType", "AllSpecies"))
listMetaModels <- SGFilterMetaModels(query)
print(listMetaModels$mmid)
~~~

Each meta-model is attributed an id (mmid) which is a character string containing the following information:

- The province
- The area within the province
- The potential vegetation type
- The climate scenario
- The response variable
- The species

For instance, the following mmid "QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies" contains this information:
- The province of Quebec (QC)
- The area within the province is the forest management unit 02664 (FMU02664)
- The balsam fir-white birch potential vegetation type (MS2)
- The climate scenario (NO_CHANGE)
- The response variable is the volume of living trees (AliveVolume)
- The meta-model was fitted on the all-species volume (AllSpecies)


The goodness of fit of a particular meta-model can be visualized using the SGGOFGraph 
function. The SGPredict function provides predictions from particular meta-models as in
the folling example:

~~~R
SGGOFGraph("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies")
predMS2_FMU02664 <- SGPredict("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 1, 150, 1)

SGGOFGraph("QC_6OUEST_STR_MS2_6OUEST_NoChange_AliveVolume_AllSpecies")
predMS2_6Ouest <- SGPredict("QC_6OUEST_STR_MS2_6OUEST_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
~~~

Predictions can be easily plotted:

~~~R
predMS2_FMU02664$zone <- "FMU02664"
predMS2_6Ouest$zone <- "6Ouest"
predMS2 <- rbind(predMS2_FMU02664, predMS2_6Ouest)
require(ggplot2)
ggplot() + geom_line(aes(x=AgeYr, y=Pred, col=zone), predMS2)
~~~