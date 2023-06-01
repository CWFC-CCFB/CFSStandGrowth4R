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


Predictions from particular meta-models can be generated using the SGPredict function:

~~~R
predMS2 <- SGPredict("QC_FMU02664_MS2_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
predMS2_3Ouest <- SGPredict("QC_3OUEST_MS2_NoChange_AliveVolume_AllSpecies", 1, 150, 1)
~~~

and the predictions can be easily plotted:

~~~R
plot(v ~ age, predMS2, ylim = c(0,250), main="MS2_FMU02664", type="l", lwd=3)
plot(v ~ age, predMS2_3Ouest, ylim = c(0,250), main="MS2_3Ouest", type="l", lwd=3)
~~~