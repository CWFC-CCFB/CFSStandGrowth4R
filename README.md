[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) 

# CFSStandGrowth4R

An R client for the CFSStandGrowth Web API

## Introduction

The CFSStandGrowth4R package makes it possible to
- query the repository of meta-models through the CFSStandGrowth Web API
- obtain predictions of a particular meta-model

## License

The CFSStandGrowth4R package is licensed under the GNU Lesser General Public License 3 (LGPL-3).

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

The code below provides the list of meta-models for the ecological type "MS22" (balsam fir-white birch on mesic sites) whose response variable is the all-species volume of living trees.

~~~R
library("CFSStandGrowth4R")

fields <- SGGetMetaModelQueryFields()
print(fields)

query <- SGQuery(SGContains("stratumGroup", "MS22"), SGAnd(), SGContains("outputType", "AllSpecies"))
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

For instance, the following mmid "QC_4OUEST_MS22_NoChange_AliveVolume_AllSpecies" contains this information:
- The province of Quebec (QC)
- The area within the province is the bioclimatic subdomain 4OUEST
- The balsam fir-white birch potential ecological type (MS22)
- The climate scenario (NO_CHANGE)
- The response variable is the volume of living trees (AliveVolume)
- The meta-model was fitted on the all-species volume (AllSpecies)


The goodness of fit of a particular meta-model can be visualized using the SGGOFGraph 
function. The SGPredict function provides predictions from particular meta-models as in
the following example:

~~~R
SGGOFGraph("QC_4OUEST_MS22_NoChange_AliveVolume_AllSpecies")
predMS22 <- SGPredict("QC_4OUEST_MS22_NoChange_AliveVolume_AllSpecies", 1, 150)
~~~

