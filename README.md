# sdtt: Simple Detection Theory Tool

## Overview
<<<<<<< HEAD
<<<<<<< HEAD
sdtt is an R package for calculating various signal detection theory (SDT) statistics. The package enables users to assign outcomes (i.e., hit, miss, correct rejection, and false alarm) to data and calculate proportions for each outcome. Currently, two measures from SDT are implemented in sdtt, *d'* and *criterion*. When calculating SDT measures, an automatic check is run to determine whether extreme values have been detected; if extreme values are detected, the user is prompted to selected a correction type (only the loglinear correction is implemented at this time) which recalculates proportions and the user defined SDT measure.
=======
sdtt is an R package for calculating various signal detection theory (SDT) statistics. The package enables users to assign outcomes (i.e., hit, miss, correct rejection, and false alarm) to data and calculate proportions for each outcome. Currently, the sensitivity measure *d'* and the response bias measures *absolute criterion* (c) and *relative criterion* (c') are implemented in sdtt. When calculating SDT measures, an automatic check is run to determine whether extreme values have been detected; if extreme values are detected, the user is prompted to selected a correction type (only the loglinear correction is implemented at this time) which recalculates proportions and the user defined SDT measure.
>>>>>>> sdtt-devel
=======
sdtt is an R package for calculating various signal detection theory (SDT) statistics. The package enables users to assign outcomes (i.e., hit, miss, correct rejection, and false alarm) to data and calculate proportions for each outcome. Currently, two measures from SDT are implemented in sdtt, *d'* and *criterion*. When calculating SDT measures, an automatic check is run to determine whether extreme values have been detected; if extreme values are detected, the user is prompted to select a correction type (only the loglinear correction is implemented at this time) which recalculates proportions and the user defined SDT measure.
>>>>>>> cc3b5448ea7050261e1112b20bf3f0bbc5790bd6

## Installation
The initial version of sdtt can be installed from [GitHub](github.com) with:

``` r

# install.packages("devtools")
devtools::install_github("s-b-moore/sdtt")
```

<<<<<<< HEAD
An updated version of sdtt is available. This involved a complete rewrite of the functions within the original sdtt package but should prove to be more user-friendly and much quicker. The development version can be installed from [GitHub](github.com) with:
=======
An updated version of sdtt is also available. This involved a complete rewrite of the functions within the original sdtt package but should prove to be more user-friendly and much quicker. The development version can be installed from [Github](github.com) with:
>>>>>>> sdtt-devel

``` r

# install.packages("devtools")
devtools::install_github("s-b-moore/sdtt", ref = "sdtt-devel")
```

## Future development
The package will be updated with additional functionality going forward, with the aim to make it as user-friendly as possible. If you have any suggestions for the package, please feel free to contact the package author/maintainer (contact details located in DESCRIPTION file).
