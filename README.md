# sdtt: Simple Detection Theory Tool

## Overview
sdtt is an R package for calculating various signal detection theory (SDT) statistics. The package enables users to assign outcomes (i.e., hit, miss, correct rejection, and false alarm) to data and calculate proportions for each outcome. Currently, the sensitivity measure *d'* and the response bias measures *absolute criterion* (c) and *relative criterion* (c') are implemented in sdtt. When calculating SDT measures, an automatic check is run to determine whether extreme values have been detected; if extreme values are detected, the user is prompted to selected a correction type (only the loglinear correction is implemented at this time) which recalculates proportions and the user defined SDT measure.

## Installation
The initial version of sdtt can be installed from [GitHub](github.com) with:

``` r

# install.packages("devtools")
devtools::install_github("s-b-moore/sdtt")
```

An updated version of sdtt is also available. This involved a complete rewrite of the functions within the original sdtt package but should prove to be more user-friendly and much quicker. The development version can be installed from [Github](github.com) with:

``` r

# install.packages("devtools")
devtools::install_github("s-b-moore/sdtt", ref = "sdtt-devel")
```

A further developmental version of sdtt is available which implements the relative criterion measure. This can be installed from [GitHub](github.com) with:

``` r

# install.packages("devtools")
devtools::install_github("s-b-moore/sdtt", ref = "relative-c")
```

## Future development
The package will be updated with additional functionality going forward, with the aim to make it as user-friendly as possible. If you have any suggestions for the package, please feel free to contact the package author/maintainer (contact details located in DESCRIPTION file).
