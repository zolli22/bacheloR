Untitled
================

# The `bacheloR` Package <img src="figs/hex-bacheloR.png" width="160px" align="right" />

Created by Ingrid Zoll

# About the package

*This package is still under construction!*  
`bacheloR` will contain data relating to and color palettes inspired by
the TV shows The Bachelor and The Bachelorette.

# Color Palettes

Currently, `bacheloR` contains two color palettes: `first flower` and
`final rose`.

<img src="README_files/figure-gfm/unnamed-chunk-1-1.png" width="50%" /><img src="README_files/figure-gfm/unnamed-chunk-1-2.png" width="50%" />

# Installation

``` r
library(tidyverse)

# devtools::install_github("zolli22/bacheloR")
library(bacheloR)
```

# Usage

``` r
firstflower_cols()
#>     Carmine red     Medium pink      Blush pink       Pale pink        Honeydew 
#>       "#ff4772"       "#fa89a2"       "#feaeca"       "#eed3d6"       "#e8f4e1" 
#>      Mint green Turquoise green       Sap green   Peacock green 
#>       "#b2edc5"       "#a5d1b4"       "#71a989"       "#428a5f"
finalrose_cols()
#>     Dark red      Crimson    Poppy red       Salmon  Light peach     Shamrock 
#>    "#7a001d"    "#b0072c"    "#eb4833"    "#ff8170"    "#e2b5a2"    "#67a282" 
#>    Pine tree Forest green   Deep green 
#>    "#457359"    "#285339"    "#214540"
```

``` r
finalrose_palettes
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="50%" /><img src="README_files/figure-gfm/unnamed-chunk-5-2.png" width="50%" />
