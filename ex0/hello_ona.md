hello-ona
================

Below are the codes that load the tidyverse and igraph libraries:

``` r
library("igraph")
```

    ## Warning: package 'igraph' was built under R version 4.1.2

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::as_data_frame() masks tibble::as_data_frame(), igraph::as_data_frame()
    ## x purrr::compose()       masks igraph::compose()
    ## x tidyr::crossing()      masks igraph::crossing()
    ## x dplyr::filter()        masks stats::filter()
    ## x dplyr::groups()        masks igraph::groups()
    ## x dplyr::lag()           masks stats::lag()
    ## x purrr::simplify()      masks igraph::simplify()

Hello, network!
