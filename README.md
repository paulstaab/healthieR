
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis-CI Build Status](https://travis-ci.org/paulstaab/healthieR.svg?branch=master)](https://travis-ci.org/paulstaab/healthieR) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/qesjciivdfau6baf/branch/master?svg=true)](https://ci.appveyor.com/project/paulstaab/healthier/branch/master) [![Coverage Status](https://coveralls.io/repos/github/paulstaab/healthieR/badge.svg?branch=master)](https://coveralls.io/github/paulstaab/healthieR?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/healthieR)](https://cran.r-project.org/package=healthieR)

healthieR
=========

This package imports data from Apple Health. It is usuable for basic analyses at the moment, but still in an early state. Expect that the user inferface is still subject to potentially major changes.

Installation
------------

At the moment `healthieR` is not on CRAN, but can be installed from my [drat](https://github.com/eddelbuettel/drat) repository:

``` r
install.packages("drat")
drat::addRepo("paulstaab")
install.packages("healthieR")
```

Obtain the Data
---------------

1.  Open the Apple Health App on your iPhone / iPad
2.  Go to the "Data" tab
3.  Click the little portait in the upper right corner
4.  Scroll down to "Export Data"
5.  Use Dropbox, Email or something similar to transfer the **Export.zip** to your computer

Load the Data
-------------

To import the data, enter the Path the the **Export.zip** archive here

``` r
zip_file <- "./Export.zip"
```

and load use the `read_apple_health` function to convert it into an format that we can query from `R` later:

``` r
library(healthieR)
health_data <- read_apple_health(zip_file)
health_data
#> Apple Health Export from 2017-01-25 19:11:49 
#> Containing:
#> * 71 health records
```

Analyse the Data
----------------

You can new use the extract functions to optain different parts of your data as a `data.frame`;

``` r
suppressPackageStartupMessages(library(dplyr))
health_data %>% extract_steps()
#>        type          start_time            end_time  unit value
#> 1 StepCount 2016-06-06 10:55:57 2016-06-06 10:56:12 count    22
#> 2 StepCount 2016-06-06 11:04:03 2016-06-06 11:09:55 count    78
health_data %>% extract_weight()
#>       type          start_time            end_time unit value
#> 1 BodyMass 2016-06-10 05:30:00 2016-06-10 05:30:00   kg  88.8
#> 2 BodyMass 2016-06-12 20:07:00 2016-06-12 20:07:00   kg  99.9
```
