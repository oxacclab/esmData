
# esmData

<!-- badges: start -->
![R-CMD-check](https://github.com/oxacclab/esmData/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The esmData package holds data used in Matt Jaquiery's Exploring Social Metacognition thesis.
The data is de-identified and documented.
It is not necessarily expected that anyone will want to play around with these data, or even assess the computational reproducibility of the thesis, but the package helps in streamlining the thesis files and allows alternative uses if they are wanted.

## Installation

You can install directly from GitHub using the `remotes` package:

``` r
remotes::install_github("oxacclab/esmData")
```

## Example

Once esmData is installed you can load all the data from one or other of the experimental tasks:

``` r
library(esmData)

experiment <- 'dotstask'  ## or 'datequiz'
# Load a dotstask object into the workspace holding experimental data
tada(experiment)  

```

If more specific experimental data is required (it often is!) then you can access the relevant studies directly using `select_experiment` and supplying a filtering function:

``` r
library(dplyr)  ## for filter()

select_experiment(
  project = 'datequiz',
  f = function(x) filter(x, study == 'accuracyDates', manipulationOK)
)
```
