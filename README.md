# Applied Epi - fake COVID linelist

A report dashboard constructed with mock linelist data from [Applied Epi](https://github.com/appliedepi/epiRhandbook_eng/tree/master/data/covid_example_data). 

## Project description

With a csv formatted COVID-19 linelist, this analysis checks the data for errors and constructs the following outputs:

* Rolling graph of cumulative cases and hospitalisations from the last six months

* Summary table of new cases and hospitalisations from the last 7 days

* A plotly map of new cases from the last 7 days with locations

* Demographic tables of cases from the last 6 months broken down by age, gender, race and ethnicity

* A graph of the relative percentages of the most common symptoms in COVID cases from the last 6 months. 

## Requirements

This dashboard has been constructed entirely in R and Rmarkdown. The following packages, available on CRAN are required:

- tidyverse
- readxl
- here
- runner
- lubridate
- ggmap
- osmdata
- plotly
- showtext
- gt
- gtsummary
- apyramid
- reactablefmtr
