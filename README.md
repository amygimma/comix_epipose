# Comix Epipose

#### Report templates for the CoMix Survey

### Instructions 

#####

This code was written in R version 4, please use this version to avoid difficulties. 

You can download the new version of R [here](https://www.r-project.org/).

RStudio also has built-in functionality for choosing between R versions, if necessary, described [here](https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop).


##### Recommended workflow, run with `r/00_run_all.R` script

1. Open this data repository as a project in RStudio by clicking `comix_epipose.Rproj`.
2. If needed, install the packages by copying and pasting the code below or running source(file.path("r", "install_packages.R")).
3. Add your country's data in the `data` folder in the project directory.
4. Set country name and other variables in the `r/00_run_all.R` file. 
5. Type `source("r/00_run_all.R")` into the R console from the project folder to create reports.
6. Outputs will be saved to a directory called `outputs` and in a sub-directory of the country's name (for example, `outputs/Portugal`)

\* If running with the `Knit` button in RStudio, clear workspace and set variables in the `r/setup_report.R` file. Output will be in the `r` folder.


## Install packages

Copy and paste this code into your R or Rstudio console. You should only need to do this once. 


```
install.packages("countrycode")
install.packages("cowplot")
install.packages("data.table")
install.packages("dplyr")
install.packages("forcats")
install.packages("ggplot2")
install.packages("ggthemr")
install.packages("here")
install.packages("kableExtra")
install.packages("knitr")
install.packages("patchwork")
install.packages("qs")
install.packages("reshape2")
install.packages("socialmixr")
install.packages("tidyr")
```
