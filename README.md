# Comix Epipose

#### Report templates for the CoMix Survey

### Instructions 



This code was written in R version 4, please use this version to avoid difficulties. 

You can download the new version of R [here](https://www.r-project.org/).

RStudio also has built-in functionality for choosing between R versions, if necessary, described [here](https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop).


#### Recommended workflow, run with `r/00_run_all.R` script


1. Open this data repository as a project in RStudio by clicking `comix_epipose.Rproj`. 
2. If needed, install the packages by copying and pasting the code below or running `source(file.path("r", "install_packages.R"))`.
3. Add your country's data in the `data` folder in the project directory.
4. In the `r/00_run_all.R` file., set country name (`countries`) and other variables (`path_to_data`, `truncate_contacts_n`, and `matrix_boots_n`). 
5. Type `source("r/00_run_all.R")` into the R console from the project folder to create reports.
6. Outputs will be saved to a directory called `outputs` and in a sub-directory of the country's name (for example, `outputs/Portugal`)

** Please do not push any data or outputs to the repository **

\* If running with the `Knit` button in RStudio, clear workspace and set variables in the `r/setup_report.R` file. Output will be in the `r` folder.

### Contributions

If you would like to make any changes to the report, you can either do so in your local environment for your own use, or you can make a pull request for changes you would like to share with the consortium. 

Please follow these guidelines:

1. Use tidyverse packages and conventions to keep a common style to the repository.
2. Changes must be applicable to all countries or be able to handle differences between countries. The code should be able to run for all countries.
3. Summarise new code and functionality when you open the pull request.
4. Keep dependencies to a minimum, try to use the ones already in use if possible. If you add a dependency, please add the package to the README and "r/install_packages.R" file.
5. Follow this code of conduct for reviewing and receiving feedback: https://github.com/thoughtbot/guides/tree/main/code-review (thanks thoughtbot!)

### Install packages

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
