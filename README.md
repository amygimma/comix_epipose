# Comix Epipose

#### Report templates for the CoMix Survey

### Instructions 

##### Recommended workflow, run with `r/00_run_all.R` script

1. Open as a project in RStudio by clicking `comix_epipose.Rproj`.
2. Add your country's data in the `data` folder in the project directory.
3. Set country name and other variables in the `r/00_run_all.R` file. 
4. Type `source("r/00_run_all.R")` into the R console from the project folder to create reports.
5. Outputs will be saved to a directory called `outputs` and in a sub-directory of the country's name (for example, `outputs/Portugal`)

\* If running with the `Knit` button in RStudio, clear workspace and set variables in the `r/setup_report.R` file. Output will be in the `r` folder.