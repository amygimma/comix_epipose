# Runs reports for one or multiple countries from a single data file

# Instructions: -----
# If running with 00_run_all.R script: Set country name and other variables below
# If running with Knit button: Set country name and other variables in `r/00_run_all.R` file

preset_variables <- TRUE

# Group 1 countries
countries <- c("Austria", "Denmark", "France", "Italy", "Poland", "Portugal", "Spain")
# Sample for single country
# countries <- "Poland"

path_to_data <- "data"
truncate_contacts_n <- 50
matrix_boots_n <- 1000


for (country in countries) {
  country_name_ <- country

  source(file.path("r", "run_rmarkdown_report.R"))
}
