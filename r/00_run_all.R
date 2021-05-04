# Runs reports for one or multiple countries from a single data file

# Instructions: -----
# If running with script (in console or terminal): Set country name and other
  # variables in `r/00_run_all.R` file
# If running with Knit button: Set country name and other variables below

source("data/pull_data.R")
# source("data/")
preset_variables <- TRUE

# Group 1 countries
# g1 <- c("Austria", "Denmark", "France", "Italy") #, "Poland", "Portugal", "Spain")
#
# # g1 <- c("Austria", "Denmark", "France", "Italy", "Poland")
#
# g2 <- c("Finland", "Greece", "Lithuania", "Switzerland", "Slovenia")

# Sample for single country
# countries <- "Denmark"
countries <- c("Spain")

path_to_data <- dir_data_clean
truncate_contacts_n <- 50
matrix_boots_n <- 1000


for (country in countries) {
  country_name_ <- country

  source(file.path("r", "run_rmarkdown_report.R"))
}
