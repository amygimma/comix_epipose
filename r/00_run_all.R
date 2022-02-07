# Runs reports for one or multiple countries from a single data file

# Instructions: -----
# If running with script (in console or terminal): *Set country name* and other
  # variables (path_to_data, etc) in `r/00_run_all.R` file (this file)
# If running with Knit button: Set country name and other in setup_report.R

preset_variables <- TRUE

# Group 1 countries
g1 <- c("Austria", "Denmark", "France", "Italy", "Poland", "Portugal", "Spain")
#
# Group 2 countries
g2 <- c("Finland", "Greece", "Lithuania", "Switzerland", "Slovenia")
# Group 3 countries
#
g3 <- c("Hungary", "Slovakia", "Estonia")

# Sample for single country
# countries <- c("Spain")

countries <- c(g1,g2,g3)
countries <- "Hungary"

path_to_data <- "C:/Users/kw/Filr/Net Folders/EPH Shared/Comix_survey/data/clean"
truncate_contacts_n <- 50
matrix_boots_n <- 50


for (country in countries) {
  country_name_ <- country
  country_name_ <- "Hungary"

  source(file.path("r", "run_rmarkdown_report.R"))
}
