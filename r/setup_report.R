# Sets variables, reads, & cleans data for both local environment and rmarkdown rendering

# Instructions: -----
  # If running with Knit button: Set country name andother variables below
  # If running with run_all.R script: Set country name and other variables
    # in `r/00_run_all.R` file (and ignore variables below)

# Set seed -----
set.seed(01012021)

# Set variables -----
# Important: Do not change preset_variables variable -----
if (!exists("preset_variables")) preset_variables <- FALSE

if (preset_variables == FALSE) {
  message("Setting data")
  country_name_ <- "Switzerland"

  path_to_data <- "data"
  path_to_data <- "~/../amygimma/Filr/Net Folders/EPH Shared/Comix_survey/data/clean"
  truncate_contacts_n <- 50
  matrix_boots_n <- 250
}


# Load libraries -----
library(here)
library(qs)
library(dplyr)
library(tidyr)
library(forcats)
library(knitr)
library(kableExtra)
library(data.table)
library(ggplot2)
library(ggthemr)
library(cowplot)
library(patchwork)

library(socialmixr)
library(reshape2)
library(countrycode)

if (!grepl("~/|C:/", path_to_data)) path_to_data <- here::here(path_to_data)

# library(flextable)
here::here()
# ggthemr::ggthemr("grape")


map_eu_nations <- c(
  "at" =  "Austria",
  "be" = "Belgium",
  "dk" = "Denmark",
  "es" = "Spain",
  "fr" = "France",
  "it" = "Italy",
  "pl" = "Poland",
  "pt" = "Portugal",
  "lt" = "Lithuania",
  "ch" = "Switzerland",
  "fi" = "Finland"
)

# Age levels/labels for participants
part_age_levs <- c("0-4", "5-11", "12-15", "16-17", "18-29", "30-39", "40-49",
                "50-59", "60-69", "70+")

# Read participants data ---------

# data_files <- list.files(file.path(here::here(), path_to_data))
country_code <- names(map_eu_nations[map_eu_nations == country_name_])
data_files <- list.files(file.path(path_to_data), recursive = T)
data_files <- grep(country_code, data_files, value = T)
data_files <- grep("qs", data_files, value = T)
data_files <- grep("archive|min", data_files, value = T, invert = T)

participants_file <- grep("part", data_files, value = T)
message(paste0("Reading from: ", participants_file))
# part <- qread(file.path(here::here(), path_to_data, participants_file)) %>%
part <- qread(file.path(path_to_data, participants_file)) %>%
  mutate(wave_id = paste0(panel, wave)) %>%
  mutate(part_age_group =
           ifelse(part_age_group %in% c("Under 1", "1-4"), "0-4", part_age_group)) %>%
  mutate(part_age_group = ifelse(part_age_group == "70-120", "70+", part_age_group)) %>%
  mutate(part_age_est_min = case_when(
    part_age_group == "0-4" ~ 0,
    part_age_group == "5-11" ~ 5,
    part_age_group == "12-15" ~ 12,
    part_age_group == "16-17" ~ 16,
    part_age_group == "18-29" ~ 18,
    part_age_group == "30-39" ~ 30,
    part_age_group == "40-49" ~ 40,
    part_age_group == "50-59" ~ 50,
    part_age_group == "60-69" ~ 60,
    part_age_group == "70+" ~ 70
  )) %>%
  mutate(part_age_est_max = case_when(
    part_age_group == "0-4" ~ 4,
    part_age_group == "5-11" ~ 11,
    part_age_group == "12-15" ~ 15,
    part_age_group == "16-17" ~ 17,
    part_age_group == "18-29" ~ 29,
    part_age_group == "30-39" ~ 39,
    part_age_group == "40-49" ~ 49,
    part_age_group == "50-59" ~ 59,
    part_age_group == "60-69" ~ 69,
    part_age_group == "70+" ~ 110
  )) %>%
  mutate(part_age_group = factor(part_age_group, levels = part_age_levs)) %>%
  mutate(country_name = map_eu_nations[country]) %>%
  filter(country_name == country_name_) %>%
  group_by(wave_id) %>%
  mutate(round = group_indices()) %>%
  ungroup(wave_id)



# Read contacts data -----------
# contacts_file <- grep("contact", data_files, value = T)
# contacts_file <- "contacts_v4.qs"
contacts_file <- grep("contacts", data_files, value = T)

message(paste0("Reading from: ", contacts_file))

# contacts <- qread(file.path(here::here(), path_to_data, contacts_file)) %>%
contacts <- qread(file.path(path_to_data, contacts_file)) %>%
  mutate(country_name = map_eu_nations[country]) %>%
  filter(country_name == country_name_)

if (nrow(part) == 0) stop(paste("No participants found for", country_name_))
if (nrow(contacts) == 0) stop(paste("No contacts found for", country_name_))

# Prepare data -------------
contacts <- contacts %>% mutate(cnt_any = 1)
contacts <- contacts %>% mutate(cnt_setting := factor(case_when(
  cnt_home == 1 ~ "Home",
  cnt_work == 1 ~ "Work",
  cnt_school == 1 ~ "School",
  cnt_other == 1 ~ "Other")

))
contacts <- contacts %>%
  mutate(cnt_total_time = ifelse(is.na(cnt_total_time), "Unknown", cnt_total_time))

setting_order <-  c("Home", "Work", "School", "Other")
contacts <- contacts %>%
  mutate(cnt_setting = fct_relevel(cnt_setting, setting_order)) %>%
  mutate(wave_id = paste0(panel, wave))

part_ages <- part %>% select(country, panel, wave, part_id, part_age_group)
contacts <- left_join(contacts, part_ages,
                      by = c("country", "panel", "wave", "part_id"))


trunc_contacts <- contacts %>%
  arrange(cnt_setting) %>%
  group_by(wave, part_id) %>%
  slice(1:truncate_contacts_n) %>%
  ungroup()


# Create contacts with selected participant data
byv <- c("country", "panel", "wave", "wave_id", "part_id", "part_age_group")
base_part <- part %>% select(all_of(byv))

part_contacts <- merge(base_part, contacts, all.x = T, by = byv)
trunc_part_contacts <- merge(base_part, trunc_contacts, all.x = T)

# Functions

format_n <- function(n) {
  formatC(n, 1, format = "f")
}


# Set ggtheme colors (found here: https://www.shanelynn.ie/themes-and-colours-for-r-ggplots-with-ggthemr/)

tableau_colours <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')
# Set white as outlining color
tableau_colours <- c("#555555", tableau_colours)

ggthemr_reset()
# Define colours for your figures with define_palette
tableau <- define_palette(
  swatch = tableau_colours, # colours for plotting points and bars
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]), #upper and lower colours for continuous colours
  background = "#EEEEEE" #defining a grey-ish background
)
# set the theme for your figures:
ggthemr(tableau)
# Create plots with familiar tableau look

