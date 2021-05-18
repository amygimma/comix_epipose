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
  #country_name_ <- "Austria"

  path_to_data <- "data"
  #path_to_data <- "C:/Users/kw/Filr/Net Folders/EPH Shared/Comix_survey/data/clean"
  truncate_contacts_n <- 50
  matrix_boots_n <- 50
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

#if (!grepl("~/|C:/", path_to_data)) path_to_data <- here::here(path_to_data)

# library(flextable)
here::here()
# ggthemr::ggthemr("grape")

map_eu_nations <- c(
  "at" =  "Austria",
  "be" = "Belgium",
  "dk" = "Denmark",
  "es" = "Spain",
  "fr" = "France",
  "gr" = "Greece",
  "it" = "Italy",
  "pl" = "Poland",
  "pt" = "Portugal",
  "lt" = "Lithuania",
  "ch" = "Switzerland",
  "fi" = "Finland",
  "si" = "Slovenia",
  "sk" = "Slovakia",
  "ee" = "Estonia",
  "hr" = "Hungary"
)


# Age levels/labels for participants
# part_age_levs <- c("0-4", "5-11", "12-15", "16-17", "18-29", "30-39", "40-49",
#                 "50-59", "60-69", "70+")
part_age_levs <- c("0-4", "5-17", "18-29", "30-39", "40-49",
                   "50-59", "60-69", "70+")

# Read participants data ---------

# data_files <- list.files(file.path(here::here(), path_to_data))
country_code <- names(map_eu_nations[map_eu_nations == country_name_])
data_files <- list.files(file.path(path_to_data), recursive = T)
data_files <- grep(paste0(country_code, "_"), data_files, value = T)
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
  mutate(part_age_group = ifelse(part_age_group %in% c("5-11", "12-15", "16-17"), "5-17", part_age_group)) %>%
  mutate(part_age_est_min = case_when(
    part_age_group == "0-4" ~ 0,
    # part_age_group == "5-11" ~ 5,
    part_age_group == "5-17" ~ 5,
    # part_age_group == "12-15" ~ 12,
    # part_age_group == "16-17" ~ 16,
    part_age_group == "18-29" ~ 18,
    part_age_group == "30-39" ~ 30,
    part_age_group == "40-49" ~ 40,
    part_age_group == "50-59" ~ 50,
    part_age_group == "60-69" ~ 60,
    part_age_group == "70+" ~ 70
  )) %>%
  mutate(part_age_est_max = case_when(
    part_age_group == "0-4" ~ 4,
    # part_age_group == "5-11" ~ 11,
    part_age_group == "5-17" ~ 17,
    # part_age_group == "12-15" ~ 15,
    # part_age_group == "16-17" ~ 17,
    part_age_group == "18-29" ~ 29,
    part_age_group == "30-39" ~ 39,
    part_age_group == "40-49" ~ 49,
    part_age_group == "50-59" ~ 59,
    part_age_group == "60-69" ~ 69,
    part_age_group == "70+" ~ 110
  )) %>%
  mutate(part_age_group = factor(part_age_group, levels = part_age_levs)) %>%
  mutate(country_name = map_eu_nations[country]) %>%
  mutate(went_to_school = case_when(panel == "A" ~ "Adult",
                                    part_attend_school_yesterday=="Yes" ~ "Yes",
                                    part_attend_school_yesterday %in% c("Don’t know", "Prefer not to answer") ~ "NA",
                                    panel == "C" & is.na(part_attend_school_yesterday) ~ "NA",
                                    !part_attend_school_yesterday %in% c("Adult", "Yes", "NA") ~ "No")) %>%
  mutate(wave_sch_id = paste0(panel, wave, " ", went_to_school)) %>%
  filter(country_name == country_name_) %>%
  group_by(wave_id) %>%
  mutate(round = cur_group_id()) %>%
  ungroup(wave_id) %>%
  arrange(survey_round)

part <- part %>%
  mutate(part_attend_school_yesterday = ifelse(part_attend_school_yesterday == "Not applicable as it was a weekend/holiday/day off", "No, it was a weekend/holiday/day off", part_attend_school_yesterday)) %>%
  mutate(part_attend_school_yesterday = ifelse(part_attend_school_yesterday == "Not applicable as it was closed", "No, it was closed", part_attend_school_yesterday)) %>%
  mutate(part_attend_school_yesterday = ifelse(went_to_school == "NA", "NA", part_attend_school_yesterday)) %>%
  mutate(went_to_school = ifelse(panel == "A", "", went_to_school))


# Check for missing age groups
adults <- part %>% filter(panel == "A")
max_adult_wave <- max(adults$wave)

age_counts <- adults %>%
  count(wave, part_age_group) %>%
  count(part_age_group) %>%
  filter(n < max_adult_wave)

if (nrow(age_counts) > 0) {
  if (isTRUE(age_counts$part_age_group == "70+")) {
    # If missing age group is only 70+, set variable to TRUE
    message("NOTICE: No participants in 70+ age group in at least one wave, regrouping to 60+")
    age_limits_ <- c(0, 5, 18, 30, 40, 50, 60, 120)
    over_70_notice <- "Due to limitations in recruitment, the oldest age group is now 60 and over rather than 70 and over."
    age_levs <- c("[0,5)", "[5,18)", "[18,30)", "[30,40)", "[40,50)",
                  "[50,60)", "60+")
    age_labels <- c("0-4", "5-17", "18-29", "30-39", "40-49",
                    "50-59", "60+")
    part <- part %>%
      mutate(part_age_group =
               ifelse(part_age_group %in% c("60-69", "70+"), "60+",
                      as.character(part_age_group)))%>%
      mutate(part_age_group = factor(part_age_group, levels = age_labels))

  } else {
    # If missing age group is not only 70 +, throw error
    stop("Missing age group under 70")
  }
} else {
  age_limits_ <- c(0, 5, 11, 18, 30, 40, 50, 60, 70, 120)
  age_levs <- c("[0,5)", "[5,11)", "[11,18)", "[18,30)", "[30,40)", "[40,50)",
                "[50,60)", "[60,70)", "70+")
  age_labels <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49",
                  "50-59", "60-69", "70+")
}

wave_id_levs <- unique(part$wave_id)
part <- part %>% mutate(wave_id = factor(wave_id, levels = wave_id_levs))

wave_sch_id_levs <- unique(part$wave_sch_id)
part <- part %>% mutate(wave_sch_id = factor(wave_sch_id, levels = wave_sch_id_levs))

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
table(contacts$cnt_total_time, useNA = "always")

contacts <- contacts %>%
  mutate(cnt_total_time = as.character(cnt_total_time)) %>%
  mutate(cnt_total_time = ifelse(is.na(cnt_total_time), "Unknown", cnt_total_time))
table(contacts$cnt_total_time, useNA = "always")
setting_order <-  c("Home", "Work", "School", "Other")
contacts <- contacts %>%
  mutate(cnt_setting = fct_relevel(cnt_setting, setting_order)) %>%
  mutate(wave_id = paste0(panel, wave))

contacts <- contacts %>% mutate(wave_id = factor(wave_id, levels = wave_id_levs))

part_ages <- part %>% select(country, panel, wave, part_id, part_age_group)
contacts <- left_join(contacts, part_ages,
                      by = c("country", "panel", "wave", "part_id"))

part_went_to_school <- part %>% select(country, panel, wave, part_id,
                                       went_to_school, part_attend_school_yesterday, wave_sch_id)
contacts <- left_join(contacts, part_went_to_school,
                      by = c("country", "panel", "wave", "part_id"))

trunc_contacts <- contacts %>%
  arrange(cnt_setting) %>%
  group_by(wave, part_id) %>%
  slice(1:truncate_contacts_n) %>%
  ungroup()

# Create contacts with selected participant data
byv <- c("country", "panel", "wave", "wave_id", "part_id", "part_age_group", "wave_sch_id")
base_part <- part %>% select(all_of(byv))

part_contacts <- merge(base_part, contacts, all.x = T, by = byv)
trunc_part_contacts <- merge(base_part, trunc_contacts, all.x = T)

# Create contacts with child sample, separate went to school or not
byv_school <- c("country", "panel", "wave", "wave_id", "part_id", "part_age_group",
                "went_to_school", "wave_sch_id")
child_part <- part %>% filter(panel == "C") %>%  select(all_of(byv_school))
child_part_contacts <- merge(child_part, contacts, all.x = T, by = byv)

trunc_child_part_contacts <- merge(child_part, trunc_contacts, all.x = T)

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
# Create plots with tableau look

