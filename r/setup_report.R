# Set country name and path to your local files

country_name_ <- "Portugal"
path_to_data <- "data"
truncate_contacts_n <- 50
matrix_boots_n <- 250

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
library(ggplot2)
library(countrycode)

# library(flextable)
here::here()
ggthemr::ggthemr("grape")


map_eu_nations <- c(
  "at" =  "Austria",
  "dk" = "Denmark",
  "es" = "Spain",
  "fr" = "France",
  "it" = "Italy",
  "pl" = "Poland",
  "pt" = "Portugal"
)
# Read participants data ---------

data_files <- list.files(file.path(here::here(), path_to_data))
participants_file <- grep("part", data_files, value = T)
message(paste0("Reading from: ", participants_file))
part <- qread(file.path(here::here(), path_to_data, participants_file)) %>%
  mutate(part_age_group = ifelse(part_age_group == "70-120", "70+", part_age_group)) %>%
  mutate(country_name = map_eu_nations[country]) %>%
  filter(country_name == country_name_)


# Read contacts data -----------
contacts_file <- grep("contact", data_files, value = T)
message(paste0("Reading from: ", contacts_file))

contacts <- qread(file.path(here::here(), path_to_data, contacts_file)) %>%
  mutate(country_name = map_eu_nations[country]) %>%
  filter(country_name == country_name_)


# Prepare data -------------

contacts <- contacts %>% mutate(cnt_setting := factor(case_when(
  cnt_home == 1 ~ "Home",
  cnt_work == 1 ~ "Work",
  cnt_school == 1 ~ "School",
  cnt_other == 1 ~ "Other")

))

setting_order <-  c("Home", "Work", "School", "Other")
contacts <- contacts %>%
  mutate(cnt_setting = fct_relevel(cnt_setting, setting_order))

part_ages <- part %>% select(country, panel, wave, part_id, part_age_group)
contacts <- left_join(contacts, part_ages,
                      by = c("country", "panel", "wave", "part_id"))

trunc_contacts <- contacts %>%
  arrange(cnt_setting) %>%
  group_by(wave, part_id) %>%
  slice(1:truncate_contacts_n) %>%
  ungroup()


