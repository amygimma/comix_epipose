# Set country name and path to your local files

country_name_ <- "Portugal"
path_to_data <- "data"

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

country_code_ <- unique(part$country)



