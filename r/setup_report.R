# Set country name and path to your local files

country <- "Portugal"
country_code_ <- "pt"
path_to_folder <- "data"

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



# Read participants data ---------

part <- qread(file.path(here::here(), path_to_folder, "participants.qs"))
table(part$part_age_group)
part <- part %>% filter(country == country_code_) %>%
  mutate(part_age_group = ifelse(part_age_group == "70-120", "70+", part_age_group))
table(part$part_age_group)

table(part$country, part$wave)

# Read contacts data -----------

contacts <- qread(file.path(here::here(), path_to_folder, "contacts.qs"))
contacts <- contacts %>% filter(country == country_code_)
table(contacts$country, contacts$wave)


# Read households data ----------

# hh <- file.path(path_to_folder, "households.rds")



