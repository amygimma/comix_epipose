
# Load paths and functions -------------------
if(!exists("here")) {
  library(here)
  here::here()
}

source(here::here("r", "socialmixr_forked", "forked_contact_matrix.R"))
source(here::here("r", "socialmixr_forked", "forked_check_survey.R"))

if (!exists("country_name_") | !exists("part")) {
  source(file.path(here::here(), "r", "setup_report.R"))
}

# Set up directory ------------------

sub_folder_name <- file.path(here::here(), "outputs", country_name_)
dir.create(sub_folder_name, recursive = T, showWarnings = F)

# Simplify data -------------------
cnt_sm <- trunc_contacts %>%
  select("country", "panel", "wave", "wave_id", "part_id", "part_wave_uid", "cnt_age_est_min",
         "cnt_age_est_max", "cnt_gender",
         "cnt_home", "cnt_work", "cnt_school", "weekday") %>%
  rename("dayofweek" = weekday) %>%
  group_by(part_wave_uid) %>%
  mutate(cont_wave_uid = paste(part_wave_uid, row_number(), sep = "-"))

part_sm <- part %>%
  select("country", "panel", "wave", "wave_id", "part_id", "part_wave_uid",  "part_age_est_min", "part_age_est_max","part_gender",
         "hh_size") %>%
  mutate(part_age = NA_integer_)


# Set age limits -------------------
# age_limits_ <- c(0, 5, 11, 18, 30, 40, 50, 60, 70, 120)
# age_levs <- c("[0,5)", "[5,11)", "[11,18)", "[18,30)", "[30,40)", "[40,50)",
#               "[50,60)", "[60,70)", "70+")
# age_labels <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49",
#                 "50-59", "60-69", "70+")

table(part$part_age_group)
age_map <- age_labels
names(age_map) <- age_levs


# Get population data -------------------
# Note: Population data is from 2015, will update with newer pop estimates
pop_data <- socialmixr::wpp_age(countries = c(country_name_), years = c(2015))



# Generate full matrices, adjusted by day of week -------------------
wave_cm_dfs <- list()
for(wave_id_ in unique(part$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id == wave_id_)
  c_filt <- cnt_sm %>% filter(wave_id == wave_id_)

  comix_survey <- socialmixr::survey(
    participants =   p_filt,
    contacts =   c_filt)
  if ("C" %in% unique(p_filt$panel)) {
    # Adds keep.all.age.groups argument to fill matrices to the highest age group
    comix_cm_output <- forked_contact_matrix(comix_survey,
                                             # survey.pop = pop_data,
                                             age.limits = age_limits_,
                                             symmetric = F,
                                             n = matrix_boots_n,
                                             weigh.dayofweek = T,
                                             estimated.contact.age = "sample",
                                             estimated.participant.age = "sample",
                                             missing.contact.age = "sample",
                                             keep.all.age.groups = T )
  } else {
    comix_cm_output <- socialmixr::contact_matrix(comix_survey,
                                                  # survey.pop = pop_data,
                                                  age.limits = age_limits_,
                                                  symmetric = F,
                                                  n = matrix_boots_n,
                                                  weigh.dayofweek = T,
                                                  estimated.contact.age = "sample",
                                                  estimated.participant.age = "sample",
                                                  missing.contact.age = "sample")
  }

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
                                   function(x) {x$matrix})) / length(comix_cm_output$matrices)
  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave_id = wave_id_)
  wave_cm_dfs[[wave_id_]] <- cm_df
}

cm_dfs <- rbindlist(wave_cm_dfs)
table(cm_dfs$participant_age)
cm_dfs <- cm_dfs %>%
  mutate(wave = paste("Wave", wave_id),
         participant_age = factor(participant_age, levels = age_levs, labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))

write.csv(format(cm_dfs),
          file.path(sub_folder_name, "cm_all_contacts_weighted_day.csv"),
          row.names = FALSE)

max_cell <- ceiling(max(cm_dfs$contacts, na.rm = T))
break_points <- ifelse(max_cell > 3, 1, 0.5)
cm_plot_all <- ggplot(cm_dfs, aes(x = contact_age, y = participant_age, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_gradientn(
    colors = c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "#EEEEEE",
    values = c(0, 1, 3, 5, 12)/12,
    breaks =  seq(0,max_cell,break_points),
    limits = c(0,max_cell)
  )  +
  coord_fixed(ratio = 1, xlim = NULL,
              ylim = NULL, expand = FALSE, clip = "off") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.text.y =  element_text(size = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


adj_mean_contacts_all <- cm_dfs %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_all <- adj_mean_contacts_all %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)


# Generate adult matrices, adjusted by day of week and population --------------

# cnt_sm_adult <- cnt_sm %>%
#   filter(cnt_age_est_min > 18 | is.na(cnt_age_est_min))
# age_limits_ <- c(18, 30, 40, 50, 60, 70, 120)
# age_levs <- c("[18,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "70+")
# age_labels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")
# age_map <- age_labels
# names(age_map) <- age_levs
# adult_contacts <-

#  Scenario 1: Combine adults with wave C1 --------------------------
wave_cm_dfs_adult_c1 <- list()
mean_contacts_split_waves <- list()
part_sm_adult <- part_sm %>% filter(panel != "C")
cnt_sm_adult <- cnt_sm %>% filter(panel != "C")

for(wave_id_ in unique(part_sm_adult$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id %in% c(wave_id_, "C1"))
  c_filt <- cnt_sm %>% filter(wave_id %in% c(wave_id_, "C1"))

  comix_survey <- socialmixr::survey(
    participants =   p_filt,
    contacts =   c_filt)
  # browser()
  # use population data and create symmetric matrices
  comix_cm_output_c1 <- socialmixr::contact_matrix(comix_survey,
                                                survey.pop = pop_data,
                                                age.limits = age_limits_,
                                                symmetric = T,
                                                n = matrix_boots_n,
                                                weigh.dayofweek = T,
                                                estimated.contact.age = "sample",
                                                estimated.participant.age = "sample",
                                                missing.contact.age = "sample"
  )

  reduced_cm_c1 <- Reduce("+", lapply(comix_cm_output_c1$matrices,
                                      function(x) {x$matrix})) / length(comix_cm_output$matrices)

  # Create split matrices - not used in first report ---------------------------
  # comix_cm_split <- socialmixr::contact_matrix(comix_survey,
  #                                              survey.pop = pop_data,
  #                                              age.limits = age_limits_,
  #                                              split = T,
  #                                              symmetric = T,
  #                                              n = matrix_boots_n,
  #                                              weigh.dayofweek = T,
  #                                              estimated.contact.age = "sample",
  #                                              estimated.participant.age = "sample",
  #                                              missing.contact.age = "sample"
  # )

  # contact_means <- unlist(lapply(
  #   comix_cm_split$matrices, function(cm_output) cm_output$mean.contacts))
  # mean_contacts_split <- mean(contact_means)

  cm_df_c1 <- melt(reduced_cm_c1, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df_c1 <- cm_df_c1 %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_c1[[wave_id_]] <- cm_df_c1
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_c1 <- rbindlist(wave_cm_dfs_adult_c1)
mean_contacts_adult_df_c1 <- rbindlist(mean_contacts_split_waves)

cm_dfs_adult_c1 <- cm_dfs_adult_c1 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:9, labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_c1$participant_age)

write.csv(format(cm_dfs_adult_c1),
          file.path(sub_folder_name, "cm_adult_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)

max_cell <- ceiling(max(cm_dfs_adult_c1$contacts, na.rm = T))
break_points <- ifelse(max_cell > 3, 1, 0.5)
cm_plot_adult_c1 <- ggplot(cm_dfs_adult_c1, aes(x = contact_age, y = participant_age, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_gradientn(
    colors = c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "#EEEEEE",
    values = c(0, 1, 3, 5, 12)/12,
    breaks =  seq(0,max_cell,break_points),
    limits = c(0,max_cell)
  )  +
  coord_fixed(ratio = 1, xlim = NULL,
              ylim = NULL, expand = FALSE, clip = "off") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.text.y =  element_text(size = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


adj_mean_contacts_adult_c1 <- cm_dfs_adult_c1 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_c1 <- adj_mean_contacts_adult_c1 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)



#  Scenario 1: Combine adults with wave C2 --------------------------
wave_cm_dfs_adult_c2 <- list()
mean_contacts_split_waves_c2 <- list()
part_sm_adult <- part_sm %>% filter(panel != "C")
cnt_sm_adult <- cnt_sm %>% filter(panel != "C")

for(wave_id_ in unique(part_sm_adult$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id %in% c(wave_id_, "C2"))
  c_filt <- cnt_sm %>% filter(wave_id %in% c(wave_id_, "C2"))

  comix_survey <- socialmixr::survey(
    participants =   p_filt,
    contacts =   c_filt)
  # browser()
  # use population data and create symmetric matrices
  comix_cm_output <- socialmixr::contact_matrix(comix_survey,
                                                survey.pop = pop_data,
                                                age.limits = age_limits_,
                                                symmetric = T,
                                                n = matrix_boots_n,
                                                weigh.dayofweek = T,
                                                estimated.contact.age = "sample",
                                                estimated.participant.age = "sample",
                                                missing.contact.age = "sample"
  )

  reduced_cm_c2 <- Reduce("+", lapply(comix_cm_output$matrices,
                                      function(x) {x$matrix})) / length(comix_cm_output$matrices)

  # Create split matrices - not used in first report ---------------------------
  # comix_cm_split <- socialmixr::contact_matrix(comix_survey,
  #                                              survey.pop = pop_data,
  #                                              age.limits = age_limits_,
  #                                              split = T,
  #                                              symmetric = T,
  #                                              n = matrix_boots_n,
  #                                              weigh.dayofweek = T,
  #                                              estimated.contact.age = "sample",
  #                                              estimated.participant.age = "sample",
  #                                              missing.contact.age = "sample"
  # )

  # contact_means <- unlist(lapply(
  #   comix_cm_split$matrices, function(cm_output) cm_output$mean.contacts))
  # mean_contacts_split <- mean(contact_means)

  cm_df_c2 <- melt(reduced_cm_c2, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df_c2 <- cm_df_c2 %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_c2[[wave_id_]] <- cm_df_c2
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_c2 <- rbindlist(wave_cm_dfs_adult_c2)
mean_contacts_adult_df_c2 <- rbindlist(mean_contacts_split_waves_c2)

cm_dfs_adult_c2 <- cm_dfs_adult_c2 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:9, labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_c2$participant_age)

write.csv(format(cm_dfs_adult_c2),
          file.path(sub_folder_name, "cm_adult_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)

max_cell <- ceiling(max(cm_dfs_adult_c2$contacts, na.rm = T))
break_points <- ifelse(max_cell > 3, 1, 0.5)
cm_plot_adult_c2 <- ggplot(cm_dfs_adult_c2, aes(x = contact_age, y = participant_age, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_gradientn(
    colors = c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "#EEEEEE",
    values = c(0, 1, 3, 5, 12)/12,
    breaks =  seq(0,max_cell,break_points),
    limits = c(0,max_cell)
  )  +
  coord_fixed(ratio = 1, xlim = NULL,
              ylim = NULL, expand = FALSE, clip = "off") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.text.y =  element_text(size = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


adj_mean_contacts_adult_c2 <- cm_dfs_adult_c2 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_c2 <- adj_mean_contacts_adult_c2 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)

