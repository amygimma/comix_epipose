
# Load data -------------------
if(!exists("here")) {
  library(here)
  here::here()
}
if (!exists("country_name_") | !exists("part")) {
  source(file.path(here::here(), "r", "setup_report.R"))
}

# Set up directory ------------------

sub_folder_name <- file.path(here::here(), "outputs", country_name_)
dir.create(sub_folder_name, recursive = T, showWarnings = F)

# Simplify data -------------------
cnt_sm <- trunc_contacts %>%
  select("country", "wave", "part_id", "part_wave_uid", "cnt_age_est_min",
         "cnt_age_est_max", "cnt_gender",
         "cnt_home", "cnt_work", "cnt_school", "weekday") %>%
  rename("dayofweek" = weekday) %>%
  group_by(part_wave_uid) %>%
  mutate(cont_wave_uid = paste(part_wave_uid, row_number(), sep = "-"))

part_sm <- part %>%
  select("country", "wave", "part_id", "part_wave_uid",  "part_age_est_min", "part_age_est_max","part_gender",
         "hh_size") %>%
  mutate(part_age = NA_integer_)


# Set age limits -------------------
age_limits_ <- c(0, 5, 11, 18, 30, 40, 50, 60, 70, 120)
age_levs <- c("[0,5)", "[5,11)", "[11,18)", "[18,30)", "[30,40)", "[40,50)",
              "[50,60)", "[60,70)", "70+")
age_labels <- c("0-4", "5-11", "12-18", "18-29", "30-39", "40-49",
                "50-59", "60-69", "70+")
age_map <- age_labels
names(age_map) <- age_levs


# Get population data -------------------
# Note: Population data is from 2015, will update with newer pop estimates
pop_data <- socialmixr::wpp_age(countries = c(country_name_), years = c(2015))



# Generate full matrices, adjusted by day of week -------------------
wave_cm_dfs <- list()
for(wave_ in unique(part$wave)) {
  p_filt <- part_sm %>% filter(wave == wave_)
  c_filt <- cnt_sm %>% filter(wave == wave_)

  comix_survey <- socialmixr::survey(
    participants =   p_filt,
    contacts =   c_filt)

  comix_cm_output <- socialmixr::contact_matrix(comix_survey,
                                                # survey.pop = pop_data,
                                                age.limits = age_limits_,
                                                symmetric = F,
                                                n = matrix_boots_n,
                                                weigh.dayofweek = T,
                                                estimated.contact.age = "sample",
                                                estimated.participant.age = "sample",
                                                missing.contact.age = "sample"
  )

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
                                   function(x) {x$matrix})) / length(comix_cm_output$matrices)
  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_)
  wave_cm_dfs[[wave_]] <- cm_df
}

cm_dfs <- rbindlist(wave_cm_dfs)
table(cm_dfs$participant_age)
cm_dfs <- cm_dfs %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = age_levs, labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))

write.csv(format(cm_dfs),
          file.path(sub_folder_name, "cm_all_contacts_weighted_day.csv"),
          row.names = FALSE)

cm_plot_all <- ggplot(cm_dfs, aes(x = contact_age, y = participant_age, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile() +
  facet_grid(cols = vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_gradientn(
    colors = c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "#EEEEEE",
    values = c(0, 1, 3, 5, 12)/12,
    breaks =  seq(0,2,0.5),
    limits = c(0,2)
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

cnt_sm_adult <- cnt_sm %>%
  filter(cnt_age_est_min > 18 | is.na(cnt_age_est_min))
age_limits_ <- c(18, 30, 40, 50, 60, 70, 120)
age_levs <- c("[18,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "70+")
age_labels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")
age_map <- age_labels
names(age_map) <- age_levs
# adult_contacts <-

wave_cm_dfs_adult <- list()
mean_contacts_split_waves <- list()
for(wave_ in unique(part$wave)) {
  p_filt <- part_sm %>% filter(wave == wave_)
  c_filt <- cnt_sm_adult %>% filter(wave == wave_)

  comix_survey <- socialmixr::survey(
    participants =   p_filt,
    contacts =   c_filt)

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

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
                                   function(x) {x$matrix})) / length(comix_cm_output$matrices)

  # Create split matrices - not used in first report ---------------------------
  comix_cm_split <- socialmixr::contact_matrix(comix_survey,
                                               survey.pop = pop_data,
                                               age.limits = age_limits_,
                                               split = T,
                                               symmetric = T,
                                               n = matrix_boots_n,
                                               weigh.dayofweek = T,
                                               estimated.contact.age = "sample",
                                               estimated.participant.age = "sample",
                                               missing.contact.age = "sample"
  )

  contact_means <- unlist(lapply(
    comix_cm_split$matrices, function(cm_output) cm_output$mean.contacts))
  mean_contacts_split <- mean(contact_means)

  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_)
  wave_cm_dfs_adult[[wave_]] <- cm_df
  mean_contacts_split_waves[[wave_]] <- data.frame(
    wave = paste("Wave ", wave_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult <- rbindlist(wave_cm_dfs_adult)
mean_contacts_adult_df <- rbindlist(mean_contacts_split_waves)

cm_dfs_adult <- cm_dfs_adult %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:6, labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult$participant_age)

write.csv(format(cm_dfs_adult),
          file.path(sub_folder_name, "cm_adult_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)


cm_plot_adult <- ggplot(cm_dfs_adult, aes(x = contact_age, y = participant_age, fill = contacts)) + theme(legend.position = "bottom") +
  geom_tile() +
  facet_grid(cols = vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_gradientn(
    colors = c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "#EEEEEE",
    values = c(0, 1, 3, 5, 12)/12,
    breaks =  seq(0,2,0.5),
    limits = c(0,2)
  )  +
  coord_fixed(ratio = 1, xlim = NULL,
              ylim = NULL, expand = FALSE, clip = "off") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.text.y =  element_text(size = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


adj_mean_contacts_adult <- cm_dfs_adult %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult <- adj_mean_contacts_adult %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)



