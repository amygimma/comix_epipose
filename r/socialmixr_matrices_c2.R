
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
  select("country", "panel", "wave", "wave_id", "part_id", "part_wave_uid","went_to_school",
         "cnt_age_est_min", "cnt_age_est_max", "cnt_gender", "survey_round",
         "cnt_home", "cnt_work", "cnt_school", "weekday") %>%
  rename("dayofweek" = weekday) %>%
  group_by(part_wave_uid) %>%
  mutate(cont_wave_uid = paste(part_wave_uid, row_number(), sep = "-"))

part_sm <- part %>%
  select("country", "panel", "wave", "wave_id", "part_id", "part_wave_uid",  "part_age_group",
         "part_age_est_min", "part_age_est_max","part_gender", "survey_round",
         "hh_size", "went_to_school") %>%
  mutate(part_age = NA_integer_)


# Set age limits -------------------
# age_limits_ <- c(0, 5, 11, 18, 30, 40, 50, 60, 70, 120)
# age_levs <- c("[0,5)", "[5,11)", "[11,18)", "[18,30)", "[30,40)", "[40,50)",
#               "[50,60)", "[60,70)", "70+")
# age_labels <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49",
#                 "50-59", "60-69", "70+")

age_map <- age_labels
names(age_map) <- age_levs
table(part$part_age_group, useNA = "always")

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
cm_plot_all <- ggplot(cm_dfs, aes(x = contact_age, y = participant_age, fill = contacts)) +
  theme(legend.position = "bottom") +
  geom_tile() +
  facet_wrap(vars(wave_id)) +
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
#adult_contacts <-

#  Scenario 1: Combine adults with wave C1 only--------------------------
wave_cm_dfs_adult_c1 <- list()
mean_contacts_split_waves <- list()
part_sm_adult_c1 <- part_sm %>% filter(panel %in% c("A","B"))
cnt_sm_adult_c1 <- cnt_sm %>% filter(panel %in% c("A","B"))

for(wave_id_ in unique(part_sm_adult_c1$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id %in% c(wave_id_, "C1"))
  c_filt <- cnt_sm %>% filter(wave_id %in% c(wave_id_, "C1"))

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

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
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

  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_c1[[wave_id_]] <- cm_df
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_c1 <- rbindlist(wave_cm_dfs_adult_c1)
mean_contacts_adult_c1_df <- rbindlist(mean_contacts_split_waves)

max_age_length <- length(age_labels)
cm_dfs_adult_c1 <- cm_dfs_adult_c1 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:max_age_length , labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_c1$participant_age)

write.csv(format(cm_dfs_adult_c1),
          file.path(sub_folder_name, "cm_adult_c1_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)

adj_mean_contacts_adult_c1 <- cm_dfs_adult_c1 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_c1 <- adj_mean_contacts_adult_c1 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)




#  Scenario 2: Combine adults with wave C2 only --------------------------
wave_cm_dfs_adult_c2 <- list()
mean_contacts_split_waves <- list()
part_sm_adult_c2 <- part_sm %>% filter(panel %in% c("A","B"))
cnt_sm_adult_c2 <- cnt_sm %>% filter(panel %in% c("A","B"))

for(wave_id_ in unique(part_sm_adult_c2$wave_id)) {
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

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
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

  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_c2[[wave_id_]] <- cm_df
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_c2 <- rbindlist(wave_cm_dfs_adult_c2)
mean_contacts_adult_c2_df <- rbindlist(mean_contacts_split_waves)

max_age_length <- length(age_labels)
cm_dfs_adult_c2 <- cm_dfs_adult_c2 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:max_age_length , labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_c2$participant_age)

write.csv(format(cm_dfs_adult_c2),
          file.path(sub_folder_name, "cm_adult_c2_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)


adj_mean_contacts_adult_c2 <- cm_dfs_adult_c2 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_c2 <- adj_mean_contacts_adult_c2 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)



#  Scenario 3: Combine adults with children who went to school
wave_cm_dfs_adult_s3 <- list()
mean_contacts_split_waves <- list()
part_sm_adult_s3 <- part_sm %>% filter(panel %in% c("A","B"))
cnt_sm_adult_s3 <- cnt_sm %>% filter(panel %in% c("A","B"))

for(wave_id_ in unique(part_sm_adult_s3$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id %in% c(wave_id_) | went_to_school == "Yes")
  c_filt <- cnt_sm %>% filter(wave_id %in% c(wave_id_) | went_to_school == "Yes")

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

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
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

  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_s3[[wave_id_]] <- cm_df
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_s3 <- rbindlist(wave_cm_dfs_adult_s3)
mean_contacts_adult_s3_df <- rbindlist(mean_contacts_split_waves)

max_age_length <- length(age_labels)
cm_dfs_adult_s3 <- cm_dfs_adult_s3 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:max_age_length , labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_s3$participant_age)

write.csv(format(cm_dfs_adult_s3),
          file.path(sub_folder_name, "cm_adult_s3_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)


adj_mean_contacts_adult_s3 <- cm_dfs_adult_s3 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_s3 <- adj_mean_contacts_adult_s3 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)





#  Scenario 4: Combine adults with children who did not attend school
wave_cm_dfs_adult_s4 <- list()
mean_contacts_split_waves <- list()
part_sm_adult_s4 <- part_sm %>% filter(panel %in% c("A","B"))
cnt_sm_adult_s4 <- cnt_sm %>% filter(panel %in% c("A","B"))

for(wave_id_ in unique(part_sm_adult_s4$wave_id)) {
  p_filt <- part_sm %>% filter(wave_id %in% c(wave_id_) | went_to_school == "No")
  c_filt <- cnt_sm %>% filter(wave_id %in% c(wave_id_) | went_to_school == "No")

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

  reduced_cm <- Reduce("+", lapply(comix_cm_output$matrices,
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

  cm_df <- melt(reduced_cm, varnames = c("participant_age", "contact_age"), value.name = "contacts")
  cm_df <- cm_df %>% mutate(wave = wave_id_)
  wave_cm_dfs_adult_s4[[wave_id_]] <- cm_df
  # mean_contacts_split_waves[[wave_id_]] <- data.frame(
  #   wave = paste("Wave ", wave_id_), mean_contacts = mean_contacts_split)

}

cm_dfs_adult_s4 <- rbindlist(wave_cm_dfs_adult_s4)
mean_contacts_adult_s4_df <- rbindlist(mean_contacts_split_waves)

max_age_length <- length(age_labels)
cm_dfs_adult_s4 <- cm_dfs_adult_s4 %>%
  mutate(wave = paste("Wave", wave),
         participant_age = factor(participant_age, levels = 1:max_age_length , labels = age_labels),
         contact_age = factor(contact_age, levels = age_levs, labels = age_labels))
table(cm_dfs_adult_s4$participant_age)

write.csv(format(cm_dfs_adult_s4),
          file.path(sub_folder_name, "cm_adult_s4_contacts_weighted_day_pop_symmetric.csv"),
          row.names = FALSE)


adj_mean_contacts_adult_s4 <- cm_dfs_adult_s4 %>%
  group_by(wave, participant_age) %>%
  summarise(mean_contacts = sum(contacts))


adj_mean_contacts_table_adult_s4 <- adj_mean_contacts_adult_s4 %>%
  pivot_wider(names_from = wave,
              values_from = mean_contacts) %>%
  rename("Participant age" =  participant_age)


# all the CM plots are here
max_cell <- ceiling(max(max(cm_dfs_adult_c1$contacts, na.rm = T),
                    max(cm_dfs_adult_c2$contacts, na.rm = T),
                    max(cm_dfs_adult_s3$contacts, na.rm = T),
                    max(cm_dfs_adult_s4$contacts, na.rm = T)))
#break_points <- ifelse(max_cell > 3, 1, 0.5)

cm_plot_adult_c1 <- ggplot(cm_dfs_adult_c1, aes(x = contact_age, y = participant_age, fill = contacts)) +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_viridis(discrete=FALSE, name='Mean contacts',
                     limits = c(0.01,max_cell), trans='log',
                     breaks=c(0.01,0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2., 5., max_cell),
                     na.value='black') +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = FALSE, clip = "off") +
  theme_bw() +
  ggtitle("C. Combined with \nC1 only") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size=16)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(5, 'cm'))


cm_plot_adult_c2 <- ggplot(cm_dfs_adult_c2, aes(x = contact_age, y = participant_age, fill = contacts)) +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_viridis(discrete=FALSE, name='Mean contacts',
                     limits = c(0.01,max_cell), trans='log',
                     breaks=c(0.01,0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2., 5., max_cell),
                     na.value='black') +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = FALSE, clip = "off") +
  theme_bw() +
  ggtitle("D. Combined with \nC2 only") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size=16)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(5, 'cm'))


cm_plot_adult_s3 <-
  ggplot(cm_dfs_adult_s3, aes(x = contact_age, y = participant_age, fill = contacts)) +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_viridis(discrete=FALSE, name='Mean contacts',
                     limits = c(0.01, max_cell), trans='log',
                     breaks=c(0.01, 0.01,0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2., 5., max_cell),
                     na.value='black') +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = FALSE, clip = "off") +
  theme_bw() +
  ggtitle("A. Combined with \nchildren who attended school") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size=16)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(5, 'cm'))


cm_plot_adult_s4 <-
  ggplot(cm_dfs_adult_s4, aes(x = contact_age, y = participant_age, fill = contacts)) +
  geom_tile() +
  facet_wrap(vars(wave)) +
  labs(
    y = "Age of participant",
    x = "Age of contact",
    fill = "Contacts"
  ) +
  scale_fill_viridis(discrete=FALSE, name='Mean contacts',
                     limits = c(0.01,max_cell), trans='log',
                     breaks=c(0.01,0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2., 5., max_cell),
                     na.value="black") +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = FALSE, clip = "off") +
  theme_bw() +
  ggtitle("B. Combined with \nchildren who did not go to school") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text = element_text(size=16)) +
  theme(legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(5, 'cm'))
