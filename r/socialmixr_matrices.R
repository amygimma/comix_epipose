source("r/setup_report.R")


pop_data <- pop_data %>% filter(country == country & year == 2020)

age_limits <- seq(15, 100, 5)
c_cm <- contacts %>%
  # filter(cnt_mass == "individual" ) %>%
  select("part_id", "cont_id", "cnt_age_est_min","cnt_age_est_max", "cnt_gender",
         "cnt_home", "cnt_work", "cnt_school")

p_cm <- part %>%
  select("part_id",  "part_age", "part_gender", "hh_size")
# "dayofweek")

comix_survey <- socialmixr::survey(part, c_cm)
comix_cm <- socialmixr::contact_matrix(comix_survey,
                                       survey.pop = pop_data,
                                       age.limits = age_limits_,
                                       symmetric = T)