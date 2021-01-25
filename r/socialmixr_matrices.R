source("r/setup_report.R")

age_limits_ <- c(18, 30, 40, 50, 60, 70, 120)
pop_data <- socialmixr::wpp_age(countries = c(country_name_), years = c(2020))
# pop_data <- pop_data %>% filter(country == country_name_ & year == 2020)
# pop_data <- pop_data %>% filter(country == country & year == 2020)


# age_limits <- seq(15, 100, 5)
c_cm <- trunc_contacts %>%
  # filter(cnt_mass == "individual" ) %>%
  select("part_id", "part_wave_uid", "cnt_age_est_min","cnt_age_est_max", "cnt_gender",
         "cnt_home", "cnt_work", "cnt_school") %>%
  group_by(part_wave_uid) %>%
  mutate(cont_wave_uid = paste(part_wave_uid, row_number(), sep = "-"))

p_cm <- part %>%
  select("part_id", "part_wave_uid",  "part_age", "part_gender", "hh_size", "weekday") %>%
  rename("dayofweek" = weekday)
# "dayofweek")

bootstrap_n <- 250
comix_survey <- socialmixr::survey(part, c_cm)
t <- Sys.Date()
comix_cm <- socialmixr::contact_matrix(comix_survey,
                                       survey.pop = pop_data,
                                       age.limits = age_limits_,
                                       # symmetric = T,
                                       n = 250,
                                       weigh.dayofweek = T,


                                      )





Sys.Date() - t


