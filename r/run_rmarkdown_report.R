
# Instructions:
# 1. Create a "data" folder and save contact and participant qs files to this folder
# 2. Set up the report in the file `r/setup_report.R`
# 3. Run this file from the project folder by running
#    `source("r/run_markdown_report.R")` in the R console.
# 4. Outputs will be saved to a directory called `outputs` and in a sub-directory
#     of the country's name (for example, `outputs/Portugal`)

source(file.path("r", "setup_report.R"))
parent_waves <- part %>% filter(sample_type == "child")
max_parent_wave <- max(parent_waves$wave)
if (max_parent_wave == 1) {
  rmd_filename <- "comix_base_report_one_parent_wave.Rmd"
# } else if (max_parent_wave == 2) {
  # Work in progress
#   rmd_filename <- "comix_base_report_two_parent_waves.Rmd"
} else {
  rmd_filename <- "comix_base_report_adult_only.Rmd"
}

sub_folder_name <- file.path("outputs", country_name_)
dir.create(sub_folder_name, recursive = T, showWarnings = F)

message(paste("starting:", countryname_dict))

rmarkdown::render(file.path("r", rmd_filename),
                  params = list(
                    preset_variables = preset_variables,
                    country_name_ = country_name_,
                    path_to_data = path_to_data,
                    truncate_contacts_n = truncate_contacts_n,
                    matrix_boots_n = matrix_boots_n
                  ),
                  output_file =
                    file.path(here::here(),
                              sub_folder_name,
                              paste("CoMix_Report_",
                                    country_name_, "_",
                                    format(Sys.time(), '%d-%b-%Y'),
                                    "_v2.html", sep="")))

message(paste("finished:", country_name_))



