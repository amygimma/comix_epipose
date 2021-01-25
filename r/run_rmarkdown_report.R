
# Instructions:
# 1. Create a "data" folder and save contact and participant qs files to this folder
# 2. Set up the report in the file `r/setup_report.R`
# 3. Run this from the project folder by running
#    `Rscript "r/run_markdown_report.R"` in the R console.
# 4. Outputs will be saved to a directory called `outputs` and in a sub-directory
#     of the country's name (for example, `outputs/Portugal`)

source(file.path("r", "setup_report.R"))

sub_folder_name <- file.path("outputs", country_name_)
dir.create(sub_folder_name, recursive = T, showWarnings = F)

rmarkdown::render(file.path("r", "comix_base_report.Rmd"),
                  params = list(country_name = country_name),
                  output_file =
                    file.path(here::here(),
                              sub_folder_name,
                              paste("CoMix_Report_",
                              format(Sys.time(), '%d-%b-%Y'),
                              ".html", sep="")))
