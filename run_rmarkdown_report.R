
# Instructions:
# 1. Save contact and participant qs files to the `data` folder
# 2. Set up the report in the file `r/setup_report.R`
# 3. Run this from the project folder by running
#    `Rscript "r/run_markdown_report.R"` in the R console.

rmarkdown::render(file.path(here::here(), "r", "comix_base_report.Rmd"),
                  params = list(country_name = country_name),
                  output_file = paste("CoMix_Report", country_name,
                                      format(Sys.time(), '%A %d %B %Y'),
                                      ".html", sep="_"))
