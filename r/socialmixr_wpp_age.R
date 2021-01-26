socialmixr_wpp_age <- function(pop, countries, years)
{
  ## circumvent R CMD CHECK errors by defining global variables
  popF <- NULL
  popM <- NULL
  sex <- NULL
  country <- NULL
  lower.age.limit <- NULL
  age <- NULL
  female <- NULL
  male <- NULL
  country_code <- NULL

  # data(popF, package = "wpp2015", envir = environment())
  # data(popM, package = "wpp2015", envir = environment())
  #
  # popM <- data.table(popM)
  # popF <- data.table(popF)
  #
  # popM <- popM[, sex := "male"]
  # popF <- popF[, sex := "female"]
  #
  # pop <- rbind(popM, popF)
  pop <- as.data.table(pop)
  #
  if (!missing(countries))
  {
    ## match by UN country code
    pop <- suppressWarnings(pop[country_code %in% countrycode(countries, "country.name", "iso3n")])
  }

  if (nrow(pop) > 0) {
    pop <- melt(pop, id.vars = c("country", "country_code", "age", "sex"), variable.name = "year")
    pop <- data.table(dcast(pop, country + country_code + age + year ~ sex, value.var = "value"))

    pop[, year := as.integer(as.character(year))]

    if (!missing(years))
    {
      if (any(pop$year %in% years))
      {
        pop <- pop[year %in% years]
      } else {
        available.years <- unique(pop$year)
        nearest.year <- available.years[which.min(abs(available.years - years))]
        warning("Don't have population data available for ", years, ". Will return nearest year (", nearest.year, ").")
        pop <- pop[year %in% nearest.year]
      }
    }

    pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
    pop <- pop[, list(country, lower.age.limit, year, population = (female + male) * 1000)]
  }

  return(as.data.frame(pop))
}
