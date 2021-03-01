
forked.check.survey <- function(x, columns=FALSE, quiet=FALSE, error=FALSE, id.column="part_id", participant.age.column="part_age", country.column="country", year.column="year", contact.age.column="cnt_age", ...)
{
  if (error) error_func=stop else error_func=warning
  if (!is.data.frame(x$participants) || !is.data.frame(x$contacts))
    stop("The 'participants' and 'contacts' elements of 'x' must be data.frames")

  x <- clean(x)

  success <- TRUE
  if (!missing(columns)) {
    if (!(id.column %in% colnames(x$participants) &&
          id.column %in% colnames(x$contacts)))
    {
      error_func("id.columns '", id.column, "' does not exist in both the ",
                 "participants and contacts data frames")
      success <- FALSE
    }

    if (!(participant.age.column %in% colnames(x$participants)))
    {
      error_func("participant age column '", participant.age.column, "' does not exist ",
                 "in the participant data frame")
      success <- FALSE
    }

    if (!(contact.age.column %in% colnames(x$contacts)))
    {
      exact.column <- paste(contact.age.column, "exact", sep="_")
      min.column <- paste(contact.age.column, "est_min", sep="_")
      max.column <- paste(contact.age.column, "est_max", sep="_")

      if (!((exact.column %in% colnames(x$contacts)) ||
            (min.column %in% colnames(x$contacts) && max.column %in% colnames(x$contacts))))
      {
        error_func("contact age column '", contact.age.column,
                   "' or columns to estimate contact age ('", exact.column, "' or '",
                   min.column, "' and '", max.column, "') do not exist in the contact data frame")
        success <- FALSE
      }
    }

    if (!(country.column %in% colnames(x$participants)))
    {
      error_func("country column '", country.column, "' does not exist ",
                 "in the participant data frame")
      success <- FALSE
    }
  }

  if (!quiet) {
    if (success) message("Check OK.") else message("Check FAILED.")
  }
  invisible(c(id=id.column, participant.age=participant.age.column,
              country=country.column, year=year.column,
              contact.age=contact.age.column
  ))
}
