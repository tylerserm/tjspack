#' @title Check and Correct Age in Longitudinal Data
#'
#' @description This function checks to ensure a subject's age is consistent over longitudinal data. It will also correct any incorrect ages.
#'
#' @param df the dataframe you would like applied to this function
#' @param Subj_ID the column name where patients identifying numbers are stored
#' @param testdate the column name where the date the sample was collected is stored
#' @param DOB the column name where the patient's date of birth is stored
#' @param Age the column where the patient's age is stored
#'
#' @returns The entered dataframe now with any incorrectly entered ages fixed
#' @export
#'
#' @examples
#' #This example dataset has an incorrectly entered in the Age column. The third
#' #TestDate for ID 1 was entered as 8.83, but based on the DOB this patient should only have been 5.83 years old at this visit
#' example <- data.frame(
#' ID = c(1, 1, 1, 2, 2),
#' TestDate = as.Date(c("2023-01-01", "2023-01-01", "2023-04-01", "2023-02-01", "2023-02-01")),
#' DOB = as.Date(c("2017-06-01", "2017-06-01", "2017-06-01", "2017-12-14", "2017-12-14")),
#' Age = c(5.58, 5.58, 8.83, 5.05, 5.05))
#' check_age(example, "ID", "TestDate", "DOB", "Age")
#' #Notice now, this incorrect age has been corrected
#' example
check_age <- function(df, Subj_ID, testdate, DOB, Age) {
  df[[testdate]] <- as.Date(df[[testdate]], format = "%m/%d/%Y")
  df[[DOB]] <- as.Date(df[[DOB]], format = "%m/%d/%Y")

  df$calculated_age <- round(as.numeric(difftime(df[[testdate]], df[[DOB]], units = "weeks")) / 52.25, 2)

  df$age_check <- abs(df$calculated_age - df[[Age]]) < 0.01

  for (i in which(df$age_check == FALSE)) {
    correct_rows <- df[df[[Subj_ID]] == df[[Subj_ID]][i] & df$age_check == TRUE, ]

    if (nrow(correct_rows) > 0) {
      df[[DOB]][i] <- correct_rows[[DOB]][1]

      df$calculated_age[i] <- as.numeric(difftime(df[[testdate]][i], df[[DOB]][i], units = "weeks")) / 52.25
      df[[Age]][i] <- round(df$calculated_age[i], 2)
      df$age_check[i] <- TRUE
    }
  }
  df <- df[, !names(df) %in% c("age_check","calculated_age")]
  return(df)
}
