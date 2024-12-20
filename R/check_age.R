#' @title Check Age in longitudinal data
#'
#' @description This function checks to ensure a subject's age is consistant over longitudinal data.
#'
#' @param Subj_ID the patients identifying number
#' @param df the dataframe you would like applied to this function
#' @param testdate the date the sample was collected
#' @param DOB the patients date of birth
#' @param Age the patients age
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
#' Age = c(5.58, 5.58, 8.83, 5.05, 5.05)
#' check_Age(example, "ID", "TestDate", "DOB", "Age")
#' #Notice now, this incorrect age has been corrected
#' example
Example_Dataset<-read.csv("C:/Users/tyler/Downloads/examplespreadsheet.csv")

check_age <- function(df, Subj_ID, testdate, DOB, Age) {

  # Ensure the columns are in Date format
  df[[testdate]] <- as.Date(df[[testdate]], format = "%m/%d/%Y")
  df[[DOB]] <- as.Date(df[[DOB]], format = "%m/%d/%Y")

  # Calculate the expected age based on the difference between the dates
  df$calculated_age <- round(as.numeric(difftime(df[[testdate]], df[[DOB]], units = "weeks")) / 52.25, 2)

  # Check if the calculated age matches the provided age
  df$age_check <- abs(df$calculated_age - df[[Age]]) < 0.01

  # Iterate through rows where age_check is FALSE (incorrect age)
  for (i in which(df$age_check == FALSE)) {
    # Find other rows with the same ID and correct age
    correct_rows <- df[df[[Subj_ID]] == df[[Subj_ID]][i] & df$age_check == TRUE, ]

    if (nrow(correct_rows) > 0) {
      # Replace the incorrect Birthday with the correct one (from the first row found)
      df[[DOB]][i] <- correct_rows[[DOB]][1]

      # Recalculate the age based on the corrected Birthday
      df$calculated_age[i] <- as.numeric(difftime(df[[testdate]][i], df[[DOB]][i], units = "weeks")) / 52.25
      df[[Age]][i] <- round(df$calculated_age[i], 2)
      df$age_check[i] <- TRUE  # Mark this row as having a correct age now
    }
  }
  df <- df[, !names(df) %in% c("age_check","calculated_age")]
  return(df)
}

df <- check_age(Example_Dataset, "Lead.Specimen.RecordedDate", "Birthday", "Age","ID")
