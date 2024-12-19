#' @title My Hello World Function
#'
#' @description
#'
#' @param df the dataframe you would like applied to this function
#' @param testdate the date the sample was collected
#' @param DOB the patients date of birth
#' @param Age the patients age
#'
#' @returns
#' @export
#'
#' @examples
check_age <- function(df, testdate, DOB, Age) {
  # Ensure the columns are in Date format
  df$testdate <- as.Date(df$testdate)
  df$DOB <- as.Date(df$DOB)

  # Calculate the expected age based on the difference between the dates
  df$calculated_age <- round(as.numeric(difftime(df$testdate, df$DOB, units = "weeks")) / 52.25, 2)

  # Check if the calculated age matches the provided age
  df$age_check <- abs(df$calculated_age - df$Age) < 0.01

  # Iterate through rows where age_check is FALSE (incorrect age)
  for (i in which(df$age_check == FALSE)) {
    # Find other rows with the same ID and correct age
    correct_rows <- df[df$ID == df$ID[i] & df$age_check == TRUE, ]

    if (nrow(correct_rows) > 0) {
      # Replace the incorrect Birthday with the correct one (from the first row found)
      df$DOB[i] <- correct_rows$DOB[1]

      # Recalculate the age based on the corrected Birthday
      df$calculated_age[i] <- as.numeric(difftime(df$testdate[i], df$DOB[i], units = "weeks")) / 52.25
      df$Age[i] <- round(df$calculated_age[i], 2)
      df$age_check[i] <- TRUE  # Mark this row as having a correct age now
    }
  }
  df <- df[, !names(df) %in% c("age_check","calculated_age")]
  return(df)
}

df <- check_and_fix_age(df)
