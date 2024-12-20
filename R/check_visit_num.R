#' @title Check and Correct Visit Number in Longitudinal Data
#'
#' @description This function checks to ensure a subject's visit number is correct over longitudinal data. It will also correct any incorrect entries.
#'
#' @param df the dataframe you would like applied to this function
#' @param Subj_ID the patients identifying number
#' @param testdate the date the sample was collected
#' @param visit_num the visit number, the record of which instance the patient was seen
#'
#' @returns The entered dataframe now with any incorrectly entered visit number information fixed
#' @export
#'
#' @examples
#' This example dataset has an incorrectly entered in the visit_num column. The second and third
#' #visit_num for ID 1 was entered as 1, but there obviously can't be three tests from visit 1 on different days
#' example <- data.frame(
#' ID = c(1, 1, 1, 2, 2),
#' TestDate = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-02-01", "2023-02-01")),
#' visit_num = c(1, 1, 1, 1, 2)
#' check_Age(example, "ID", "TestDate", "DOB", "Age")
#' #Notice now, the incorrect visit numbers have been corrected
#' example
check_visit_num <- function(df, Subj_ID, testdate, visit_num) {
  df <- df[order(df[[Subj_ID]], df[[testdate]]), ]

  for (subject_id in unique(df[[Subj_ID]])) {
    subject_data <- df[df[[Subj_ID]] == subject_id, ]

    for (i in 1:nrow(subject_data)) {
      if(i == 1 && subject_data[[visit_num]][i] != 1){
        df[[visit_num]][which(rownames(df) == rownames(subject_data)[i])] = 1
        subject_data[[visit_num]][i] = 1
      }
      if (subject_data[[visit_num]][i] %in% subject_data[[visit_num]][c(1:(i-1), (i+1):nrow(subject_data))]) {

        if (!subject_data[[testdate]][i] %in% subject_data[[testdate]][c(1:(i-1), (i+1):nrow(subject_data))]) {
          df[[visit_num]][which(df[[Subj_ID]] == subject_id & df[[testdate]] == subject_data[[testdate]][i] )] <- subject_data[[visit_num]][i-1] + 1
          subject_data[[visit_num]][i] <- subject_data[[visit_num]][i-1] + 1

        }
      }
    }
  }

  return(df)
}
