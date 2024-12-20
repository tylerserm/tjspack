#' @title Check and Correct Visit Order Number in Longitudinal Data
#'
#' @description This function checks to ensure a subject's visit order number is correct over longitudinal data. It will also correct any incorrect entries.
#'
#' @param df the dataframe you would like applied to this function
#' @param Subj_ID the column name where patients identifying numbers are stored
#' @param testdate the column name where the date the sample was collected is stored
#' @param visit_num the column the visit number, the record of which instance the patient was seen, is stored
#'
#' @returns The entered dataframe now with any incorrectly entered visit order number information fixed
#' @export
#'
#' @examples
#' This example dataset has an incorrectly entered in the visit_num column. The second and third
#' #visit_num for ID 1 are accidentally reversed.
#' example <- data.frame(
#' ID = c(1, 1, 1, 2, 2),
#' TestDate = as.Date(c("2023-01-01", "2023-02-01", "2023-04-01", "2023-02-01", "2023-02-01")),
#' visit_num = c(1, 3, 2, 1, 2))
#' check_visit_order(example, "ID", "TestDate", "visit_num")
#' #Notice now, the incorrect visit order numbers have been corrected
#' example
check_visit_order <- function(df, Subj_ID, testdate, visit_num) {
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

  for (subject_id in unique(df[[Subj_ID]])) {
    subject_data <- df[df[[Subj_ID]] == subject_id, ]

    for (i in 1:nrow(subject_data)) {
      if (subject_data[[visit_num]][i] %in% subject_data[[visit_num]][c(1:(i-1), (i+1):nrow(subject_data))]) {

        if (!subject_data[[testdate]][i] %in% subject_data[[testdate]][c(1:(i-1), (i+1):nrow(subject_data))]) {
          df[[visit_num]][which(df[[Subj_ID]] == subject_id & df[[testdate]] == subject_data[[testdate]][i] )] <- subject_data[[visit_num]][i-1] + 1
        }
      }
    }
    for (n in 1:(nrow(subject_data)-1)) {
      if (nrow(subject_data) >= 2 && subject_data[[visit_num]][n] > subject_data[[visit_num]][n+1]) {
        subject_data[[visit_num]][n] = subject_data[[visit_num]][n+1] - 1
      }
    }
    full_sequence <- seq(min(subject_data[[visit_num]]), max(subject_data[[visit_num]]))

    missing_nums <- setdiff(full_sequence, subject_data[[visit_num]])

    if (length(missing_nums) > 0) {
      for (missing_num in missing_nums) {
        first_after_missing <- which(subject_data[[visit_num]] > missing_num)[1]

        for (x in which(subject_data[[visit_num]] == first_after_missing):nrow(subject_data)) {
          df[[visit_num]][which(rownames(df) == rownames(subject_data)[x])] <- subject_data[[visit_num]][x] - 1
        }
      }
    }
  }

  return(df)
}

