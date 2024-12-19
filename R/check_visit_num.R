#' @title My Hello World Function
#'
#' @description
#'
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
check_and_fix_visit_num <- function(df) {
  # Sort the data by ID and Lead.Specimen.RecordedDate for proper date comparison
  df <- df[order(df$ID, df$Lead.Specimen.RecordedDate), ]

  for (subject_id in unique(df$ID)) {
    subject_data <- df[df$ID == subject_id, ]

    for (i in 1:nrow(subject_data)) {
      if(i == 1 && subject_data$Visit.Num[i] != 1){
        df$Visit.Num[which(rownames(df) == rownames(subject_data)[i])] = 1
        subject_data$Visit.Num[i] = 1
      }
      if (subject_data$Visit.Num[i] %in% subject_data$Visit.Num[c(1:(i-1), (i+1):nrow(subject_data))]) {

        if (!subject_data$Lead.Specimen.RecordedDate[i] %in% subject_data$Lead.Specimen.RecordedDate[c(1:(i-1), (i+1):nrow(subject_data))]) {
          df$Visit.Num[which(df$ID == subject_id & df$Lead.Specimen.RecordedDate == subject_data$Lead.Specimen.RecordedDate[i] )] <- subject_data$Visit.Num[i-1] + 1
          subject_data$Visit.Num[i] <- subject_data$Visit.Num[i-1] + 1

        }
      }
    }
  }

  return(df)
}

df <- check_and_fix_visit_num(df)
