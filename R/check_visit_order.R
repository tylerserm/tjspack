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
check_and_fix_visit_num_order <- function(df) {
  # Sort the data by ID and Lead.Specimen.RecordedDate for proper date comparison
  df <- df[order(df$ID, df$Lead.Specimen.RecordedDate), ]

  for (subject_id in unique(df$ID)) {
    subject_data <- df[df$ID == subject_id, ]

    for (i in 1:nrow(subject_data)) {
      if (subject_data$Visit.Num[i] %in% subject_data$Visit.Num[c(1:(i-1), (i+1):nrow(subject_data))]) {

        if (!subject_data$Lead.Specimen.RecordedDate[i] %in% subject_data$Lead.Specimen.RecordedDate[c(1:(i-1), (i+1):nrow(subject_data))]) {
          df$Visit.Num[which(df$ID == subject_id & df$Lead.Specimen.RecordedDate == subject_data$Lead.Specimen.RecordedDate[i] )] <- subject_data$Visit.Num[i-1] + 1
        }
      }
    }
    for (n in 1:(nrow(subject_data)-1)) {
      if (nrow(subject_data)>=2 && subject_data$Visit.Num[n]>subject_data$Visit.Num[n+1]){
        subject_data$Visit.Num[n] = subject_data$Visit.Num[n+1] - 1
      }
    }
    full_sequence <- seq(min(subject_data$Visit.Num), max(subject_data$Visit.Num))

    # Check for missing numbers in the Visit.Num sequence
    missing_nums <- setdiff(full_sequence, subject_data$Visit.Num)

    if (length(missing_nums) > 0) {
      # If there are missing numbers, start adjusting Visit.Num from the first missing number
      for (missing_num in missing_nums) {
        # Find the first row after the missing number
        first_after_missing <- which(subject_data$Visit.Num > missing_num)[1]

        # Adjust Visit.Num starting from that row onward
        for (x in which(subject_data$Visit.Num == first_after_missing):nrow(subject_data) ){
          df$Visit.Num[which(rownames(df) ==rownames(subject_data)[x])] <- subject_data$Visit.Num[x]-1
        }
      }
    }

  }

  return(df)
}

df <- check_and_fix_visit_num_order(df)
