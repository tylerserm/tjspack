#' @title Update Venous Confirmatory Sample Stage
#'
#' @description This function is specific to case management of elevated blood lead patients and how samples are automatically coded as confirmatory if they are venous blood draws even if they are an initial test result.
#'
#' @param df the dataframe you would like applied to this function
#' @param Subj_ID the patients identifying number
#' @param visit_num the visit number, the record of which instance the patient was seen
#' @param Case_Status the category of your case investigation, typically inital or confirmatory
#' @param Test_Type the source of your sample, for example the collection method used
#'
#' @returns The entered dataframe now with any incorrectly entered case statuses information fixed based on testing results
#' @export
#'
#' @examples
#' This example dataset has an incorrectly entered case status of initial despite a venous blood draw, which automatically confirms the case.
#' example <- data.frame(
#' ID = c(1, 1, 1, 2, 2),
#' visit_num = c(1, 3, 2, 1, 2)
#' Case_Status = c( "Initial", "Confirmatory", "Most_Recent", "Initial", "Most_Recent")
#' Test_Type = c( "Capillary", "Capillary", "Venous" , "Venous", "Capillary")
#' check_Age(example, "ID",)
#' #Notice now, the incorrect case status has been corrected
#' example
update_stage <- function(df, Subj_ID, visit_num, Case_Status, Test_Type) {
  df[[Case_Status]] <- ifelse(df[[Test_Type]] == "Venous" &
                                (df[[visit_num]] == 1 | df[[Case_Status]] == "Initial"),
                              "Confirmatory", df[[Case_Status]]
  )

  return(df)
}

df <- update_stage(df)

