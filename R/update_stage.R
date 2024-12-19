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
update_venous_specimen_stage <- function(df) {
  # Check the conditions and update Specimen.Stage
  df$Specimen.Stage <- ifelse(df$Blood.Draw.Type == "Venous" &
                                (df$Visit.Num == 1 | df$Specimen.Stage == "Initial"),
                              "Confirmatory", df$Specimen.Stage  # Leave as is if conditions are not met
  )

  return(df)  # Return the modified dataframe
}

df <- update_venous_specimen_stage(df)

