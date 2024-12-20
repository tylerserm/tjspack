library(shiny)
library(readr)
library(dplyr)
library(tools)

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
  df <- df[, !names(df) %in% c("age_check", "calculated_age")]
  return(df)
}

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

update_stage <- function(df, Subj_ID, visit_num, Case_Status, Test_Type) {
  df[[Case_Status]] <- ifelse(df[[Test_Type]] == "Venous" &
                                (df[[visit_num]] == 1 | df[[Case_Status]] == "Initial"),
                              "Confirmatory", df[[Case_Status]]
  )

  return(df)
}

ui <- fluidPage(
  titlePanel("Update Data Functions"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),

      checkboxGroupInput("functions", "Choose Functions to Apply (if not all, must select Custom in order below)",
                         choices = list(
                           "Check Age" = "check_age",
                           "Check Visit Order" = "check_visit_order",
                           "Update Stage" = "update_stage"
                         ),
                         selected = c("check_age", "check_visit_order", "update_stage")),  # Default selected functions

      textInput("Subj_ID", "Subject ID Column", value = "Subj_ID"),
      textInput("testdate", "Test Date Column", value = "testdate"),
      textInput("DOB", "Date of Birth Column", value = "DOB"),
      textInput("Age", "Age Column", value = "Age"),
      textInput("visit_num", "Visit Number Column", value = "visit_num"),

      conditionalPanel(
        condition = "input.functions.indexOf('update_stage') > -1",
        textInput("Case_Status", "Case Status Column", value = "Case_Status"),
        textInput("Test_Type", "Test Type Column", value = "Test_Type")
      ),

      selectInput("order", "Choose the order to apply functions:",
                  choices = c("Default", "Custom (run in order of selections)"),
                  selected = "Default"),

      actionButton("update", "Apply Functions to Data"),

      downloadButton("download", "Download Cleaned Data")
    ),

    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  updated_df <- reactiveVal(NULL)

  output$table <- renderTable({
    req(data())
    head(data())
  })

  observeEvent(input$update, {
    df <- data()

    selected_functions <- input$functions

    if (length(selected_functions) == 0) {
      return(NULL)
    }

    Subj_ID <- input$Subj_ID
    testdate <- input$testdate
    DOB <- input$DOB
    Age <- input$Age
    visit_num <- input$visit_num
    Case_Status <- input$Case_Status
    Test_Type <- input$Test_Type

    if (input$order == "Custom (run in order of selections)") {
      for (func in selected_functions) {
        if (func == "check_age") {
          df <- check_age(df, Subj_ID, testdate, DOB, Age)
        }
        if (func == "check_visit_order") {
          df <- check_visit_order(df, Subj_ID, testdate, visit_num)
        }
        if (func == "update_stage") {
          df <- update_stage(df, Subj_ID, visit_num, Case_Status, Test_Type)
        }
      }
    } else {
      df <- check_age(df, Subj_ID, testdate, DOB, Age)
      df <- check_visit_order(df, Subj_ID, testdate, visit_num)
      df <- update_stage(df, Subj_ID, visit_num, Case_Status, Test_Type)
    }

    updated_df(df)

    output$table <- renderTable({
      df
    })
  })

  output$download <- downloadHandler(
    filename = function() {
      paste(file_path_sans_ext(input$file1$name), "_cleaned.csv", sep = "")
    },
    content = function(file) {
      req(updated_df())  # Ensure the updated dataframe is available
      write.csv(updated_df(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
