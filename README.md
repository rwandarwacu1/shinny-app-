# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Load the DIG dataset
dig_data <- read.csv("DIG.csv")
##data cleaning#####################################################################################################
dig_data<- dig_data|>select(ID,TRTMT,AGE,SEX,BMI,KLEVEL,CREAT,DIABP,SYSBP,HYPERTEN,CVD,WHF,DIG,HOSP,HOSPDAYS, DEATH,DEATHDAY)

#conversion of numerical colums , factor variables are converted in the Task 2 
dig_data[,c("AGE","BMI","KLEVEL","CREAT","DIABP","SYSBP","HOSPDAYS","DEATHDAY")]<- lapply(dig_data[,c("AGE","BMI","KLEVEL","CREAT","DIABP","SYSBP","HOSPDAYS","DEATHDAY")],function(column){as.numeric(column)}) 



# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Key Variables", tabName = "variables", icon = icon("chart-bar")),
      menuItem("Outcomes & Relationships", tabName = "outcomes", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Data Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Data Summary", width = 12, status = "primary",
                    DTOutput("data_table"))
              )
      ),
      
      # Tab 2: Key Variables
      tabItem(tabName = "variables",
              fluidRow(
                box(title = "Variable Selection", width = 4, status = "info",
                    selectInput("var_select", "Choose a variable:", choices = names(dig_data)),
                    checkboxInput("show_summary", "Show Summary Statistics", value = TRUE)),
                box(title = "Visualization", width = 8, status = "primary",
                    plotOutput("var_plot"),
                    verbatimTextOutput("summary_stats"))
              )
      ),
      
      # Tab 3: Outcomes & Relationships
      tabItem(tabName = "outcomes",
              fluidRow(
                box(title = "Relationship Analysis", width = 4, status = "info",
                    selectInput("x_var", "X-axis Variable:", choices = names(dig_data)),
                    selectInput("y_var", "Y-axis Variable:", choices = names(dig_data)),
                    actionButton("plot_button", "Generate Plot")),
                box(title = "Scatter Plot", width = 8, status = "primary",
                    plotOutput("scatter_plot"))
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Data Table
  output$data_table <- renderDT({
    datatable(dig_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Variable Visualization
  output$var_plot <- renderPlot({
    req(input$var_select)
    ggplot(dig_data, aes_string(x = input$var_select)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Distribution of", input$var_select), x = input$var_select, y = "Count")
  })
  
  output$summary_stats <- renderPrint({
    if (input$show_summary) {
      summary(dig_data[[input$var_select]])
    }
  })
  
  # Scatter Plot
  observeEvent(input$plot_button, {
    output$scatter_plot <- renderPlot({
      req(input$x_var, input$y_var)
      ggplot(dig_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "blue", alpha = 0.6) +
        theme_minimal() +
        labs(title = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
             x = input$x_var, y = input$y_var)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
