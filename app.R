# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(survival)
library(survminer)
library(plotly)

# Load the DIG 
dig_data <- read.csv("shinny-app-Athi-victor-/DIG.csv")

##data cleaning#####################################################################################################
#dig_data<- dig_data|>select(ID,TRTMT,AGE,SEX,BMI,KLEVEL,CREAT,DIABP,SYSBP,HYPERTEN,CVD,WHF,DIG,HOSP,HOSPDAYS, DEATH,DEATHDAY)

#conversion of numerical colums , factor variables are converted in the Task 2 
dig_data[,c("AGE","BMI","KLEVEL","CREAT","DIABP","SYSBP","HOSPDAYS","DEATHDAY")]<- lapply(dig_data[,c("AGE","BMI","KLEVEL","CREAT","DIABP","SYSBP","HOSPDAYS","DEATHDAY")],function(column){as.numeric(column)}) 

dig_data$TRTMT<- factor(dig_data$TRTMT,levels = c(0,1), labels = c("Placebo","Treatment") )
dig_data$SEX<- factor(dig_data$SEX,levels = c(1,2), labels = c("Male","Female") )
dig_data$DEATH<- factor(dig_data$DEATH,levels = c(0,1), labels = c("Alive","Death") )
dig_data$HYPERTEN<- factor(dig_data$HYPERTEN,levels = c(0,1), labels = c("NO HYPERTEN","HYPERTEN") )
dig_data$CVD<- factor(dig_data$CVD,levels = c(0,1), labels = c("NO CVD","CVD") )
dig_data$WHF<- factor(dig_data$WHF,levels = c(0,1), labels = c("NO WHF","WHF") )
dig_data$HOSP<- factor(dig_data$HOSP,levels = c(0,1), labels = c("NO HOSP","HOSP") )
dig_data$DIG<- factor(dig_data$DIG,levels = c(0,1), labels = c("NO DIG TOX","DIG TOX") )

##########################################################################################################################
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
                    sliderInput("rows_display", "Number of rows to display:", min = 5, max = 100, value = 10),
                    DTOutput("data_table"))
              )
      ),
      
      # Tab 2: Key Variables
      tabItem(tabName = "variables",
              fluidRow(
                box(title = "Variable Selection", width = 4, status = "info",
                    selectInput("var_select", "Choose a variable:", choices = sort(names(dig_data))),
                    checkboxInput("show_summary", "Show Summary Statistics", value = TRUE),
                    uiOutput("filter_ui")),
                box(title = "Visualization", width = 8, status = "primary",
                    plotlyOutput("var_plot"),
                    verbatimTextOutput("summary_stats"))
              )
      ),
      
      # Tab 3: Outcomes & Relationships
      tabItem(tabName = "outcomes",
              fluidRow(
                box(title = "Relationship Analysis", width = 4, status = "info",
                    selectInput("x_var", "X-axis Variable:", choices = sort(names(dig_data))),
                    selectInput("y_var", "Y-axis Variable:", choices = sort(names(dig_data))),
                    selectInput("plot_type", "Select Plot Type:", choices = c("Scatter Plot", "Density Plot", "Boxplot", "Violin Plot", "Bar Chart", "Histogram"), selected = "Scatter Plot"),
                    checkboxGroupInput("group_vars", "Group by (optional):", choices = sort(names(dig_data)), inline = TRUE),
                    checkboxInput("facet_wrap", "Enable Facet Wrap", value = FALSE),
                    actionButton("plot_button", "Generate Plot")),
                box(title = "Interactive Plot", width = 8, status = "primary",
                    plotlyOutput("dynamic_plot"))
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive dataset based on filters
  filtered_data <- reactive({
    data <- dig_data
    if (!is.null(input$filter_var) && !is.null(input$filter_value)) {
      data <- data %>% filter(.data[[input$filter_var]] %in% input$filter_value)
    }
    data
  })
  
  # Dynamic filter UI
  output$filter_ui <- renderUI({
    req(input$var_select)
    if (is.factor(dig_data[[input$var_select]]) || is.character(dig_data[[input$var_select]])) {
      selectInput("filter_value", paste("Filter", input$var_select, ":"),
                  choices = unique(dig_data[[input$var_select]]), multiple = TRUE)
    } else {
      sliderInput("filter_value", paste("Filter", input$var_select, ":"),
                  min = min(dig_data[[input$var_select]], na.rm = TRUE),
                  max = max(dig_data[[input$var_select]], na.rm = TRUE),
                  value = range(dig_data[[input$var_select]], na.rm = TRUE))
    }
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(filtered_data()[1:input$rows_display, ], options = list(scrollX = TRUE))
  })
  
  # Variable Visualization
  output$var_plot <- renderPlotly({
    req(input$var_select)
    data <- filtered_data()
    plot <- if (is.numeric(data[[input$var_select]])) {
      ggplot(data, aes_string(x = input$var_select)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Distribution of", input$var_select), x = input$var_select, y = "Count")
    } else {
      ggplot(data, aes_string(x = input$var_select)) +
        geom_bar(fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Distribution of", input$var_select), x = input$var_select, y = "Count")
    }
    ggplotly(plot)
  })
  
  output$summary_stats <- renderPrint({
    if (input$show_summary) {
      summary(filtered_data()[[input$var_select]])
    }
  })
  
  # Dynamic Plot Selection
  observeEvent(input$plot_button, {
    output$dynamic_plot <- renderPlotly({
      req(input$x_var)
      data <- filtered_data()
      plot <- NULL
      if (input$plot_type == "Scatter Plot") {
        plot <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = "blue", alpha = 0.6) +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
               x = input$x_var, y = input$y_var)
        if (input$facet_wrap && !is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      } else if (input$plot_type == "Density Plot") {
        plot <- ggplot(data, aes_string(x = input$x_var, fill = input$y_var)) +
          geom_density(alpha = 0.5) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$x_var, "by", input$y_var),
               x = input$x_var, y = "Density")
      } else if (input$plot_type == "Boxplot") {
        plot <- ggplot(data, aes_string(x = input$y_var, y = input$x_var)) +
          geom_boxplot(fill = "orange", color = "black") +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$x_var, "by", input$y_var),
               x = input$y_var, y = input$x_var)
      } else if (input$plot_type == "Violin Plot") {
        plot <- ggplot(data, aes_string(x = input$y_var, y = input$x_var)) +
          geom_violin(fill = "purple", color = "black") +
          theme_minimal() +
          labs(title = paste("Violin Plot of", input$x_var, "by", input$y_var),
               x = input$y_var, y = input$x_var)
      } else if (input$plot_type == "Bar Chart") {
        plot <- ggplot(data, aes_string(x = input$x_var, fill = input$y_var)) +
          geom_bar(position = "dodge") +
          theme_minimal() +
          labs(title = paste("Bar Chart of", input$x_var, "by", input$y_var),
               x = input$x_var, y = "Count")
      } else if (input$plot_type == "Histogram") {
        plot <- ggplot(data, aes_string(x = input$x_var)) +
          geom_histogram(bins = 30, fill = "blue", color = "white") +
          theme_minimal() +
          labs(title = paste("Histogram of", input$x_var), x = input$x_var, y = "Frequency")
      }
      ggplotly(plot)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)