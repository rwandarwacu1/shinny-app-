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
dig_data <- read.csv("DIG.csv")

##data processing#####################################################################################################

#conversion of numerical columns 
numerical_columns <- c("AGE", "BMI", "KLEVEL", "CREAT", "DIABP", "SYSBP", "HOSPDAYS", "DEATHDAY", 
                       "EJF_PER", "CHESTX", "DIGDOSER", "CHFDUR", "HEARTRTE")
dig_data[, numerical_columns] <- lapply(dig_data[, numerical_columns], as.numeric)

# Conversion of categorical columns to factors
dig_data$TRTMT <- factor(dig_data$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
dig_data$SEX <- factor(dig_data$SEX, levels = c(1, 2), labels = c("Male", "Female"))
dig_data$DEATH <- factor(dig_data$DEATH, levels = c(0, 1), labels = c("Alive", "Death"))
dig_data$HYPERTEN <- factor(dig_data$HYPERTEN, levels = c(0, 1), labels = c("NO HYPERTEN", "HYPERTEN"))
dig_data$CVD <- factor(dig_data$CVD, levels = c(0, 1), labels = c("NO CVD", "CVD"))
dig_data$WHF <- factor(dig_data$WHF, levels = c(0, 1), labels = c("NO WHF", "WHF"))
dig_data$HOSP <- factor(dig_data$HOSP, levels = c(0, 1), labels = c("NO HOSP", "HOSP"))
dig_data$DIG <- factor(dig_data$DIG, levels = c(0, 1), labels = c("NO DIG TOX", "DIG TOX"))
dig_data$RACE <- factor(dig_data$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))

dig_data$FUNCTCLS <- factor(dig_data$FUNCTCLS, levels = c(1, 2, 3, 4), labels = c("Class I", "Class II", "Class III", "Class IV"))
dig_data$CHFETIOL <- factor(dig_data$CHFETIOL, levels = c(1, 2, 3), labels = c("Ischemic", "Non-ischemic", "Unknown"))
dig_data$PREVMI <- factor(dig_data$PREVMI, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$ANGINA <- factor(dig_data$ANGINA, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$DIABETES <- factor(dig_data$DIABETES, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$DIGUSE <- factor(dig_data$DIGUSE, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$DIURETK <- factor(dig_data$DIURETK, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$DIURET <- factor(dig_data$DIURET, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$ACEINHIB <- factor(dig_data$ACEINHIB, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$NITRATES <- factor(dig_data$NITRATES, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$HYDRAL <- factor(dig_data$HYDRAL, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$VASOD <- factor(dig_data$VASOD, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$MI <- factor(dig_data$MI, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$UANG <- factor(dig_data$UANG, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$STRK <- factor(dig_data$STRK, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$SVA <- factor(dig_data$SVA, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$VENA <- factor(dig_data$VENA, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$CREV <- factor(dig_data$CREV, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$OCVD <- factor(dig_data$OCVD, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$RINF <- factor(dig_data$RINF, levels = c(0, 1), labels = c("No", "Yes"))
dig_data$OTH <- factor(dig_data$OTH, levels = c(0, 1), labels = c("No", "Yes"))

dig_data$DWHF <- factor(dig_data$DWHF, levels = c(0, 1), labels = c("No", "Yes"))

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
                    selectInput("var_select", "Choose a variable:", choices = c("Select", sort(names(dig_data)))),
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
                    selectInput("x_var", "X-axis Variable:", choices = c("Select", sort(names(dig_data)))),
                    selectInput("y_var", "Y-axis Variable:", choices = c("Select", sort(names(dig_data)))),
                    selectInput("plot_type", "Select Plot Type:", choices = c("Scatter Plot", "Density Plot", "Boxplot", "Violin Plot", "Bar Chart", "Histogram"), selected = "Scatter Plot"),
                    checkboxGroupInput("group_vars", "Group by (optional):", choices = sort(names(dig_data)), inline = TRUE),
                    checkboxInput("facet_wrap", "Enable Facet Wrap", value = TRUE),
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
    req(input$var_select != "Select")
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
    req(input$var_select != "Select")
    data <- filtered_data()
    plot <- if (is.numeric(data[[input$var_select]])) {
      ggplot(data, aes_string(x = input$var_select)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Distribution of", input$var_select), x = input$var_select, y = "Count")
    } else {
      ggplot(data, aes_string(x = input$var_select, fill = input$var_select)) +
        geom_bar(color = "black") +
        theme_minimal() +
        labs(title = paste("Bar Plot of", input$var_select), x = input$var_select, y = "Count") +
        guides(fill = guide_legend(title = "Categories"))
    }
    if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
      plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
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
      req(input$x_var != "Select", input$y_var != "Select")
      data <- filtered_data()
      plot <- NULL
      if (input$plot_type == "Scatter Plot") {
        plot <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = "red", alpha = 0.6) +
          theme_minimal() +
          labs(title = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
               x = input$x_var, y = input$y_var) +
          theme(legend.position = "right")
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      } else if (input$plot_type == "Density Plot") {
        plot <- ggplot(data, aes_string(x = input$x_var, fill = input$y_var)) +
          geom_density(alpha = 0.5) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$x_var, "by", input$y_var),
               x = input$x_var, y = "Density")
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      } else if (input$plot_type == "Boxplot") {
        plot <- ggplot(data, aes_string(x = input$y_var, y = input$x_var)) +
          geom_boxplot(fill = "orange", color = "black") +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$x_var, "by", input$y_var),
               x = input$y_var, y = input$x_var)
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      } else if (input$plot_type == "Violin Plot") {
        plot <- ggplot(data, aes_string(x = input$y_var, y = input$x_var)) +
          geom_violin(fill = "purple", color = "black") +
          theme_minimal() +
          labs(title = paste("Violin Plot of", input$x_var, "by", input$y_var),
               x = input$y_var, y = input$x_var)
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      } else if (input$plot_type == "Bar Chart") {
        plot <- ggplot(data, aes_string(x = input$x_var, fill = input$y_var)) +
          geom_bar(position = "dodge") +
          theme_minimal() +
          labs(title = paste("Bar Chart of", input$x_var, "by", input$y_var),
               x = input$x_var, y = "Count")+
          guides(fill = guide_legend(title = "Categories"))
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
        
      } else if (input$plot_type == "Histogram") {
        plot <- ggplot(data, aes_string(x = input$x_var)) +
          geom_histogram(bins = 30, fill = "green", color = "black") +
          theme_minimal() +
          labs(title = paste("Histogram of", input$x_var), x = input$x_var, y = "Frequency")
        if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
          plot <- plot + facet_wrap(as.formula(paste("~", paste(input$group_vars, collapse = "+"))))
        }
      }
      ggplotly(plot)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


