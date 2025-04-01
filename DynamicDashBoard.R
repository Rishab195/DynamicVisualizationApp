library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(GGally)
library(reshape2)
library(colourpicker)  # Ensure this is loaded

ui <- dashboardPage(
  dashboardHeader(title = "Advanced Data Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("binoculars")),
      menuItem("Visualization", tabName = "data_viz", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Exploration Tab
      tabItem(tabName = "data_explore",
              fluidRow(
                box(
                  title = "Upload Dataset", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("uploadData", "Choose CSV/Excel File", accept = c(".csv", ".xlsx", ".xls"))
                )
              ),
              fluidRow(
                tabBox(
                  title = "Data Analysis", width = 12,
                  tabPanel("Data View", withSpinner(dataTableOutput("data_table"))),
                  tabPanel("Structure", withSpinner(verbatimTextOutput("data_structure"))),
                  tabPanel("Basic Summary", withSpinner(verbatimTextOutput("basic_summary"))),
                  tabPanel("Advanced Summary", withSpinner(verbatimTextOutput("advanced_summary"))),
                  tabPanel("Column Analysis",
                           fluidRow(
                             column(6, h4("Numeric Columns"), withSpinner(verbatimTextOutput("numeric_cols"))),
                             column(6, h4("Categorical Columns"), withSpinner(verbatimTextOutput("categorical_cols")))
                           ),
                           fluidRow(
                             column(12, h4("Missing Values"), withSpinner(dataTableOutput("missing_value_table")))
                           )
                  )
                )
              )
      ),
      
      # Visualization Tab
      tabItem(tabName = "data_viz",
              fluidRow(
                box(
                  title = "Visualization Settings", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("x_var", "X-Axis Variable", choices = NULL),
                           selectInput("agg_x", "X-Axis Aggregation", 
                                       choices = c("None", "Sum", "Mean", "Count", "Median"))
                    ),
                    column(4,
                           selectInput("y_var", "Y-Axis Variable", choices = NULL),
                           selectInput("agg_y", "Y-Axis Aggregation", 
                                       choices = c("None", "Sum", "Mean", "Count", "Median"))
                    ),
                    column(4,
                           selectInput("chart_type", "Chart Type",
                                       choices = c("Scatter Plot", "Bar Chart", "Line Chart", 
                                                   "Histogram", "Boxplot", "Pie Chart")),
                           colourpicker::colourInput("plot_color", "Plot Color", value = "#1f77b4")  # Explicit namespace
                    )
                  ),
                  actionButton("plotDataBtn", "Generate Visualization", class = "btn-success")
                )
              ),
              fluidRow(
                box(
                  title = "Interactive Plot", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(plotlyOutput("data_plot", height = "500px"))
                )
              ),
              fluidRow(
                box(
                  title = "Correlation Analysis", status = "warning", solidHeader = TRUE, width = 6,
                  selectInput("correlation_vars", "Variables for Correlation", 
                              choices = NULL, multiple = TRUE),
                  withSpinner(plotlyOutput("correlation_matrix", height = "400px"))
                ),
                box(
                  title = "Pair Plot Analysis", status = "warning", solidHeader = TRUE, width = 6,
                  selectInput("pair_plot_vars", "Variables for Pair Plot", 
                              choices = NULL, multiple = TRUE),
                  withSpinner(plotlyOutput("pair_plot", height = "400px"))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Data Loading
  uploaded_data <- reactive({
    req(input$uploadData)
    ext <- tools::file_ext(input$uploadData$name)
    tryCatch({
      if (ext == "csv") {
        read_csv(input$uploadData$datapath)
      } else if (ext %in% c("xls", "xlsx")) {
        read_excel(input$uploadData$datapath)
      } else {
        stop("Invalid file format. Please upload a CSV or Excel file.")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(data.frame())
    })
  })
  
  # Update UI choices when data is uploaded
  observeEvent(uploaded_data(), {
    df <- uploaded_data()
    if (nrow(df) > 0) {
      updateSelectInput(session, "x_var", choices = names(df))
      updateSelectInput(session, "y_var", choices = names(df))
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "correlation_vars", choices = numeric_cols)
      updateSelectInput(session, "pair_plot_vars", choices = numeric_cols)
    }
  })
  
  # Data Exploration Outputs
  output$data_table <- renderDataTable({ 
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 10))
  })
  
  output$data_structure <- renderPrint({ req(uploaded_data()); str(uploaded_data()) })
  output$basic_summary <- renderPrint({ req(uploaded_data()); summary(uploaded_data()) })
  
  output$advanced_summary <- renderPrint({
    req(uploaded_data())
    df <- uploaded_data()
    numeric_cols <- df %>% select_if(is.numeric)
    list(
      Mean = colMeans(numeric_cols, na.rm = TRUE),
      Median = apply(numeric_cols, 2, median, na.rm = TRUE),
      SD = apply(numeric_cols, 2, sd, na.rm = TRUE),
      Variance = apply(numeric_cols, 2, var, na.rm = TRUE),
      Min = apply(numeric_cols, 2, min, na.rm = TRUE),
      Max = apply(numeric_cols, 2, max, na.rm = TRUE)
    )
  })
  
  output$numeric_cols <- renderPrint({ 
    req(uploaded_data())
    paste(names(uploaded_data())[sapply(uploaded_data(), is.numeric)], collapse = ", ") 
  })
  
  output$categorical_cols <- renderPrint({ 
    req(uploaded_data())
    paste(names(uploaded_data())[!sapply(uploaded_data(), is.numeric)], collapse = ", ") 
  })
  
  output$missing_value_table <- renderDataTable({ 
    req(uploaded_data())
    datatable(
      data.frame(
        Column = names(uploaded_data()),
        Missing_Count = colSums(is.na(uploaded_data())),
        Missing_Percent = round(colSums(is.na(uploaded_data())) / nrow(uploaded_data()) * 100, 2)
      ),
      options = list(pageLength = 10)
    )
  })
  
  # Data Processing with Aggregation
  processed_data <- reactive({
    req(uploaded_data(), input$x_var, input$y_var, input$plotDataBtn)
    df <- uploaded_data()
    
    # Apply aggregation
    agg_func <- function(x, agg_type) {
      switch(agg_type,
             "Sum" = sum(x, na.rm = TRUE),
             "Mean" = mean(x, na.rm = TRUE),
             "Count" = length(x[!is.na(x)]),
             "Median" = median(x, na.rm = TRUE),
             x)
    }
    
    if (input$agg_x != "None" || input$agg_y != "None") {
      df_group <- df %>% 
        group_by(across(all_of(input$x_var))) %>%
        summarise(
          x_val = agg_func(get(input$x_var), input$agg_x),
          y_val = agg_func(get(input$y_var), input$agg_y),
          .groups = "drop"
        )
      return(df_group)
    }
    df %>% select(x_val = !!input$x_var, y_val = !!input$y_var)
  })
  
  # Visualization
  output$data_plot <- renderPlotly({
    req(processed_data(), input$chart_type)
    df <- processed_data()
    
    p <- switch(input$chart_type,
                "Scatter Plot" = ggplot(df, aes(x = x_val, y = y_val)) + 
                  geom_point(color = input$plot_color),
                "Bar Chart" = ggplot(df, aes(x = x_val, y = y_val)) + 
                  geom_bar(stat = "identity", fill = input$plot_color),
                "Line Chart" = ggplot(df, aes(x = x_val, y = y_val)) + 
                  geom_line(color = input$plot_color),
                "Histogram" = ggplot(df, aes(x = x_val)) + 
                  geom_histogram(fill = input$plot_color, bins = 30),
                "Boxplot" = ggplot(df, aes(x = x_val, y = y_val)) + 
                  geom_boxplot(fill = input$plot_color),
                "Pie Chart" = ggplot(df, aes(x = "", y = y_val, fill = x_val)) + 
                  geom_bar(stat = "identity") + coord_polar("y")
    ) + theme_minimal() +
      labs(title = paste(input$chart_type, "of", input$x_var, "vs", input$y_var))
    
    ggplotly(p)
  })
  
  # Correlation and Pair Plots
  output$correlation_matrix <- renderPlotly({
    req(uploaded_data(), input$correlation_vars)
    df <- uploaded_data() %>% select(all_of(input$correlation_vars))
    corr_matrix <- cor(df, use = "complete.obs")
    melted_corr <- melt(corr_matrix)
    ggplotly(
      ggplot(melted_corr, aes(Var1, Var2, fill = value)) + 
        geom_tile() + 
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation Matrix")
    )
  })
  
  output$pair_plot <- renderPlotly({
    req(uploaded_data(), input$pair_plot_vars)
    df <- uploaded_data() %>% select(all_of(input$pair_plot_vars))
    ggplotly(ggpairs(df, title = "Pair Plot Analysis"))
  })
}

# Save and run the app
# writeLines(text = deparse(substitute(ui)), "C:/Users/HP/Desktop/Shiny/Project/temp.R")
shinyApp(ui,server)
