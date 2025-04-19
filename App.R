library(shiny)
library(ggplot2)
library(DT)
library(colourpicker)
library(reshape2)
library(ggcorrplot)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .padded-layout {
        padding-top: 20px;
      }
    "))),
  titlePanel("Plots & Summary Statistics Generator "),
  
  tabsetPanel(
      tabPanel("Step 1: Upload Data",
               sidebarLayout(
                 div(class = "padded-layout",
                 sidebarPanel(
                   fileInput("file", "Upload CSV File", accept = ".csv"),
                   checkboxInput("header", "Header", TRUE),
                   h4("📊 Basic Info"),
                   verbatimTextOutput("basic_info")
                 )),
                 mainPanel(
                   h4("🔍 Data Preview"),
                   DTOutput("data_preview"),
                   hr(),
                   h4("📈 Summary Statistics (Numerical Columns)"),
                   verbatimTextOutput("summary_stats")
                 )
               )
      ),
    
    tabPanel("Step 2: Plot Generator",
             sidebarLayout(
               div(class = "padded-layout",
               sidebarPanel(
                 selectInput("plot_type", "Select Plot Type",
                             choices = c("Scatter" = "scatter", 
                                         "Box Plot" = "box", 
                                         "Violin Plot" = "violin",
                                         "Histogram" = "hist")),
                 
                 uiOutput("var_select"),
                 
                 textInput("xlab", "X-axis Label", ""),
                 textInput("ylab", "Y-axis Label", ""),
                 colourInput("color", "Plot Color", "steelblue"),
                 
                 selectInput("theme", "Plot Theme", choices = c("Minimal" = "theme_minimal",
                                                                "Classic" = "theme_classic",
                                                                "Light" = "theme_light",
                                                                "Dark" = "theme_dark")),
                 
                 sliderInput("plotWidth", "Plot Width (px)", min = 300, max = 1600, value = 800),
                 sliderInput("plotHeight", "Plot Height (px)", min = 300, max = 1200, value = 500),
                 
                 downloadButton("downloadPlot", "📥 Download Plot")
               )),
               mainPanel(
                 plotOutput("custom_plot")
               )
             )
    ),
    
    # tabPanel("Step 3: Summary Statistics",
    #          h4("Table: Numeric Summary"),
    #          verbatimTextOutput("summary_stats")
    # ),
    
    tabPanel("Step 3: Correlation Matrix",
             h4("Correlation Heatmap (numeric variables only)"),
             downloadButton("download_corr_plot", "Download Correlation Plot"),
             plotOutput("cor_plot", height = "600px")
    ),
    tabPanel("Step 4: Univariate & Bivariate Analysis",
             div(class = "padded-layout",
             sidebarLayout(
               sidebarPanel(
                 h4("🎯 Select Target Variable"),
                 uiOutput("target_selector"),
                 actionButton("analyze_btn", "Run Analysis", class = "btn-primary"),
                 numericInput("round_digits", "Decimal Precision(Univariate Summary):", value = 2, min = 0, max = 6)
               ),
               mainPanel(
                 h4("📌 Univariate Summary"),
                 fluidRow(
                   column(
                     width = 7,
                     h4("🔢 Numeric Summary"),
                     downloadButton("download_numeric", "Download Numeric Summary"),
                     DTOutput("numeric_summary")
                   ),
                   column(
                     width = 5,
                     h4("🔠 Categorical Summary"),
                     downloadButton("download_categorical", "Download Categorical Summary"),
                     DTOutput("categorical_summary")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(6,
                          h4("📊 Bivariate Summary (Target Relationship)"),
                          DTOutput("bivar_table")
                          ),
                   column(6)
                 )
                 
               )
             ))
    )
  )
)

server <- function(input, output, session) {
  # Read uploaded data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = input$header)
  })
  
  # Preview
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 10), options = list(scrollX = TRUE),rownames = FALSE)
  })
  
  # Basic info
  output$basic_info <- renderPrint({
    req(data())
    df <- data()
    cat("Number of Rows:", nrow(df), "\n")
    cat("Number of Columns:", ncol(df), "\n")
    cat("\nColumn Names:\n")
    print(names(df))
    cat("\nData Types:\n")
    print(sapply(df, class))
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(data())
    df <- data()
    summary(df[sapply(df, is.numeric)])
  })
  
  # Dynamic variable selectors
  output$var_select <- renderUI({
    req(data())
    cols <- names(data())
    tagList(
      selectInput("xvar", "X Variable", choices = cols),
      conditionalPanel(
        condition = "input.plot_type != 'hist'",
        selectInput("yvar", "Y Variable", choices = cols)
      )
    )
  })
  
  # Theme helper
  get_theme <- reactive({
    switch(input$theme,
           "theme_minimal" = theme_minimal(),
           "theme_classic" = theme_classic(),
           "theme_light" = theme_light(),
           "theme_dark" = theme_dark())
  })
  
  # Plot generator
  output$custom_plot <- renderPlot({
    req(input$xvar)
    df <- data()
    p <- ggplot(df, aes_string(x = input$xvar))
    
    if (input$plot_type == "scatter") {
      req(input$yvar)
      p <- p + aes_string(y = input$yvar) +
        geom_point(color = input$color)
    } else if (input$plot_type == "box") {
      req(input$yvar)
      p <- p + aes_string(y = input$yvar) +
        geom_boxplot(fill = input$color)
    } else if (input$plot_type == "violin") {
      req(input$yvar)
      p <- p + aes_string(y = input$yvar) +
        geom_violin(fill = input$color)
    } else if (input$plot_type == "hist") {
      p <- p + geom_histogram(fill = input$color, bins = 30)
    }
    
    p + labs(x = input$xlab, y = input$ylab) + get_theme()
  }, width = function() input$plotWidth, height = function() input$plotHeight)
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("custom_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$plotWidth, height = input$plotHeight)
      print(output$custom_plot())
      dev.off()
    }
  )
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(data())
    df <- data()
    summary(df[sapply(df, is.numeric)])
  })
  
  # Correlation matrix plot
  output$cor_plot <- renderPlot({
    req(data())
    df <- data()
    # num_cols <- df[sapply(df, is.numeric)]
    # corr_matrix <- cor(num_cols, use = "complete.obs")
    # corrplot::corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)
    num_df <- df[sapply(df, is.numeric)]
    cor_matrix <- cor(num_df, use = "pairwise.complete.obs")
    ggcorrplot(cor_matrix, lab = TRUE, type = "lower", lab_size = 3,
               colors = c("#6D9EC1", "white", "#E46726"))
  })
  output$download_corr_plot <- downloadHandler(
    filename = function() {
      paste0("correlation_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- data()
      # num_cols <- df[sapply(df, is.numeric)]
      # corr_matrix <- cor(num_cols, use = "complete.obs")
      # corrplot::corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)
      num_df <- df[sapply(df, is.numeric)]
      cor_matrix <- cor(num_df, use = "pairwise.complete.obs")
      p<- ggcorrplot(cor_matrix, lab = TRUE, type = "lower", lab_size = 3,
                 colors = c("#6D9EC1", "white", "#E46726"))
      print(p)
      dev.off()
    }
  )
  
  ######### TAB 4 ##############
  # Target variable selector
  output$target_selector <- renderUI({
    req(data())
    selectInput("target", "Target Variable:", choices = names(data()))
  })
  summary_data <- reactiveValues(numeric = NULL, categorical = NULL)
  # Run on button click
  observeEvent(input$analyze_btn, {
    req(data(), input$target)
    df <- data()
    target <- input$target
    
    # Univariate Table: Summary stats grouped by target
    output$numeric_summary <- renderDT({
      req(data(), input$round_digits)
      df <- data()
      digits <- input$round_digits
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) == 0) return(NULL)
      
      numeric_summary <- data.frame(
        Variable = num_cols,
        Mean = sapply(df[num_cols], function(x) round(mean(x, na.rm = TRUE), digits)),
        Median = sapply(df[num_cols], function(x) round(median(x, na.rm = TRUE), digits)),
        SD = sapply(df[num_cols], function(x) round(sd(x, na.rm = TRUE), digits)),
        Min = sapply(df[num_cols], function(x) round(min(x, na.rm = TRUE), digits)),
        Max = sapply(df[num_cols], function(x) round(max(x, na.rm = TRUE), digits)),
        NA_Count = sapply(df[num_cols], function(x) sum(is.na(x)))
      )
      
      summary_data$numeric <- numeric_summary
      datatable(numeric_summary, options = list(dom = 't', scrollX = TRUE),rownames = FALSE)
    })
    
    output$categorical_summary <- renderDT({
      req(data())
      df <- data()
      cat_cols <- names(df)[sapply(df, function(col) is.character(col) || is.factor(col))]
      if (length(cat_cols) == 0) return(NULL)
      
      mode_fn <- function(x) {
        ux <- na.omit(unique(x))
        if (length(ux) == 0) return(NA)
        ux[which.max(tabulate(match(x, ux)))]
      }
      
      cat_summary <- data.frame(
        Variable = cat_cols,
        Mode = sapply(df[cat_cols], mode_fn),
        NA_Count = sapply(df[cat_cols], function(x) sum(is.na(x)))
      )
      
      summary_data$categorical <- cat_summary
      datatable(cat_summary, options = list(dom = 't', scrollX = TRUE),rownames = FALSE)
    })
    
    # Bivariate Table: Simple statistical test (numeric vs target)
    output$bivar_table <- renderDT({
      df <- data()
      numeric_vars <- names(df)[sapply(df, is.numeric) & names(df) != target]
      is_class <- is.factor(df[[target]]) || is.character(df[[target]]) || is.logical(df[[target]])
      
      bivar_results <- lapply(numeric_vars, function(var) {
        tryCatch({
          if (is_class) {
            pval <- summary(aov(df[[var]] ~ df[[target]]))[[1]][["Pr(>F)"]][1]
          } else {
            pval <- cor.test(df[[var]], df[[target]], method = "pearson")$p.value
          }
          data.frame(Variable = var, P_Value = round(pval, 4))
        }, error = function(e) {
          data.frame(Variable = var, P_Value = NA)
        })
      })
      
      bivar_df <- do.call(rbind, bivar_results)
      datatable(bivar_df, options = list(scrollX = TRUE),rownames = FALSE)
    })
    output$download_numeric <- downloadHandler(
      filename = function() {
        paste0("numeric_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(summary_data$numeric, file, row.names = FALSE)
      }
    )
    
    output$download_categorical <- downloadHandler(
      filename = function() {
        paste0("categorical_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(summary_data$categorical, file, row.names = FALSE)
      }
    )
  })
  
}

shinyApp(ui = ui, server = server)
