# Case Study 3
# Author - Jack Schnair

# Load the required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)

lego_data <- read_csv("lego.csv")

ui <- fluidPage(
  titlePanel("Case Study 3 - Jack Schnair"),
  tabsetPanel(
    tabPanel("Home",
             dashboardPage(
               dashboardHeader(title = "Introduction"),
               dashboardSidebar(
                 p("")
               ),
               dashboardBody(
                 p("This shiny application was made to demonstrate how regression can be used to demonstrate
                   the relationship between a dependent variable and one or more independent variables.", style = "font-size:18px;"),
                 br(),
                 p("Click on the tabs above to use simple linear regression or multilinear regression.", style = "font-size:18px;"),
                 br(),
                 p("The dataset being used is the Brickset dataset. It is a record of most, if not all, lego sets ever made.
                    It is updated and maintained by a community of lego collectors and is up to date as of November 2023.", style = "font-size:18px;"),
                 br()
               )
    )),
    tabPanel("Simple Linear Regression",
             dashboardPage(
               dashboardHeader(title = "Linear Regression"),
               dashboardSidebar(
                 selectInput("dependent_var", "Select Dependent Variable", choices = c("USD_MSRP","Pieces", "Minifigures", "Owned", "Ratings","Current_Price"), selected = "USD_MSRP"),
                 selectInput("independent_var", "Select Independent Variable", choices = c("Pieces", "Minifigures", "USD_MSRP","Owned", "Ratings","Current_Price")),
                 actionButton("run_regression", "Run Simple Linear Regression"),
                 p("Select an indenpentent variable and a dependent variable from the Lego dataset, then press the run button.", 
                   style = "font-size:18px; padding-left: 15px; padding-right: 15px;"),
                 br(),
                 p("USD_MSRP and Pieces have the most correlation by far. Some independent variables make better models than others. 
                   Try them all to find out which work best.", style = "font-size:18px; padding-left: 15px; padding-right: 15px;"),
                 p("Take a look at the correlation coefficient to gauge how accurate each model is.", 
                   style = "font-size:18px; padding-left: 15px; padding-right: 15px;"),
                 br()
               ),
               dashboardBody(
                 box(
                   title = "Lego Data Linear Regression Plot",
                   plotOutput("regression_plot"),
                   verbatimTextOutput("correlation_output"),
                   verbatimTextOutput("summary")
                 )
               )
             )
    ),
    tabPanel("Multilinear Regression",
             dashboardPage(
               dashboardHeader(title = "Multilinear Regression"),
               dashboardSidebar(
                 selectInput("dependent", "Select Dependent Variable:", choices = c("Pieces", "Minifigures", "USD_MSRP","Owned", "Ratings","Current_Price"), selected = "USD_MSRP"),
                 checkboxGroupInput("independent", "Select Independent Variables:", choices = c("Pieces", "Minifigures", "USD_MSRP","Current_Price","Owned", "Rating")),
                 actionButton("run_multiregression", "Run Multilinear Regression"),
                 p("Select a dependent variable and check all the dependent variables you'd like, then press the run button.", 
                   style = "font-size:18px; padding-left: 15px; padding-right: 15px;"),
                 br(),
                 p("There are many different combinations of varaibles you can use. Experiment to see what works best.
                   Notice that when Pieces are involved, the other variables don't change the regression line much since 
                   Pieces has such a strong correlation.", style = "font-size:18px; padding-left: 15px; padding-right: 15px;")
               ),
               dashboardBody(
                 box(
                   title = "Lego Data Linear Regression Plot",
                   plotOutput("regression_plot2"),
                   verbatimTextOutput("regression_summary")
                 )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observes updates in choice of column
  observe({
    if (!is.null(lego_data)) {
      updateSelectInput(session, "dependent_var", choices = c("USD_MSRP", "Pieces", "Minifigures", "Current_Price", "Owned", "Ratings"))
      updateSelectInput(session, "independent_var", choices = c("Pieces", "Minifigures", "USD_MSRP","Current_Price","Owned", "Ratings"))
    }
  })
    
    output$thing <- renderPlot({
      print("Thing Happened")
    })
  
  # Plot linear regression line
  observeEvent(input$run_regression, {
    req(input$dependent_var, input$independent_var)
    
    formula <- as.formula(paste(input$dependent_var, "~", input$independent_var))
    model <- lm(formula, data = lego_data)
    
    correlation <- cor(lego_data[[input$dependent_var]], lego_data[[input$independent_var]])
    
    # Create a scatter plot with the linear regression line
    plot_data <- data.frame(x = lego_data[[input$independent_var]], y = lego_data[[input$dependent_var]])
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Linear Regression Plot", x = input$independent_var, y = input$dependent_var)
    
    output$regression_plot <- renderPlot({
      print(p)
    })
    
    output$summary <- renderPrint({
      summary(model)
    })
    
    output$correlation_output <- renderText({
      paste("Correlation Coefficient:", round(correlation, 4))
    })
  }
 )
  
  # Multilinear regression plot
  observeEvent(input$run_multiregression, {
    req(input$dependent, input$independent)
    
    independent_vars <- input$independent
    formula <- as.formula(paste(input$dependent, "~", paste(independent_vars, collapse = "+")))
    lm_model <- lm(formula, data = lego_data)
    
    
    p2 <- ggplot(lego_data, aes_string(y = input$dependent, x = paste(input$independent, collapse = "+"))) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +
          labs(title = "Multiple Linear Regression", x = input$dependent, y = "Fitted Values")
    
    output$regression_plot2 <- renderPlot({
      print(p2)
    })
    
    # Summary of Multi linear regression
    output$regression_summary <- renderPrint({
      summary(lm_model)
    })
  }
  )
}

shinyApp(ui, server)
