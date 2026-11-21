library(shiny)
library(bslib)
library(readr)
library(tidyverse)
library(plotly)
library(manipulate)

loan <- read_csv("loan_cleaned.csv") %>%
mutate(loan_status = as.factor(if_else(loan_status == 1, "Approved", "Rejected")),
       previous_loan_defaults_on_file = ifelse(previous_loan_defaults_on_file == "Yes", 1, 0),
       previous_loan_defaults_on_file = factor(previous_loan_defaults_on_file))

fit <- glm(loan_status ~ person_age + person_gender + person_income + loan_amnt + credit_score,
           data = loan,
           family = binomial(link = "logit"))
# Define UI for application

# Need to figure out navbarMenu,.. to make tab
ui <- page_navbar(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Loan Evaluation dashboard",
  
  sidebar = sidebar( 
    width = 350,
    # ... [Keep your existing sidebar inputs exactly as they are] ...
    h5("Filter Control"),
    helpText("Adjust graphs without resetting inputs:"),
    radioButtons("filter_gender", "Filter Graphs by Gender:", 
                 choices = c("All", "Female", "Male"), selected = "Female", inline = TRUE),
    selectInput("filter_age", "Filter Graphs by Age Bracket:",
                choices = c("All", "18-30", "30-50", "50-70", "70+"), 
                selected = "18-30"),
    hr(),
    h4("Applicant Profile: "),
    numericInput("age", "Person Age:", value = 18, min = 18, max = 95, step = 1),
    selectInput("gender", "Person Gender:", choices = c("Female", "Male")),
    numericInput("income", "Annual Income ($): ", value = 5000, min = 5000, max= 2500000, step = 100),
    numericInput("amount", "Requested Loan Amount($):", value = 500, min = 500, max = 35000, step = 25),
    sliderInput("credit", "Credit Score: ", value = 600, min = 300, max = 850, step = NULL),
    actionButton("prediction_button", "Run Prediction")
  ),
  
  layout_columns(
    col_widths = c(6, 6, 12),
    card(plotlyOutput("plot1", height = "400px")),
    card(plotlyOutput("plot2", height = "400px")),
    card(
      card_header("EVALUATION RESULT"),       
      uiOutput("prediction_result")         
    )
  )
)


# Server
server <- function(input, output) {
  user_prediction <- eventReactive(input$prediction_button, ignoreNULL = FALSE, {
    new_data <- data.frame(
      person_age = input$age,
      person_gender = tolower(input$gender), # Convert "Female"/"Male" to "female"/"male"
      person_income = input$income,
      loan_amnt = input$amount,
      credit_score = input$credit
    )
    
    
    prob_rejected <- predict(fit, newdata = new_data, type = "response")
    prob_approved <- 1 - prob_rejected
 
    status <- ifelse(prob_approved >= 0.5, "Approved", "Rejected")
    
    list(prob = prob_approved, status = status)
  })
  
 
  output$prediction_result <- renderUI({
    res <- user_prediction()
    
    # Change colors based on status (Green for Approved, Red for Rejected)
    bg_color <- ifelse(res$status == "Approved", "#d4edda", "#f8d7da")
    text_color <- ifelse(res$status == "Approved", "#155724", "#721c24")
    
    div(
      style = sprintf("background-color: %s; color: %s; padding: 15px; border-radius: 5px; text-align: center;", bg_color, text_color),
      h3(sprintf("Predicted Status: %s", res$status), style = "margin-top: 0;"),
      h5(sprintf("Approval Probability: %.1f%%", res$prob * 100), style = "margin-bottom: 0;")
    )
  })
  output$plot1 <- renderPlotly({
    if (input$filter_age == "All")
      loan_filtered <- loan
    else if (input$filter_age == "18-30") {
      loan_filtered <- loan %>%
        filter(person_age >= 18 & person_age < 30)
    }
    else if (input$filter_age == "30-50")
      loan_filtered <- loan %>%
        filter(person_age >= 30 & person_age < 50)
    else if (input$filter_age == "50-70")
      loan_filtered <- loan %>%
        filter(person_age >= 50 & person_age < 70)
    else
      loan_filtered <- loan %>%
        filter(person_age >= 70)
    
    if (input$filter_gender == "All")
      loan_filtered <- loan_filtered
    else if (input$filter_gender == "Female")
      loan_filtered <- loan_filtered %>%
        filter(person_gender == "female")
    else
      loan_filtered <- loan_filtered %>%
        filter(person_gender == "male")
    
    
    p1 <- ggplot(loan_filtered, aes(x=person_income, y=credit_score)) +
           geom_point(aes(color=loan_status), alpha=1, size=1) +
           stat_smooth(method = "glm", family = "binomial") +
           scale_x_log10() +
           coord_cartesian(xlim=c(5000, 2500000), ylim=c(300, 850)) +
           labs(title="Income vs Credit Score", x="Annual Income ($)", 
                y="Credit Score")
   ggplotly(p1)
  })
  
  output$plot2 <- renderPlotly({
    if (input$filter_age == "All")
      loan_filtered <- loan
    else if (input$filter_age == "18-30") {
      loan_filtered <- loan %>%
        filter(person_age >= 18 & person_age < 30)
    }
    else if (input$filter_age == "30-50")
      loan_filtered <- loan %>%
        filter(person_age >= 30 & person_age < 50)
    else if (input$filter_age == "50-70")
      loan_filtered <- loan %>%
        filter(person_age >= 50 & person_age < 70)
    else
      loan_filtered <- loan %>%
        filter(person_age >= 70)
    
    if (input$filter_gender == "All")
      loan_filtered <- loan_filtered
    else if (input$filter_gender == "Female")
      loan_filtered <- loan_filtered %>%
        filter(person_gender == "female")
    else
      loan_filtered <- loan_filtered %>%
        filter(person_gender == "male")
    
    
    p2 <- ggplot(loan_filtered) +
            geom_boxplot(aes(x=income_group, y=loan_amnt, fill=loan_status), alpha=0.5) +
            coord_cartesian(ylim=c(500, 35000)) +
            labs(title="Distributions of Requested Loan Amounts by Approval Status 
                 Across Income Groups", x="Income Group", y="Loan Amount ($)")
    ggplotly(p2) %>%
      layout(boxmode = "group")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
