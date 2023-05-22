# Title: Mortgage Calculator
# Description: Shiny app that computes mortgage
# Details: this app replicates Calculator.net's mortgage calculator
# Author: Feng Cheng
# Date: April 14th, 2023
# Source: https://www.calculator.net/mortgage-calculator.html

library(shiny)
library(tidyverse)
library(DT)

# ===============================================
# Auxiliary Function(s)
# Include any auxiliary function(s)
# ===============================================

amortization <- function(principal = 300000, interest = 0.068, 
                         year = 1, period = 12){
  if(period > 0){
    principal / ((1 + interest / period)^(year * period) - 1) * interest / 
      period * (1 + interest / period)^(year * period)
  }
  else
    stop('The number of periodic payments in a year must be positive.')
}

calc <- "https://www.calculator.net/mortgage-calculator.html"

annual_amort_sch <- function(prin = 300000, inter = 0.068, yr = 30){
  term <- 1:yr
  payment <- amortization(prin, inter, yr, 12) * 12
  balance <- prin * (1 + inter / pd)^term - payment * pd / inter * 
    ((1 + inter / pd)^term - 1)
  interest_paid <- balance * inter / pd
  principal_paid <- payment - interest_paid
  data.frame(Year = term, Interest = balance, Principal = payment, 
             interest_paid, ending_balance)
}

annual_amort_sch <- function(prin = 300000, inter = 0.068, yr = 30, pd = 12){
  year <- 1:yr
  term <- year * pd
  payment <- amortization(prin, inter, yr, pd)
  end_balance <- prin * (1 + inter / pd)^term - payment * pd / inter * 
    ((1 + inter / pd)^term - 1)
  end_balance[yr] <- 0
  begin_balance <- c(prin, end_balance[-yr])
  principal_paid <- begin_balance[1:yr] - end_balance[1:yr]
  interest_paid <- payment * pd - principal_paid
  
  data.frame(year, begin_balance, 
             interest_paid, principal_paid, end_balance)
}

# ===============================================
# Define UI for a Mortgage Calculator app
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Mortgage Calculator"),
  HTML(paste0("<p>Based on <a href='", calc, "'>", calc, "</a></p>")),
  
  sidebarLayout(
    
    # -------------------------------------------------------
    # Sidebar with input widgets
    # -------------------------------------------------------
    sidebarPanel(
      numericInput(inputId = "home_price",
                   label = "Home Price ($)",
                   value = 400000),
      numericInput(inputId = "down_pay",
                   label = "Down Payment (%)",
                   value = 20),
      numericInput(inputId = 'loan_term',
                   label = 'Loan Term (years)',
                   value = 30),
      numericInput(inputId = 'int_rate',
                   label = 'Interest Rate (%)',
                   value = 6.8),
      
    ),  # closes sidebarPanel of inputs
    
    
    # -------------------------------------------------------
    # Main Panel of outputs: summary, graphs, and table
    # -------------------------------------------------------
    mainPanel(
      fluidRow(
        column(5,
               h4("Summary Information"),
               verbatimTextOutput("summary")
        ), # closes column-1
        
        column(5,
               h4("Payment Breakdown"),
               plotOutput("piechart")
        ) # closes column-2
      ), # closes fluidRow
      
      hr(), # horizontal rule
      h4("Mortgage Amortization Graph"),
      plotOutput("graph"),
      
      hr(), # horizontal rule
      h4("Annual Amortization Schedule Table"),
      dataTableOutput("table")
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage



server <- function(input, output) {

  monthly_pay <- reactive({
    pay = amortization(principal = input$home_price * 
                         (1 - input$down_pay / 100), 
                       interest = input$int_rate / 100, 
                       year = input$loan_term,
                       period = 12)
    pay
  }) # monthly payment
  
  ttl_interest <- reactive(12 * input$loan_term * monthly_pay() - 
   input$home_price * (1 - input$down_pay / 100)) # total paid interest
  
  amort_table <- reactive({
    tbl = annual_amort_sch(prin = input$home_price * 
                                            (1 - input$down_pay / 100), 
                                          inter = input$int_rate / 100, 
                                          yr = input$loan_term,
                                          pd = 12)
    tbl
  }) # table of amortization
  
  # -------------------------------------------------------
  # 1) Summary Information
  # -------------------------------------------------------
  output$summary = renderPrint({
    cat("Monthly Pay: ", "$", monthly_pay(), "\n", sep = "")
    cat("House Price: ", "$", input$home_price, "\n", sep = "")
    cat("Loan Amount: ", "$", input$home_price * 
          (1 - input$down_pay / 100), "\n", sep = "")
    cat("Down Payment: ", "$", input$home_price * 
          input$down_pay / 100, "\n", sep = "")
    cat("Total of ", 12 * input$loan_term, " Mortgage Payments: ", 
        "$", 12 * input$loan_term * monthly_pay(), "\n", sep = "")
    cat("Total Interest: ", "$", ttl_interest(), "\n", sep = "")
  })
  

  # -------------------------------------------------------
  # 2) Piechart of Payment Breakdown
  # -------------------------------------------------------
  output$piechart <- renderPlot({
    breakdown = data.frame(amount = c('Principal', 'Interest'), 
                           value = c(input$home_price * 
                                       (1 - input$down_pay / 100), 
                                     ttl_interest()) / 
                             (12 * input$loan_term * monthly_pay()))
    breakdown = mutate(breakdown, ypos = c(breakdown$value[1] / 2, 
                                            breakdown$value[1] + 
                                              breakdown$value[2] / 2))
    # ypos for the position of texts in the pie chart
    ggplot(data = breakdown, aes(x = "", y = value, fill = amount)) +
      geom_bar(stat = "identity",
               width = 1,
               color = "white") +
      coord_polar("y", start = 0) +
      theme_void() + 
      scale_fill_manual(values = c("#2B7DDB", "#8bbc21")) +
      geom_text(aes(y = ypos, label = paste0(round(value, 2) * 100, '%')),
                color = "white",
                size = 4) + theme(legend.title = element_blank())
  })

    
  # -------------------------------------------------------
  # 3) Amortization Graph
  # -------------------------------------------------------
  output$graph <- renderPlot({
    df = rbind(data.frame(year = 0, begin_balance = 0, interest_paid = 0, 
                          principal_paid = 0, 
                          end_balance = 
                            input$home_price * (1 - input$down_pay / 100)), 
                 amort_table())
    df = df |> mutate(cumPay = 0:(nrow(df) - 1) * monthly_pay() * 12)
    options(scipen = 100)
    ggplot(data = df, aes(x = year)) + 
      geom_line(aes(y = end_balance, color = 'Balance'), lwd = 1) + 
      geom_line(aes(y = cumsum(interest_paid), color = 'Interest'), lwd = 1) + 
      geom_line(aes(y = cumPay, color = 'Payment'), lwd = 1) + 
      scale_x_continuous(breaks = seq(0, nrow(df), 5)) + 
      scale_color_manual(values = c('Balance' = '#2B7DDB',
                                    'Interest' = '#8bbc21',
                                    'Payment' = '#85190f')) + theme_bw() + 
      theme(legend.key = element_rect(fill = "lightyellow"), 
            legend.title = element_blank()) + labs(x = 'Year', y = '(in $)')
  })
  
  
  # -------------------------------------------------------
  # 4) Amortization Table
  # -------------------------------------------------------
  output$table <- renderDataTable({
    out_tbl = amort_table()
    colnames(out_tbl) = c('Year', 'Beginning Balance', 'Interest', 
                                           'Principal', 'Ending Balance')
     out_tbl %>%
      datatable() %>%
      formatRound(
        columns = c("Beginning Balance", "Interest", "Principal", 
                    "Ending Balance"),
        digits = 2)
  })  
}


# Run the application 
shinyApp(ui = ui, server = server)
