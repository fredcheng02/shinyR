# ===============================================
# Fill in the following fields
# ===============================================
# Title: Text Mining on the Harry Potter Series
# Description: Word trend and bigram analysis of the Harry Potter series
# Author: Feng Cheng
# Date: April 28th, 2023


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)


# ===============================================
# Import data
# ===============================================
load("rda-data-files/philosophers_stone.rda")
load("rda-data-files/chamber_of_secrets.rda")
load("rda-data-files/prisoner_of_azkaban.rda")
load("rda-data-files/goblet_of_fire.rda")
load("rda-data-files/order_of_the_phoenix.rda")
load("rda-data-files/half_blood_prince.rda")
load("rda-data-files/deathly_hallows.rda")
d1 = data.frame(text = philosophers_stone)
d2 = data.frame(text = chamber_of_secrets)
d3 = data.frame(text = prisoner_of_azkaban)
d4 = data.frame(text = goblet_of_fire)
d5 = data.frame(text = order_of_the_phoenix)
d6 = data.frame(text = half_blood_prince)
d7 = data.frame(text = deathly_hallows)
lst <- list(d1, d2, d3, d4, d5, d6, d7)

# give the index of the book
slct_book <- function(sel_no){
  sel = c('sel1', 'sel2', 'sel3', 'sel4', 'sel5', 'sel6', 'sel7')
  match(sel_no, sel)
}

# give the data.frame for the word trend
# acrs = chapters or books
trend_df <- function(tgt = 'harry', acrs = 'chapters', tks){
  if(acrs == 'chapters'){
    word_counts = tks |> 
      count(chapter, word, sort = TRUE) |> ungroup()
    word_tbl = word_counts |> filter(word == tgt) |> arrange(chapter)
  }
  else{
    word_counts = tks |> 
      count(book, word, sort = TRUE) |> ungroup()
    word_tbl = word_counts |> filter(word == tgt) |> arrange(book)
  }
  word_tbl
}

# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Harry Potter Series Text Analysis"),
  fluidRow(
    column(3,
           p(em("Word to be analyzed", br(), "(for word trend analysis)"), 
             br(), "note that cases are ignored, and stop words", 
             em("are not allowed")),
           textInput(inputId = "word", 
                        label = "Which word?", 
                        value = "harry")
    ),
    
    column(3,
           p(em("Across chapters or books"), br(), 
             "(if `chapters` is chosen, you also need to specify 
                the book on the right)"),
           radioButtons(inputId = "across", 
                        label = "Across", 
                        choices = c("chapters" = "chap",
                                    "books" = "bk"),
                        selected = "chap")
    ),
    
    column(3,
           p(em("Book selected:", br(), 
                "(for both word trend and bigram analyses)")),
           selectInput(inputId = "select", 
                       label = "Harry Potter and the ",
                       choices = c("Philosopher's Stone" = "sel1",
                                   "Chamber of Secrets" = "sel2",
                                   "Prisoner of Azkaban" = "sel3",
                                   'Goblet of Fire' = 'sel4',
                                   'Order of the Phoenix' = 'sel5',
                                   'Half-Blood Prince' = 'sel6',
                                   'Deathly Hallows' = 'sel7'),
                       selected = "sel1")
    ),
    
    column(3,
           p(em("Number of bars", br(), "(for bigram analysis)")),
           sliderInput(inputId = "size",
                       label = "How many top frequent bigrams",
                       min = 10,
                       max = 25,
                       value = 15),
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Trend Analysis",
                       h3("Trend of the typed word across chapters/books"),
                       plotOutput("plot1"),
                       p("P.S. When we are searching across the `books`, 
                       1 to 7 on the x-axis correspond to the chronological 
                       order of the \"Harry Potter\" books."),
                       hr(),
                       h4("Table for the word across chapters/books"),
                       dataTableOutput('table1')),
              tabPanel("Bigram Analysis", 
                       # h3(""),
                       plotOutput("plot2"),
                       hr(),
                       h4("Table for the top frequent bigrams"),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # vector of book names
  book <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")
  
  # target word set to lower case; for word trend
  target <- reactive(str_to_lower(input$word))
  
  # word trend table generator; acrs = 'chap' or 'bk'
  tbl_gen <- function(acrs = 'chap'){
    if(acrs == 'chap'){
      bk = lst[[slct_book(input$select)]]
      bk_df = data.frame(chapter = 1:nrow(bk), text = bk)
      tokens = bk_df |> unnest_tokens(word, text)
      tbl = trend_df(tgt = target(), acrs = 'chapters', tks = tokens)
    }
    else{
      bk_df = data.frame()
      for(book in 1:7){
        bk_df = rbind(bk_df, data.frame(book, text = lst[[book]]))
      }
      tokens = bk_df |> unnest_tokens(word, text)
      tbl = trend_df(tgt = target(), acrs = 'books', tks = tokens)
    }
    tbl
  }
  
  # bigram subset table
  bigrams_sub <- reactive({
    bigrams = lst[[slct_book(input$select)]] |> 
      unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) |> 
      filter(!is.na(bigram))
    
    bigrams_separated = bigrams |> 
      separate(bigram, c("word1", "word2"), sep = " ")
    bigrams_filtered = bigrams_separated |> 
      filter(!word1 %in% stop_words$word) |> 
      filter(!word2 %in% stop_words$word)
    
    bigrams_count = bigrams_filtered |> 
      count(word1, word2, sort = TRUE)
    bigrams_united = bigrams_count |> 
      unite(bigram, word1, word2, sep = " ")
    
    bigrams_united |> slice(1:input$size)
  })
  
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    if(target() %in% stop_words$word)
      stop('You have entered a stop word.')
    if(input$across == 'chap'){
      tbl = tbl_gen(acrs = 'chap') |> 
        complete(chapter = 1:nrow(lst[[slct_book(input$select)]])) |> 
        replace_na(replace = list(chapter = 0, word = target(), n = 0))
      p = ggplot(data = tbl, aes(x = chapter, y = n)) + 
        geom_line(color = 'green4', lwd = 1) + labs(
          y = 'count', title = paste0('Trend of the word \"', target(), 
                                      '\" in the \"', 
                                      book[slct_book(input$select)], '\"')) + 
        # geom_text(aes(label = n), nudge_x = 0.25, nudge_y = 0.25, size = 4) + 
        scale_x_continuous(breaks = 1:nrow(lst[[slct_book(input$select)]])) + 
        theme_bw() + 
        theme(panel.grid.minor.x = element_blank())
    }
    
    else{
      tbl = tbl_gen(acrs = 'bk') |> complete(book = 1:7) |> 
        replace_na(replace = list(chapter = 0, word = target(), n = 0))
      p = ggplot(data = tbl, aes(x = book, y = n)) + 
        geom_line(color = 'cyan4', lwd = 0.8) + labs(
          y = 'count', title = paste0('Trend of the word \"', target(), 
                                      '\" across all 7 books')) + 
        geom_text(aes(label = n), nudge_x = 0.2, nudge_y = 0.1, size = 4) + 
        scale_x_continuous(breaks = 1:7) + theme_bw() + 
        theme(panel.grid.minor.x = element_blank())
    }
    p
  })
  
  # frequencies in data table
  output$table1 <- renderDataTable({
    df = tbl_gen(acrs = input$across)
    colnames(df)[3] = 'count'
    df[,c(2,1,3)]
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  output$plot2 <- renderPlot({
    ggplot(data = bigrams_sub(), aes(x = reorder(bigram, n), y = n)) +
      geom_col(fill = 'steelblue') + geom_text(aes(label = n), nudge_y = 1) + 
      coord_flip() + 
      labs(title = paste0("Top ", input$size," frequent bigrams in the \"", 
                         book[slct_book(input$select)], "\""), x = 'bigram', 
           y = 'count') +
      theme_minimal()
  })
  
  output$table2 <- renderDataTable({
    df = bigrams_sub()
    colnames(df) = c('bigram', 'count')
    df
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

