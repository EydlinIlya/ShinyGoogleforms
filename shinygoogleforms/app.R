library(shiny)
library(httr)
library(glue)
library(shinymanager)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggplot2)


credentials <- data.frame(
    user = c("Ilya", "Tata"), 
    password = c("54321", "12345"), 
  stringsAsFactors = FALSE
)

ui <- fluidPage(
    
    # Application title
    titlePanel("Shiny with Googleforms calorie tracker"),
    
    # Sidebar with a slider input widget
    sidebarLayout(
        sidebarPanel(
            titlePanel("Add new dish"),
            textInput("prod", "Product name"),
            numericInput("weight", "Grams", 0, min = 0, step = 0.1),
            numericInput("calories", "kkal (per 100 gram)", 0, min = 0, step = 0.1),
            numericInput("fats", "Fats (per 100 gram)", 0, min = 0, step = 0.1),
            numericInput("sugars", "Sugars (per 100 gram)", 0, min = 0, step = 0.1),
            numericInput("proteins", "Proteins (per 100 gram)", 0, min = 0, step = 0.1),
            actionButton("submit", "Submit")
            ),
        
        # Show a plot 
        mainPanel(
          plotOutput('data')
           
    )
)
)
ui <- secure_app(ui)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(3000)
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    gs4_deauth()
    
    observe({
    autoInvalidate()
      if(!is.null(res_auth$user)){
    data <- read_sheet("1Ak6QV96Nbnz7siJOUpjuvAEhWcoUmWZGpCYB_QWN9UU", 
                       col_types = "Tcnnnnnc") %>% 
      mutate(Timestamp = as.Date(Timestamp)) %>% 
      dplyr::filter(User == res_auth$user)
    
    cals <- data %>%
      group_by(Timestamp)  %>% 
      summarise(Calories = sum(Calories*Weight/100, na.rm = T)) %>% 
      replace_na(list(Calories=0))
    p <- ggplot(cals, aes(x=Timestamp, y=Calories)) +
      geom_point()
    output$data <- renderPlot(p)}
      })
    observeEvent(input$submit,  {
        user <- URLencode(res_auth$user)
        prod <- URLencode(input$prod)
        weight <- URLencode(as.character(input$weight))
        calories <- URLencode(as.character(input$calories))
        fats <- URLencode(as.character(input$fats))
        sugars <- URLencode(as.character(input$sugars))
        proteins <- URLencode(as.character(input$proteins))
        calories <- URLencode(as.character(input$calories))
        url <- glue("https://docs.google.com/forms/d/e/1FAIpQLScRvvI14N1wfb06f0bpiQ4mY65GpfpgAf6iZ47SyO7m-8L6-w/formResponse?usp=pp_url&entry.2136202261={prod}&entry.1081510207={weight}&entry.1407652810={proteins}&entry.1079127786={fats}&entry.489852779={sugars}&entry.1285733439={calories}&entry.1278145968={user}")
        res <- POST(
            url = url)
    }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)