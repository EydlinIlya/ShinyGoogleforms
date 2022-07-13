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
    titlePanel("Shiny with Googleforms water tracker"),
    
    # Sidebar with a slider input widget
    sidebarLayout(
        sidebarPanel(
            numericInput("water", "Add water (ml)", 0, min = 0, step = 1),
            actionButton("submit", "Submit")
        ),
        
        # Show a plot 
        mainPanel(
            
            
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
            data <- read_sheet("1tGW9iaYANpLduruY1f2m_WapwIwmx-QcSWw7fVjuYjY", 
                               col_types = "Tcn") %>% 
                mutate(Timestamp = as.Date(Timestamp)) %>% 
                dplyr::filter(user == res_auth$user) }
            
           
    })
    observeEvent(input$submit,  {
        print(res_auth$user)
        print(input$water)
        user <- URLencode(res_auth$user)
        water <- URLencode(as.character(input$water))
        url <- glue("https://docs.google.com/forms/d/e/1FAIpQLSfrl28Ujq-BLjPxlAzN1SwFDJvWfdhc2OL7uFSxSwY_F0SSow/formResponse?usp=pp_url&entry.1606266665={user}&entry.1695787879={water}")
        res <- POST(
            url = url)
    }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)