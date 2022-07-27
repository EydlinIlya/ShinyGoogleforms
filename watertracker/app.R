library(shiny)
library(httr)
library(glue)
library(shinymanager)
library(googlesheets4)
library(dplyr)
library(ggplot2)

# creds for shinymanager
credentials <- data.frame(
    user = c("Ilya", "Tata"), 
    password = c("54321", "12345"), 
    stringsAsFactors = FALSE
)

ui <- fluidPage(
    
    # Application title
    titlePanel("Shiny with Googleforms water tracker"),
    
    # Sidebar with input widgets
    sidebarLayout(
        sidebarPanel(
            numericInput("water", "Add water (ml)", 0, min = 0, step = 1),
            actionButton("submit", "Submit")
        ),
        
        # Show plots for today and for whole month 
        mainPanel(
            plotOutput("p_col", click = "plot_click"),
            plotOutput("p_line", click = "plot_click")
            
        )
    )
)
ui <- secure_app(ui)

server <- function(input, output) {
    # read google sheet every 3 seconds (fits for up to three users because of google api linitations)
    autoInvalidate <- reactiveTimer(3000)
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    # dont ask for googlesheet auth
    gs4_deauth()
    
    
    observe({
        autoInvalidate()
        if(!is.null(res_auth$user)){
            data <- read_sheet("1tGW9iaYANpLduruY1f2m_WapwIwmx-QcSWw7fVjuYjY", 
                               col_types = "Tcn") %>% 
                mutate(Date = as.Date(Timestamp)) %>% 
                dplyr::filter(user == res_auth$user & (Date - Sys.Date()) <= 30) %>% 
                group_by(Date) %>% 
                summarise(ml = sum(ml, na.rm = T))
            
            
            p_col <- 
                ggplot(data %>% dplyr::filter(Date == Sys.Date()), aes(x = Date, y = ml)) +
                geom_col(data = data.frame(Date = Sys.Date(), ml = 2000), aes(y = ml), fill = "grey") +
                geom_col(width = 0.7, fill = "blue") +
                theme_minimal() +
                theme(panel.grid.major.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank())
            output$p_col <- renderPlot(p_col)
            
            p_line <- ggplot(data, aes(Date, ml)) +
                geom_line(aes(y = ml), color = "blue") +
                geom_point() +
                theme_minimal()
            output$p_line <- renderPlot(p_line)
        }
        
        
        
    })
    observeEvent(input$submit,  {
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