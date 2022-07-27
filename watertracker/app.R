library(shiny)
library(httr)
library(glue)
library(shinymanager)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(ggthemes)
library(extrafont)

# creds for shinymanager
credentials <- data.frame(
    user = c("Ilya", "Tata"), 
    password = c("54321", "12345"), 
    stringsAsFactors = FALSE
)

ui <- dashboardPage(
    dashboardHeader(title = "Water tracker"),
    dashboardSidebar( numericInput("water", "Add water (ml)", 0, min = 0, step = 1),
                      actionButton("submit", "Submit")),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(title = "Today", plotOutput("p_col", click = "plot_click"), width = 4), 
            box(title = "Last 30 days", plotOutput("p_line", click = "plot_click"), width = 8)
        ),
        fluidRow(
            box(
                title = "About", width = 12, background = "light-blue",
                "This is the demonstartion of the googlesheets working as a batabase for the tutorial"
            )
        )
    )
) 

ui <- secure_app(ui)

server <- function(input, output) {
    #initialize sheet
    rv <- reactiveValues(sheet = NULL)
    # read google sheet every 10 seconds (fits for up to three users because of google api linitations)
    autoInvalidate <- reactiveTimer(10000)
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    # dont ask for googlesheet auth
    gs4_deauth()
    
    # read data from google sheet 
    readsheet <- function(id, user) {
        data <- read_sheet(id, 
                           col_types = "Tcn") %>% 
            mutate(Date = as.Date(Timestamp)) %>% 
            dplyr::filter(user == user & (Date - Sys.Date()) <= 30) %>% 
            group_by(Date) %>% 
            summarise(ml = sum(ml, na.rm = T))
        
    }
    # plot col plot for current date
    plot1 <- function(data) {
        p_col <- 
            ggplot(data %>% dplyr::filter(Date == Sys.Date()), aes(x = Date, y = ml)) +
            geom_col(data = data.frame(Date = Sys.Date(), ml = 2000), aes(y = ml), fill = "#1A1A1A") +
            geom_col(width = 0.7, fill = "#2E45B8") +
            theme_economist() +
            theme(panel.grid.major.x = element_blank(), axis.text.x=element_blank(), axis.title.x=element_blank())
    }
    # plot line chart for 30 days
    plot2 <- function(data) {
        
        p_line <- ggplot(data, aes(Date, ml)) +
            geom_line(aes(y = ml), colour =  "#2E45B8") +
            geom_point() +
            theme_economist()
        
    }
    
    observe({
        autoInvalidate()
        
        if(!is.null(res_auth$user)){
    
            data <- readsheet("1tGW9iaYANpLduruY1f2m_WapwIwmx-QcSWw7fVjuYjY", res_auth$user)
            #reload plots whenever data is changed
            if(is.null(rv$sheet) | !all(data == rv$sheet)) {
                rv$sheet <- data
            p_col <- plot1(data)
            output$p_col <- renderPlot(p_col)
            p_line <- plot2(data)
            output$p_line <- renderPlot(p_line)
            }
            }
        
        
        
    })
    observeEvent(input$submit,  {
        user <- URLencode(res_auth$user)
        water <- URLencode(as.character(input$water))
        url <- glue("https://docs.google.com/forms/d/e/1FAIpQLSfrl28Ujq-BLjPxlAzN1SwFDJvWfdhc2OL7uFSxSwY_F0SSow/formResponse?usp=pp_url&entry.1606266665={user}&entry.1695787879={water}")
        res <- POST(
            url = url)
        rv$sheet <- readsheet("1tGW9iaYANpLduruY1f2m_WapwIwmx-QcSWw7fVjuYjY", res_auth$user)
        p_col <- plot1(rv$sheet)
        output$p_col <- renderPlot(p_col)
        p_line <- plot2(rv$sheet)
        output$p_line <- renderPlot(p_line)
    }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)