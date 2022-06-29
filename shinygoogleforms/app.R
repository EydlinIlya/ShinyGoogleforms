library(shiny)
library(httr)
library(glue)

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
           
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$submit,  {
        prod <- URLencode(input$prod)
        weight <- URLencode(as.character(input$weight))
        calories <- URLencode(as.character(input$calories))
        fats <- URLencode(as.character(input$fats))
        sugars <- URLencode(as.character(input$sugars))
        proteins <- URLencode(as.character(input$proteins))
        calories <- URLencode(as.character(input$calories))
        url <- glue("https://docs.google.com/forms/d/e/1FAIpQLScRvvI14N1wfb06f0bpiQ4mY65GpfpgAf6iZ47SyO7m-8L6-w/formResponse?usp=pp_url&entry.2136202261={prod}&entry.1081510207={weight}&entry.1407652810={proteins}&entry.1079127786={fats}&entry.489852779=aefds&entry.1285733439={sugars}")
        print(url)
        res <- POST(
            url = url)
    }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)