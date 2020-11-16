library(shiny)
library(jsonlite)

ui <- fluidPage(
    selectInput(
        inputId = "lang",
        label = "Choose a language",
        choices = c(Español="sp", Français="fr", Português="pt")
    ),
    uiOutput("tab")
)

return_version <- function(input, status_list) {
    # Return TRUE if there are more v1 than v2
    v1 <- status_list[[paste(input$lang, 'v1', sep='')]]
    v2 <- status_list[[paste(input$lang, 'v2', sep='')]]
    return(v1 > v2)
}

server <- function(input, output) {
    url_list <- read_json("./conf/url.json")
    status_list <- read_json("./conf/status.json")
    
    output$tab <- renderUI({
        if(return_version(input, status_list)) {
            url <- a("Press here to start the survey", href=url_list[[paste(input$lang, 'v2', sep='')]])
        }else{
            url <- a("Press here to start the survey", href=url_list[[paste(input$lang, 'v1', sep='')]])
        }
        tagList("URL link:", url)
    })
}

shinyApp(server = server, ui = ui)