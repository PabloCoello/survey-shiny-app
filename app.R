library(shiny)
library(jsonlite)
library(rdrop2)

#token <- drop_auth()
#saveRDS(token, file="token.rds")

ui <- fluidPage(
    selectInput(
        inputId = "lang",
        label = "Choose a language",
        choices = c(Español="sp", Français="fr", Português="pt")
    ),
    uiOutput("tab")
)

loadData <- function(dir, file, token) {
    path <- paste(dir, file, sep='')
    data <- drop_read_csv(path, dtoken = token)
    return(data)
}

saveData <- function(data, filename=file, outputdir=dir, token) {
    filePath <- file.path(tempdir(), filename)
    write.csv(data, filePath, row.names=FALSE, quote = TRUE)
    drop_upload(filePath, path=outputdir, dtoken=token)
}

return_version <- function(input, status_list) {
    # Return TRUE if there are more v1 than v2
    version1 <- paste(input$lang, 'v1', sep='')
    version2 <- paste(input$lang, 'v2', sep='')
    v1 <- status_list[which(status_list$version == version1),'count']
    v2 <- status_list[which(status_list$version == version2),'count']
    return(v1 > v2)
}

server <- function(input, output) {
    token <- readRDS('token.rds')
    dir <- '/conf_jotform/'
    file <- 'status.csv'
    url_list <- read_json("./conf/url.json")
    status_list <- loadData(dir, file, token)

    
    output$tab <- renderUI({
        if(return_version(input, status_list)) {
            ref <- paste(input$lang, 'v2', sep='')

        }else{
            ref <- paste(input$lang, 'v1', sep='')
        }
        url <- a("Press here to start the survey", href=url_list[[ref]])
        
        status_list[which(status_list$version == ref),'count'] = 
          status_list[which(status_list$version == ref),'count'] + 1
        saveData(status_list, file, dir, token)
        
        tagList("URL link:", url)
    })
}

shinyApp(server = server, ui = ui)