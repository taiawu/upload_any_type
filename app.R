library(readxl)
library(vroom)
library(tidyverse)
library(shinyalert)
library(shinycssloaders)
library(shiny)

source("scripts/upload_formatting.R")
source("scripts/uploads_module.R")

moduleServer <- function(id, module) {
    callModule(module, id)
}

ui <- navbarPage(useShinyalert(),
                 tags$head( # make horizontal lines darker
                     tags$style(HTML("hr {border-top: 1px solid #bdbdbd;}"))
                     ),
                 
                 uploadsTabUI("uploads") # sidebar layout, all uploads
) 

server <- function(input, output, session) {
   uploadsTabServer("uploads") # server for uploading data
}

shinyApp(ui = ui, server = server)