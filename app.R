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
                  sidebarLayout( # Sidebar layout with input and output definitions
                   sidebarPanel( uploadsTabUI("uploads"),
                                 
                                 actionButton('jumpToAnalysis', p("Analyze", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                                              icon("chart-area"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff"),
                                 ),
                    mainPanel(
                      tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
                      tableOutput("data_raw_table") %>% withSpinner(color="#525252"), style = "overflow-x: scroll;overflow-y: scroll;height:580px"
                    ) # end main panel
                   )# sidebar layout, all uploads
) 

server <- function(input, output, session) {
   data_raw <- uploadsTabServer("uploads")  # server for uploading data
   output$data_raw_table <- renderTable({ data_raw() })
  
}

shinyApp(ui = ui, server = server)