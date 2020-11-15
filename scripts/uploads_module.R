
# must set the following to run. I like to do this in the main app
# moduleServer <- function(id, module) {
#   callModule(module, id)
# }

uploadsTabUI <- function(id) {
  
 #  sidebarLayout( # Sidebar layout with input and output definitions
    
  tagList(
  #   sidebarPanel( # Sidebar panel for inputs
      fileInput(NS(id, "file"), p("Browse or drag-and-drop raw (RFU) DSF data", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
                multiple = FALSE,
                accept = c(".csv", ".tsv", ".xls", ".xlsx")),
      checkboxInput(NS(id, "name_to_well"), "Overwrite column names with wells", FALSE), # Input: Checkbox if file has header
      
      tags$hr(),
      p("Upload as exported from instrument", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
      radioButtons(NS(id, "reformat"), "", # Input: Select type of reformatting necessary
                   choices = c(None = "none",
                               Biorad = "biorad",
                               Stratagene = "stratagene",
                               quantStudio = "quantStudio",
                               qTower = "qTower" # will have to figure out how to deal with multiple reader errors
                   ), selected = "none"),
      selectInput(NS(id,"qT_channel"), "Select channel (qTower only)",
                  c("For qTower, select a channel" = "None",
                    "FAM" = "FAM",
                    "JOE" = "JOE",
                    "TAMRA" = "TAMRA",
                    "ROX" = "ROX",
                    "Cy5" = "Cy5",
                    "Cy5.5" = "Cy5.5",
                    "SYPRO" = "SyPro")),
      tags$hr(),
      checkboxInput(NS(id, "cycle_to_T"), "Convert cycle number to temperature", FALSE),
      splitLayout(
        numericInput(NS(id, "start_T"), label="Starting Temp (ºC)", value = 25),
        numericInput(NS(id,"increment_T"), label="Increase per cycle (ºC)", value = 1)
      )# ,
      # actionButton(NS(id,'jumpToAnalysis'), p("Analyze", style = "font-family: 'Avenir Next'; font-size: 14px; color: black",align = "center"),
      #              icon("chart-area"), width = '100%', style="font-size: 14px; color: #00000; background-color: #fffff; border-color: #ffff")
    # ),  # end sidebar panel
    
    # Main panel for displaying outputs
   #  mainPanel(
   #    tags$style(type='text/css', "#instructions {font-size: 18px; line-height: +2;} "),
   #    
   #    tableOutput(NS(id,"opt_format")) %>% withSpinner(color="#525252"), style = "overflow-x: scroll;overflow-y: scroll;height:580px"
   # # ) # end main panel
  #)
  )
}


uploadsTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    data_raw_upload <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      tryCatch({ read_uploads(input$file$datapath, ext)
      }, error  = function(e) {
        shinyalert("File needs reformatting", "Select your instrument from 'Reformat raw from instrument', or format your data as shown in the downloadable template. Please ensure file is a csv, tsv, xls, or xlsx.")
      })
    })
    #
    df_instr_format <- reactive({
      req(data_raw_upload())
      
      instr <- input$reformat
      
      if(instr == "qTower") { req(input$qT_channel != "None") }
      
      tryCatch({
        switch(instr,
               none = data_raw_upload(),
               biorad = format_biorad(data_raw_upload()),
               stratagene = format_stratagene(data_raw_upload()),
               qTower = qTower_load(input$file$datapath, input$qT_channel),
               quantStudio = read_quantStudio(input$file$datapath),
               validate("Invalid instrument selection") )
      }, error  = function(e) {
        shinyalert("Instrument formatting unsuccessful", "Please select another instrument, or format data as shown in the downloadable template.")
      })
      
    })
    #
    df_opts_format <- reactive({
      req(df_instr_format())
      
      df_format <- df_instr_format()      
      
      tryCatch({
        # convert to well names
        if (input$name_to_well == TRUE) {
          df_format <- df_instr_format() %>% set_names(c("Temperature", WELLS1[c(1:(ncol(.)-1))]))
        }
        
        # cycle number to temperature
        if (input$cycle_to_T == TRUE) {
          Temps_calc <- cycle_to_T_func(as.numeric(input$start_T), as.numeric(input$increment_T), df_instr_format())
          df_format <- dplyr::bind_cols(Temps_calc, df_instr_format())[-2]
          names(df_format)[1] <- "Temperature"
        }
      }, error  = function(e) {
        shinyalert("Instrument formatting unsuccessful", "Please select another instrument, or format data as shown in the downloadable template.")
      })
      
      tryCatch({
        names(df_format)[1] <- "Temperature"
        
        df_format <- df_format %>% # in case someone has a file that reads in as characters
          mutate_if(is.factor, as.character) %>% # make any factors characters
          mutate_all(as.numeric)  %>% # make all numeric
          filter_all(any_vars(!is.na(.))) %>% # drop any rows which are all NAs
          discard(~all(is.na(.x))) # drop any columns which are all NAs
        
        df_format
      }, error = function(e) {
        shinyalert("File needs reformatting", "Select your instrument from 'Reformat raw from instrument', or format your data as shown in the downloadable template. Please ensure file is a csv, tsv, xls, or xlsx.")
      })
    })
    #
    # output$opt_format <- renderTable({
    #   df_opts_format()
    # })
    reactive(df_opts_format()) # this the assignable output from this module
  })
  

}