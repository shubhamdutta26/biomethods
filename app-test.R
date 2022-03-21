library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(paletteer)

header <- dashboardHeader(title = "Biomethods")

sidebar <- dashboardSidebar(
  source("scripts/sidebar.R", local=T)$value
)

body <- dashboardBody(
  tabItems(
    source("scripts/abEngin.R", local=T)$value,
    source("scripts/abConjug.R", local=T)$value,
    source("scripts/fedBatch.R", local=T)$value,
    tabItem(tabName = "qpcr",
            h2("This is Quantitative Real-Time PCR tab (under construction)")
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output, session) {
  
  # Antibody engineering begins
  
  data_hc <- reactive({source("scripts/abEnginHC.R", local=T)$value})
  
  data_lc <- reactive({source("scripts/abEnginLC.R", local=T)$value})
  
  signal_peptide <- reactive({source("scripts/abEnginSP.R", local=T)$value})
  
  restr_enz_5 <- reactive({source("scripts/abEnginRE5.R", local=T)$value})
  
  restr_enz_3 <- reactive({source("scripts/abEnginRE3.R", local=T)$value})
  
  kozak <- reactive({source("scripts/abEnginKZ.R", local=T)$value})
  
  output$fullHeavy <- renderText(paste(restr_enz_5(), kozak(), signal_peptide(), input$vh_input, data_hc(), restr_enz_3(), sep = ""))
  output$fullLight <- renderText(paste(restr_enz_5(), kozak(), signal_peptide(), input$vl_input, data_lc(), restr_enz_3(), sep = ""))
  
  # Antibody engineering ends 
  
  # Cell line titer starts
  
  stableData <- reactive({
    file <- input$stableData
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "xls", "Please upload a xls file"))
    read_excel(file$datapath)
  })
  
  # output$maxTiter <- renderText({stableData() %>% slice_max(titer) %>% pull(titer)})
  # output$winClone <- renderText({stableData() %>% slice_max(titer) %>% pull(clone)})
  # output$finalDay <- renderText({stableData() %>% slice_max(titer) %>% pull(day)})
  # output$viab <- renderText({stableData() %>% slice_max(titer) %>% pull(viability)})
  
  plotTheme <- reactive({switch(input$plotTheme,
                                'Black & White (ggplot2)' = theme_bw(),
                                'GraphPad Prism' = theme_prism(),
                                'Default (ggplot2)' = NULL)
                })
  
  stablePlot <- reactive({source("scripts/stablePlots.R", local = T)$value})
  
  output$stablePlot <- renderPlot({stablePlot()})
  
  output$downloadStablePlot <- downloadHandler(
    # Specify the file name
    filename = function() {
      paste("plot",  Sys.Date(), input$var1, sep = ".")
    },
    # Open the device and create the plot and close the device
    content = function(file) {
      ggsave(file, plot = stablePlot(), width = 10, device = input$var1, dpi = 600)
    }
  )
  
  
  # Cell line titer ends
  
  # Antibody conjugation begins
  
  fluor <- reactive({source("scripts/fluorList.R", local = T)$value})
  
  #abxxxxxx <- reactive(input$ab_volume/1000*input$ab_conc)
  
  #output$abAmount <- renderText(abxxxxxx())
  
  #output$percentRecovery <- renderText(input$antibodyRecovery/abxxxxxx()*100)
  
  output$final <- renderText((((input$ab_conc * input$ab_volume)/1000)/(input$ab_molwt * 1000) * (input$molar_excess * fluor() * 1000000) / input$fluor_conc)/1000)
  
  # Antibody conjugation ends
}

shinyApp(ui, server)