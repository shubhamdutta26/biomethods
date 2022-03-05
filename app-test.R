library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(paletteer)

source("functions/stablePlot.R")

# # Theme for stable cell plots
# stablePlotTheme <- function(){
#   #theme_prism() +
#   theme(plot.title=element_text(size=25),
#         plot.subtitle = element_text(size = 18),
#         axis.text=element_text(size=12, color = "#434343"),
#         axis.title=element_text(size=18),
#         legend.text=element_text(size=10),
#         legend.title=element_text(size=15),
#         legend.box.background = element_rect(color = "black"),legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"))
# }


header <- dashboardHeader(title = "Biomethods")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Fed Batch Analysis",
             icon = icon("chart-bar"),
             tabName = "fedBatch"),
    menuItem("Antibody Engineering",
             icon = icon("flask"),
             tabName = "abEngin"),
    menuItem("Antibody Conjugation",
             icon = icon("atom"),
             tabName = "abConjug"),
    menuItem("Quantitative Real-Time PCR",
             icon = icon("dna"),
             tabName = "qpcr")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "abEngin",
            fluidRow(
              column(width = 3,
                     box(title = "Set Parameters",
                         width = NULL,
                         status = "primary",
                         selectInput(# select Input organisms begin
                           inputId = "organism",
                           label = "Organism",
                           c('Aotus nancymaae (Owl monkey)',
                             'Callithrix jacchus (Marmosets)',
                             'Saimiri spp. (Squirrel monkey)'
                           ),
                         ),# select Input organisms end
                         selectInput(# select Input HC class begin
                           inputId = "ab_hc_class",
                           label = "Heavy chain class",
                           c('Isotype G'
                           ),
                         ),# select Input HC class end
                         selectInput(# select Input LC class begin
                           inputId = "ab_lc_class",
                           label = "Light chain class",
                           c('Kappa'
                           ),
                         ),# select Input LC class end
                         selectInput(# select Input signal peptide begin
                           inputId = "signal_peptide",
                           label = "Signal peptide (upstream)",
                           c('<none>',
                             'Osteo (Human)'
                           ),
                         ),# select Input signal peptide end
                         selectInput(# select Input 5 RE site begin
                           inputId = "restr_enz_5",
                           label = "Restriction site (5')",
                           c('<none>',
                             'EcoRI',
                             'HindIII'
                           ),
                         ),# select Input 5 RE site end
                         selectInput(# select Input 3 RE site begin
                           inputId = "restr_enz_3",
                           label = "Restriction site (3')",
                           c('<none>',
                             'EcoRI',
                             'HindIII'
                           ),
                         ),# select Input 3 RE site end
                         checkboxInput(
                           inputId = "kozak",
                           label = "Include Kozak sequence",
                           value = TRUE
                         ),
                     ),
              ),
              column(width = 9,
                     box(title = "Input Variable Heavy (VH) nucleotide sequence",
                         width = NULL,
                         status = "primary",
                         height = 200,
                         textAreaInput(
                           inputId = "vh_input",
                           label = NULL,
                           placeholder = "nucleotide sequence only",
                           height = "100px",
                         ),
                     ),
                     box(title = "Input Variable Light (VL) nucleotide sequence",
                         width = NULL,
                         status = "primary",
                         height = 200,
                         textAreaInput(
                           inputId = "vl_input",
                           label = NULL,
                           placeholder = "nucleotide sequence only",
                           height = "100px",
                         ),
                     ),
                     box(title = "Full length Heavy chain sequence",
                         width = NULL,
                         status = "success",
                         textOutput(outputId = "fullHeavy"), #outputs full heavy chain with RE sites
                         tags$style("#fullHeavy{word-break: break-all; color: #339ACC; font-size: 15px}"),
                     ),
                     box(title = "Full length Light chain sequence",
                         width = NULL,
                         status = "success",
                         textOutput(outputId = "fullLight"), #outputs full heavy chain with RE sites
                         tags$style("#fullLight{word-break: break-all; color: #339ACC; font-size: 15px}"),
                     ),
              ),
              
            )
    ),
    
    tabItem(tabName = "abConjug",
            fluidRow(
              column(width = 3,
                     box(title = "Conjugation parameters",
                         width = NULL,
                         status = "primary",
                         selectInput(
                           inputId = "fluor",
                           label = "fluor",
                           c('Alexa 350',
                             'Alexa 405',
                             'Alexa 488',
                             'Alexa 532',
                             'Alexa 555',
                             'Alexa 594',
                             'Alexa 647',
                             'Alexa 680',
                             'Alexa 700',
                             'Alexa 750')
                         ),#end of selectInput = fluor list
                         numericInput(
                           inputId = "fluor_conc",
                           label = "concentration of fluor solution (mg/ml)",
                           min = 0,
                           max = 1000,
                           value = 10
                         ),#end of numericInput = fluor conc
                         numericInput(
                           inputId = "ab_conc",
                           label = "concentration of antibody (mg/ml)",
                           min = 0,
                           max = 1000,
                           value = 10
                         ),#numericInput = ab conc
                         numericInput(
                           inputId = "ab_volume",
                           label = "volume of antibody (ul)",
                           min = 0,
                           max = 10000,
                           value = 1000
                         ),#numericInput = ab volume
                         numericInput(
                           inputId = "ab_molwt",
                           label = "molecular weight of antibody (kDa)",
                           min = 0,
                           max = 1000,
                           value = 150
                         ),#numericInput = ab volume
                         numericInput(
                           inputId = "molar_excess",
                           label = "molar excess of fluor:antibody",
                           min = 0,
                           max = 1000,
                           value = 10
                         ),#numericInput = molar ratio
                     ),
                     box(title = "Volume of Fluor solution (ul)",
                         width = NULL,
                         status = "success",
                         textOutput("final")),
                     tags$style("#final{color: #339ACC; font-size: 30px; font-style: bold}"),# Output panel with style
                     
              ),
              column(width = 9,
                     box(title = "Conjugation Protocol",
                         width = NULL,
                         status = "primary",
                         includeHTML("conjugation.html")
                     )
              )
            ),
    ),
    
    tabItem(tabName = "fedBatch",
            fluidRow(
              column(width = 3,
                     box(title = "Upload your data (xls file)",
                         width = NULL,
                         status = "primary",
                         fileInput(inputId = "stableData",
                                   label = NULL,
                                   accept = c(".xls")),
                         tags$a(href = "stableTestData.xls", "Download template", download=NA, target="_blank"),
                     ),
                     box(title = "Plotting parameters (y-axis)",
                         width = NULL,
                         status = "info",
                         radioButtons(
                           inputId = "dataplot",
                           label = NULL,
                           c('Titer (ug/ml)',
                             'Viable cell density (million/ml)',
                             'Viability (%)',
                             'pH',
                             'pO2 (mmHg)',
                             'pCO2 (mmHg)',
                             'Glutamine (mmol/L)',
                             'Glutamate (mmol/L)',
                             'Glucose (g/L)',
                             'Lactate (g/L)',
                             'Ammonium (mmol/L)',
                             'Sodium (mmol/L)',
                             'Potassium (mmol/L)'),
                         ),
                     ),
                     box(title = "Select plot theme",
                         width = NULL,
                         status = "info",
                         radioButtons(inputId = "plotTheme",
                                     label = NULL,
                                     c('Black & White (ggplot2)',
                                       'GraphPad Prism',
                                       'Default (ggplot2)')
                                     ),
                     ),
                     downloadButton(outputId = "downloadStablePlot", label = "Download Plot"),
                     radioButtons(inputId = "var1", label = NULL, choices = list("png", "pdf", "svg", "tiff"), inline = TRUE)
                     # box(title = "Winning Clone",
                     #     width = NULL,
                     #     status = "success",
                     #     h4(textOutput("winClone")),
                     #     tags$style("#winClone{color: #339ACC; font-size: 30px; font-style: bold}"),
                     # ),
                     # box(title = "Max Titer (ug/ml)",
                     #     width = NULL,
                     #     status = "success",
                     #     h4(textOutput("maxTiter")),
                     #     tags$style("#maxTiter{color: #339ACC; font-size: 30px; font-style: bold}"),
                     # ),
                     # box(title = "Viability (%)",
                     #     width = NULL,
                     #     status = "success",
                     #     h4(textOutput("viab")),
                     #     tags$style("#viab{color: #339ACC; font-size: 30px; font-style: bold}"),
                     # ),
                     # box(title = "Final Day",
                     #     width = NULL,
                     #     status = "success",
                     #     h4(textOutput("finalDay")),
                     #     tags$style("#finalDay{color: #339ACC; font-size: 30px; font-style: bold}"),
                     # ),
                     
                     
              ),
              column(width = 9,
                     box(title = "Add Title to the Plot",
                         width = NULL,
                         status = "info",
                         textInput(inputId = "title2",
                                   label = NULL,
                                   placeholder = "Plot title"),
                     ),
                     box(title = "Add Subtitle to the Plot",
                         width = NULL,
                         status = "info",
                         textInput(inputId = "subtitle2",
                                   label = NULL,
                                   placeholder = "Plot subtitle"),
                     ),
                     box(title = "Fed Batch Plots",
                         width = NULL,
                         height = 500,
                         status = "primary",
                         plotOutput("stablePlot")
                     ),
                     
                     # downloadButton(outputId = "downloadStablePlot", label = "Download Plot"),
                     # radioButtons(inputId = "var1", label = NULL, choices = list("png", "pdf", "svg", "tiff"), inline = TRUE) 
                     
              )
            ),
            
    ),
    
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
  
  data_hc <- (reactive({
    if(input$organism == 'Aotus nancymaae (Owl monkey)'){
      "gcctccaccaaggcaccggacgtcttccccctgacgccctgcggtacctctgggaccacgacggccctgggctgcctggtcaaggactacttcccccagccggtgaccgtgtcctggaactcgggcgccctgaccagcggcgtgcacaccttcccggccgtcctgcagtcctcggggctctattccctcagcagcatggtgaccaccagcagcgcgggcagccagacctacacctgcaacgtggtccacccacccaccggcttcaagaaggacctgacagtgcgacctcacaacccatcaccaccctgtcccccatgcccagcacctgaattgctggagggaccatcagtcttcctcttccccccgaaacccaaggacgtcctcaggatcaccgggacccctgaggtcacgtgcgtggtggtggacgtgagccaggacgaccccgaggtccagttcaactggtatgtggacaacgtggaggtgcacaatgccaagacgagcccgagggagcagcagttcaacagcacgtaccgcgtggtcagcgccctccccgtcctgcaccaggactggctgaagggcaaggagtacaaatgcaaggtcaccaacaaagccctcccggcccccatccagaagaccatccacaaggccaaagggacgccccgagagccgcaggtgtacatcctgcccccgtcgcgggaggagatgagcaggagccaggtcagcctgacctgcctggtgcacagcttctaccccagcgacatcgccgtggagtgggagagcagcgggcagcccgagagcagctacaagaacacgccaccagttctggacgccgacggctccttcttcctctacagcaagctcaccgtggacaagagccgctggcagcagggcaaggtcttctcgtgctccgtgatgcacgaggctctgcacaaccactacacgcagaagagcctctccgcatctccgggtaaatga"
    }else if(input$organism == 'Callithrix jacchus (Marmosets)'){
      "gcctccaccaagaatccagatgtcttccccctgacaccctgcggcacctctgggaccacaacggccctgggctgcctggtcaaggactacttccccgagccagtgaccgtgtcctggaactcaggcgccctgaccagtggcgtgcacaccttccctgctgtcctgcggtcctcgggactttactccctcagcagcatggtgaccaccagcagcgcgggcagccagacctacacctgcaacgtcgtccacacgcccaccggcttcaagaaggacgtgccagtgccattgaaacctaatggctcatcaccctgtcccccatgcccagcacctgagttcctggggggaccatcagtcttcctcttccccccaaaacccaaggacgtcctcaggatcaccggaacccctgaggtcacttgcgtggtggtggatgtgagccaggacgaccctgaggtccagttcaattggtacgtggacggcgtggaggtgcgcactgccaagacgagtccgagggaggagcagcacaacagcacgttccgcgtggtcagcgccctccctgtcctgcaccaggactggctgaagggcaaggagtacagatgcaaggtcaccaacaaagccctcccggcccccgtccagaaaaccatccacaaagccaaagggacgccccgagagccgcaggtgtacaccctgcccccagcgcgggaggagatgagcaggagccgggtcagcctgacctgcctgatccaaggcttctaccccagcgacatcgccgtggagtgggagagcaacgggcagccggagaacaactacaagaacacgcagcccgtgctggactccgacggctccttcttcctctacagcaagctcaccgtggacaagcagaggtggcagcaggggagcgtcttctcgtgctccgtgatgcacgaggctctgcacaaccactacacgcagaagagcctctccgcgtctccgggtaaatga"
    }else if(input$organism == 'Saimiri spp. (Squirrel monkey)'){
      "gcctccaacaagaaaccagacgtcttccccctgacgccctgctgcacctctgggaccacaatggccctgggctgcctggtcaaggactacttcccccagccggtgaccgtgtcctggaactcgggcgccctgaccagcggcgtgcacaccttcccggctgtcctgcagtcctcggggctctactccctcagcagcatggtgaccaccagcagcgcgggcagccagacctacacctgcaatgttgtccacacgcccaccagcttcaagaaggacctgatagtgaaacctcctccttgtgacacatcctgtccgccatgcccagcacctgatttcctggggggaccgtcagtcttcctcttccccccgaaacccaaggacatcctcaggatctccgggagccctgaggtcacgtgcgtggtggtggacgtgagccaggacgaccccgaggtccagttcaactggtacgtggacaacgtggaggtgcgcactgccaagacgagtccaagggaggagcagttcaacagcacgttccgcgtggtcagcgccctccccgtcgtgcaccaggactggctgaagggcaaggagtacaaatgcaaggtcaccaacaaagccctcccggcccccgtccagaaaaccatccgcaaagccaaagggacgccccgagagccgcaggtgtacaccctgcccccatcgcgggaggagatgagcaggagccaggtcagcctgacctgcctgatccagggcttctaccccagcgacatcgccgtggagtgggagagcaacgggcagccagagaacaactacaagaacacgcagcctgtgctggactcggacggctccttcttcctctacagcaagctcaccgtggacaagcacaggtggcagcaggggaccgtcttctcgtgctccgtgatgcacgaggctctgcacaaccactacacgcagaagagcctctctgcgtctccgggtaaatga"
    }
    
    
  })
  )
  
  
  data_lc <- (reactive({
    if(input$organism == 'Aotus nancymaae (Owl monkey)'){
      "cgagctgtggctgcgccatctgtcttcatcttccagccatctgaggaacaggtgaaatctggaactgcctctgttgtgtgcttgctgaatgacttctatcccagagatgtcagtgtcaagtggaaggtggatgacgtcgtccagtcgagtaacgtccaggacagtatcacagagcaggacagcaaggacaacacctacagcctcagcagcaccctgacgctgagcagcacagaataccagaggcacaaagtctacgcctgtgaggtcacccatcaaggcttgaactcgcccctcacgaagagcttccgtaaaggagagtgttag"
    }else if(input$organism == 'Callithrix jacchus (Marmosets)'){
      "cgagctgtggctgcgccgtctgtcttcatcttcccgccatctgaggatcaggtgaaatctggaactgccactgttgtgtgcctactgaatggcttctatcccagagatgccaaggtaaagtggaaggtggatgatgtcgtccagtcggataacagtatccaggacagtatcacagagcaggacagcaaggacaacacctacagcctcagcagcaccttgacgctgagcagcacggaataccagaggcacaacgtctacgcctgtgaggtcacccatcagagcttgacctcgcccctcacgaagagcttccgtaaaggagagtgttag"
    }else if(input$organism == 'Saimiri spp. (Squirrel monkey)'){
      "cgagctgtggctgcgccatccgtcttcatcttccagccatctgaggagcaggtgaaatctggaactgcctctcttgtgtgcatgctgaatggcttctatcccaaagatgtcaatgtgaagtggaaggtggatgacgtcgtccagccgaaggataacattcttgaaagtatcacagagcaggacagcaaggacaacacctacagcctcagcagcaccctgacgctgagcagcacagaataccagaggcacaacgtctacgcctgtgaggtcacccatcaaggcttgagatcgcccctcacgaagagcttccgtaaaggagagtgttag"
    }
    
  })
  )
  
  
  signal_peptide <- (reactive({
    switch(input$signal_peptide,
           '----' = "",
           'Osteo (Human)' = "ATGAGGGCTTGGATCTTCTTTCTGCTCTGCCTGGCCGGGCGCGCCTTGGCC"
           #'PvuI' = "XXXXXX",
    )
  })
  )
  
  restr_enz_5 <- (reactive({
    switch(input$restr_enz_5,
           '----' = "",
           'EcoRI' = "GAATTC",
           'HindIII' = "AAGCTT"
           #'PvuI' = "XXXXXX",
    )
  })
  )
  
  restr_enz_3 <- (reactive({
    switch(input$restr_enz_3,
           '----' = "",
           'EcoRI' = "GAATTC",
           'HindIII' = "AAGCTT"
           #'PvuI' = "XXXXXX",
    )
  })
  )
  
  kozak <- (reactive({
    if(input$kozak == TRUE){
      "gccgccacc"
    }else{
      ""
    }
  })
  
  )
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
  
  plotTheme <- reactive({
    switch(input$plotTheme,
           'Black & White (ggplot2)' = theme_bw(),
           'GraphPad Prism' = theme_prism(),
           'Default (ggplot2)' = NULL
           )
  })
  
  stablePlot <- reactive({
    switch(input$dataplot,
           'Titer (ug/ml)' = req(stablePlot(stableData(), titer), cancelOutput = FALSE) + plotTheme(),
    
           'Viable cell density (million/ml)' = (ggplot(stableData(), aes(x = day, y = vcd/1e6)) +
                                                   geom_line(data = stableData()[!is.na(stableData()$vcd),], aes(color = clone)) +
                                                   geom_point() +
                                                   scale_color_paletteer_d("ggsci::nrc_npg") +
                                                   labs(title = input$title2,
                                                        subtitle = input$subtitle2,
                                                        x = "Days",
                                                        y = "Viable cell density (million/ml)",
                                                        color = "Clone",
                                                        caption = Sys.Date()) +
                                                   ylim(0, NA) +
                                                   scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                                   stablePlotTheme() + plotTheme()),
           'Viability (%)' = (ggplot(stableData(), aes(x = day, y = viability)) +
                                geom_line(data = stableData()[!is.na(stableData()$viability),], aes(color = clone)) +
                                geom_point() +
                                scale_color_paletteer_d("ggsci::nrc_npg") +
                                labs(title = input$title2,
                                     subtitle = input$subtitle2,
                                     x = "Days",
                                     y = "Viability (%)",
                                     color = "Clone",
                                     caption = Sys.Date()) +
                                ylim(0,100) +
                                scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                stablePlotTheme() + plotTheme()),
           'pH' = (ggplot(stableData(), aes(x = day, y = ph)) +
                     geom_line(data = stableData()[!is.na(stableData()$ph),], aes(color = clone)) +
                     geom_point() +
                     scale_color_paletteer_d("ggsci::nrc_npg") +
                     labs(title = input$title2,
                          subtitle = input$subtitle2,
                          x = "Days",
                          y = "pH",
                          color = "Clone",
                          caption = Sys.Date()) +
                     ylim(0,14) +
                     scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                     stablePlotTheme() + plotTheme()),
           'pO2 (mmHg)' = (ggplot(stableData(), aes(x = day, y = po2)) +
                             geom_line(data = stableData()[!is.na(stableData()$po2),], aes(color = clone)) +
                             geom_point() +
                             scale_color_paletteer_d("ggsci::nrc_npg") +
                             labs(title = input$title2,
                                  subtitle = input$subtitle2,
                                  x = "Days",
                                  y = "Oxygen (mmHg)",
                                  color = "Clone",
                                  caption = Sys.Date()) +
                             ylim(0,NA) +
                             scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                             stablePlotTheme() + plotTheme()),
           'pCO2 (mmHg)' = (ggplot(stableData(), aes(x = day, y = pco2)) +
                              geom_line(data = stableData()[!is.na(stableData()$pco2),], aes(color = clone)) +
                              geom_point() +
                              scale_color_paletteer_d("ggsci::nrc_npg") +
                              labs(title = input$title2,
                                   subtitle = input$subtitle2,
                                   x = "Days",
                                   y = "Carbon dioxide (mmHg)",
                                   color = "Clone",
                                   caption = Sys.Date()) +
                              ylim(0,NA) +
                              scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                              stablePlotTheme() + plotTheme()),
           'Glutamine (mmol/L)' = (ggplot(stableData(), aes(x = day, y = glutamine)) +
                                     geom_line(data = stableData()[!is.na(stableData()$glutamine),], aes(color = clone)) +
                                     scale_color_paletteer_d("ggsci::nrc_npg") +
                                     geom_point() +
                                     labs(title = input$title2,
                                          subtitle = input$subtitle2,
                                          x = "Days",
                                          y = "Glutamine (mmol/L)",
                                          color = "Clone",
                                          caption = Sys.Date()) +
                                     ylim(0,NA) +
                                     scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                     stablePlotTheme() + plotTheme()),
           'Glutamate (mmol/L)' = (ggplot(stableData(), aes(x = day, y = glutamate)) +
                                     geom_line(data = stableData()[!is.na(stableData()$glutamate),], aes(color = clone)) +
                                     geom_point() +
                                     scale_color_paletteer_d("ggsci::nrc_npg") +
                                     labs(title = input$title2,
                                          subtitle = input$subtitle2,
                                          x = "Days",
                                          y = "Glutamate (mmol/L)",
                                          color = "Clone",
                                          caption = Sys.Date()) +
                                     ylim(0,NA) +
                                     scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                     stablePlotTheme() + plotTheme()),
           'Glucose (g/L)' = (ggplot(stableData(), aes(x = day, y = glucose)) +
                                geom_line(data = stableData()[!is.na(stableData()$glucose),], aes(color = clone)) +
                                geom_point() +
                                scale_color_paletteer_d("ggsci::nrc_npg") +
                                labs(title = input$title2,
                                     subtitle = input$subtitle2,
                                     x = "Days",
                                     y = "Glucose (g/L)",
                                     color = "Clone",
                                     caption = Sys.Date()) +
                                ylim(0,NA) +
                                scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                stablePlotTheme() + plotTheme()),
           'Lactate (g/L)' = (ggplot(stableData(), aes(x = day, y = lactate)) +
                                geom_line(data = stableData()[!is.na(stableData()$lactate),], aes(color = clone)) +
                                geom_point() +
                                scale_color_paletteer_d("ggsci::nrc_npg") +
                                labs(title = input$title2,
                                     subtitle = input$subtitle2,
                                     x = "Days",
                                     y = "Lactate (g/L)",
                                     color = "Clone",
                                     caption = Sys.Date()) +
                                ylim(0,NA) +
                                scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                stablePlotTheme() + plotTheme()),
           'Ammonium (mmol/L)' = (ggplot(stableData(), aes(x = day, y = ammonium)) +
                                    geom_line(data = stableData()[!is.na(stableData()$ammonium),], aes(color = clone)) +
                                    geom_point() +
                                    scale_color_paletteer_d("ggsci::nrc_npg") +
                                    labs(title = input$title2,
                                         subtitle = input$subtitle2,
                                         x = "Days",
                                         y = "Ammonium (mmol/L)",
                                         color = "Clone",
                                         caption = Sys.Date()) +
                                    ylim(0,NA) +
                                    scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                    stablePlotTheme() + plotTheme()),
           'Sodium (mmol/L)' = (ggplot(stableData(), aes(x = day, y = sodium)) +
                                  geom_line(data = stableData()[!is.na(stableData()$sodium),], aes(color = clone)) +
                                  geom_point() +
                                  scale_color_paletteer_d("ggsci::nrc_npg") +
                                  labs(title = input$title2,
                                       subtitle = input$subtitle2,
                                       x = "Days",
                                       y = "Sodium (mmol/L)",
                                       color = "Clone",
                                       caption = Sys.Date()) +
                                  ylim(0,NA) +
                                  scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                  stablePlotTheme() + plotTheme()),
           'Potassium (mmol/L)' = (ggplot(stableData(), aes(x = day, y = potassium)) +
                                     geom_line(data = stableData()[!is.na(stableData()$potassium),], aes(color = clone)) +
                                     geom_point() +
                                     scale_color_paletteer_d("ggsci::nrc_npg") +
                                     labs(title = input$title2,
                                          subtitle = input$subtitle2,
                                          x = "Days",
                                          y = "Potassium (mmol/L)",
                                          color = "Clone",
                                          caption = Sys.Date()) +
                                     ylim(0,NA) +
                                     scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                                     stablePlotTheme() + plotTheme())
    )
  })
  
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
  
  fluor <- (reactive({
    switch(input$fluor,
           'Alexa 350' = 410.35,
           'Alexa 405' = 1028.3,
           'Alexa 488' = 643.4,
           'Alexa 532' = 723.8,
           'Alexa 555' = 1250,
           'Alexa 594' = 819.8,
           'Alexa 647' = 1250,
           'Alexa 680' = 1150,
           'Alexa 700' = 1400,
           'Alexa 750' = 1300,
           
    )
  }))
  
  output$final <- renderText((((input$ab_conc * input$ab_volume)/1000)/(input$ab_molwt * 1000) * (input$molar_excess * fluor() * 1000000) / input$fluor_conc)/1000)
  
  # Antibody conjugation ends
}

shinyApp(ui, server)