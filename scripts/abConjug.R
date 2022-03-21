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
)