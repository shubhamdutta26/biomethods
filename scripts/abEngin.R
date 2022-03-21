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
)