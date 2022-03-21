source("scripts/plotTheme.R", local = T)


switch(input$dataplot,
       'Titer (ug/ml)' = (ggplot(stableData(), aes(x = day, y = titer)) +
                            geom_line(data = stableData()[!is.na(stableData()$titer),], aes(color = clone)) +
                            geom_point() +
                            scale_color_paletteer_d("ggsci::nrc_npg") +
                            labs(title = input$title2,
                                 subtitle = input$subtitle2,
                                 x = "Days",
                                 y = "Titer (ug/ml)",
                                 color = "Clone",
                                 caption = Sys.Date()) +
                            ylim(0, NA) +
                            scale_x_discrete(limits = c(3,4,5,6,7,8,9,10,11,12,13,14)) +
                            stablePlotTheme() + plotTheme()),
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