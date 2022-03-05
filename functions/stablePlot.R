stablePlotTheme <- function(session, input, data, param) {
  data %>% 
  (ggplot(aes(x = day, y = param)) +
     geom_line(data = data[!is.na(data$param),], aes(color = clone)) +
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
     theme(plot.title=element_text(size=25),
           plot.subtitle = element_text(size = 18),
           axis.text=element_text(size=12, color = "#434343"),
           axis.title=element_text(size=18),
           legend.text=element_text(size=10),
           legend.title=element_text(size=15),
           legend.box.background = element_rect(color = "black"),legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm")))
}