# Theme for stable cell plots
stablePlotTheme <- function(){
  #theme_prism() +
  theme(plot.title=element_text(size=25),
        plot.subtitle = element_text(size = 18),
        axis.text=element_text(size=12, color = "#434343"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=10),
        legend.title=element_text(size=15),
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(0.4,0.4,0.4,0.4,"cm"))
}
