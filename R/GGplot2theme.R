#' Thema om tijdreeksen te plotten
#'
#' @param base_size Grootte van het lettertype op de plot. Default: 10
#' @param base_family Lettertype van de plot. Default: Arial
#' @return Output opmaak van de plot
#' @export
theme1=function(base_size=10,
                base_family="Arial")
{
  theme=theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family,margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                        debug = FALSE,face = "plain", colour = "black", size = base_size, hjust = 0.5,
                        vjust = 0.5, angle = 0, lineheight = 0.9),

    axis.text = element_text(size = rel(0.8), colour = "black"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.text.x = element_text(vjust = 1,margin = margin(t = 7, r = 5, b = 5, l = 5, unit = "pt")),
    axis.text.y = element_text(hjust = 1,margin = margin(t = 5, r = 7, b = 5, l = 5, unit = "pt")),
    axis.ticks = element_line(colour = "black",size = 0.5),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.1, "cm"),

    legend.background = element_rect(colour = NA),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.5),
    panel.grid.major = element_line(colour = "grey80",size = 0.5),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2)),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE)
}

#' Thema om tijdreeksen te plotten
#'
#' @param base_size Grootte van het lettertype op de plot. Default: 10
#' @param base_family Lettertype van de plot. Default: Arial
#' @return Output opmaak van de plot
#' @export
maptheme1=function(base_size=10,
                   base_family="Arial")
{
  theme=theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family,margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"),
                        debug = FALSE,face = "plain", colour = "black", size = base_size, hjust = 0.5,
                        vjust = 0.5, angle = 0, lineheight = 0.9),

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black",fill = NA,size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,

    axis.text = element_blank(),
    strip.text = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length =unit(0, "cm"),

    legend.background = element_rect(colour = NA),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.key.size = unit(0.6, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2)),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE)
}




