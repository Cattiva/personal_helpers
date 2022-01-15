
# LinkedIn ggplot2 theme --------------------------------------------------

library(ggplot2)
library(magrittr)

extrafont::font_import()
extrafont::loadfonts()
extrafont::fonttable()

main_color <- "#248A8A"
main_color_light <- "#61B7B7"
secondary_color <- "374B9D"

palette_main <- c("#248A8A", "374B9D", "E6B33C", "E6893C")
palete_eight <- c("0F7B7B", 
                  "20358B",
                  "CC9719",
                  "CC6A19",
                  "61B7B7",
                  "7484C6",
                  "FFDB88",
                  "FFBE88")
palette_light <- c("61B7B7",
                   "7484C6",
                   "FFDB88",
                   "FFBE88")

theme_linkedin_light <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 170,
  base_rect_size = base_size / 170
){
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace% ggplot2::theme(
  plot.title = ggplot2::element_text(
    colour = main_color,
    face = "bold",
    size = base_size + 24,
    hjust = 0,
    lineheight = 0.9
  ),
  plot.subtitle = ggplot2::element_text(
    colour = main_color_light,
    face = "bold",
    size = base_size + 9,
    hjust = 0,
    lineheight = 0.9
  ),
  axis.title = ggplot2::element_text(
    colour = main_color,
    face = "bold",
    size = base_size + 3,
    #hjust = 0,
    lineheight = 0.9
  ),
  legend.text = ggplot2::element_text(
    colour = main_color,
    face = "bold",
    size = base_size - 1,
    #hjust = 0,
    lineheight = 0.9
  ),
  plot.background = ggplot2::element_blank(),
  panel.background = ggplot2::element_rect(fill = NA),
  panel.grid = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  axis.text = ggplot2::element_text(colour = main_color),
  #axis.line = ggplot2::element_line(colour = main_color),
  axis.line.x.top = ggplot2::element_blank(),
  complete = TRUE)
  
}
