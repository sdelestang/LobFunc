
#' Change factor to number
#'
#' This Changes a factor to number
#' @param Ang the angle of X tick labels
#' @param leg should the legend be shown, and if yes, then where. Options include "none", "left", "right","bottom", "top" and probably others.
#' @return sets a theme for ggplots
#' @examples
#' theme_lob(90, "left")
#' @export
theme_lob <- function(Ang=0,leg='none')
{theme(panel.background = element_rect(fill = "white",colour = NA),
       panel.border = element_rect(fill = NA, colour = "grey20"),
       axis.text.x = element_text(vjust = 0.0, angle = Ang),legend.position = leg)}
