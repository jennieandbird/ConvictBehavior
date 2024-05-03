#' @title NiceBoxplot: A function to create a nice boxplot.
#' @description This function creates a boxplot.
#' @param df dataframe
#' @param response response variable (e.g., PC_1_value)
#' @param predictor predictor variable (e.g., Treatment)
#' @param type separate boxplot by subject type (e.g., sex)
#' @param palette color palette
#' \if{html}{Palette_Default:
#' \out{<div style="text-align: center">}\figure{2024-05-01_Palette_Default.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}}
#' \if{latex}{Palette_Default:
#' \out{\begin{center}}\figure{2024-05-01_Palette_Default.png}\out{\end{center}}}
#'
#' \if{html}{Palette_Convict:
#' \out{<div style="text-align: center">}\figure{2024-05-01_Palette_Convict.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}}
#' \if{latex}{Palette_Convict:
#' \out{\begin{center}}\figure{2024-05-01_Palette_Convict.png}\out{\end{center}}}
#'
#' \if{html}{Palette_Swordtail:
#' \out{<div style="text-align: center">}\figure{2024-05-01_Palette_Swordtail.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}}
#' \if{latex}{Palette_Swordtail:
#' \out{\begin{center}}\figure{2024-05-01_Palette_Swordtail.png}\out{\end{center}}}
#'
#' \if{html}{Palette_Bombus:
#' \out{<div style="text-align: center">}\figure{2024-05-01_Palette_Bombus.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}}
#' \if{latex}{Palette_Bombus:
#' \out{\begin{center}}\figure{2024-05-01_Palette_Bombus.png}\out{\end{center}}}
#' @keywords boxplot
#' @export
#' @examples
#' NiceBoxplot(df = ConvictData, response = 'PC_1_value', predictor = 'Treatment', type = 'Sex', palette = Palette_Default)

NiceBoxplot <- function(df, response, predictor, type, palette) {
  # Replace underscores with spaces in the response variable name
  response_label <- gsub("_", " ", response)
  ggplot2::ggplot(df, aes_string(x = predictor, y = response, col = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::ggtitle(paste("Boxplot of", response_label)) +
    ggplot2::xlab(predictor) +
    ggplot2::ylab(response_label) +
    ggplot2::theme_bw() +
    # Use colors from the palette
    ggplot2::theme(axis.text.x = element_text(color = c(palette[2], palette[7], palette[6], palette[1])),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ggplot2::scale_color_manual(values = c(palette[5], palette[3])) +
    # Make the first letter of each label capitalized
    ggplot2::scale_x_discrete(labels = function(x) str_to_title(x))
}

#' @title ColorblindPlot: A function to check the NiceBoxplot for colorblind vision.
#' @description This function displays the NiceBoxplot in approximations of colorblind vision, as well as a monochrome version if the plot were printed in greyscale.
#' \if{html}{Example plot:
#' \out{<div style="text-align: center">}\figure{2024-05-01_ColorblindPlot.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}}
#' \if{latex}{Example plot:
#' \out{\begin{center}}\figure{2024-05-01_ColorblindPlot.png}\out{\end{center}}}
#'
#' @param df dataframe
#' @param response response variable (e.g., PC_1_value)
#' @param predictor predictor variable (e.g., Treatment)
#' @param type separate boxplot by subject type (e.g., sex)
#' @param palette color palette (e.g., Palette_Default, Palette_Convict, Palette_Swordtail, Palette_Bombus)
#' @keywords colorblind, accessibility
#' @export
#' @examples
#' ColorblindPlot(df = ConvictData, response = 'PC_1_value', predictor = 'Treatment', type = 'Sex', palette = Palette_Default)

ColorblindPlot <- function(df, response, predictor, type, palette){
  myboxplot <-  NiceBoxplot(df, response, predictor, type, palette)
  colorchecked <- colorblindr::cvd_grid(myboxplot)
  cowplot::plot_grid(myboxplot, colorchecked)
}