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
  ggplot2::ggplot(df, ggplot2::aes_string(x = predictor, y = response, col = type)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::ggtitle(paste("Boxplot of", response_label)) +
    ggplot2::xlab(predictor) +
    ggplot2::ylab(response_label) +
    ggplot2::theme_bw() +
    # Use colors from the palette
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = c(palette[2], palette[7], palette[6], palette[1])),
          axis.ticks = ggplot2::element_line(color = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c(palette[5], palette[3])) +
    # Make the first letter of each label capitalized
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_to_title(x))
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
  # Make the boxplot using the custom function
  myboxplot <-  NiceBoxplot(df, response, predictor, type, palette)
  # Then make a comparison for colorblindness...
  colorchecked <- colorblindr::cvd_grid(myboxplot)
  # Then compare both together
  cowplot::plot_grid(myboxplot, colorchecked)
}

#' @title NormCheck: A function to check assumptions of normality.
#' @description This function displays a QQ Plot of the residuals of the model and performs a Shapiro-Wilk normality test.
#' @param df dataframe
#' @param response response variable (e.g., PC_1_value)
#' @param predictor1 predictor variable (e.g., Treatment)
#' @param predictor2 predictor variable (e.g., State)
#' @param predictor3 predictor variable (e.g., Response)
#' @param type subject type (e.g., Sex)
#' @keywords ANOVA, model assumptions, normality
#' @export
#' @examples
#' NormCheck(df = ConvictData, response = 'PC_1_value', predictor1 = 'Treatment', predictor2 = 'State', predictor3 = 'Response', type = 'Sex')

NormCheck <- function(df, response, predictor1, predictor2, predictor3, type){
  # Set up the model
  formula_string <- paste(response, "~", predictor1, "+", predictor2, "+", predictor3, "+", type, "+", predictor1, ":", type, "+", predictor2, ":", type, "+", predictor3, ":", type)
  m1 <- aov(as.formula(formula_string), data = df)
  # Create QQ plots
  plot1 <- qqnorm(resid(m1))
  plot2 <- qqline(resid(m1))
  # Follow-up with the Shapiro-Wilk test
  s <- shapiro.test(resid(m1))
  results <- list(plot1, plot2, s)
  print(results)
}

#' @title NormCheckLog: A function to check (log-transformed) assumptions of normality.
#' @description This function displays a QQ Plot of the residuals of the model and performs a Shapiro-Wilk normality test. This time, the response variable is log-transformed.
#' @param df dataframe
#' @param response response variable (e.g., PC_1_value)
#' @param predictor1 predictor variable (e.g., Treatment)
#' @param predictor2 predictor variable (e.g., State)
#' @param predictor3 predictor variable (e.g., Response)
#' @param type subject type (e.g., Sex)
#' @keywords ANOVA, model assumptions, normality, log-transformed
#' @export
#' @examples
#' NormCheckLog(df = ConvictData, response = 'PC_1_value', predictor1 = 'Treatment', predictor2 = 'State', predictor3 = 'Response', type = 'Sex')

NormCheckLog <- function(df, response, predictor1, predictor2, predictor3, type){
  df[[response]] <- log(df[[response]])
  # Set up the model
  formula_string <- paste(response, "~", predictor1, "+", predictor2, "+", predictor3, "+", type, "+", predictor1, ":", type, "+", predictor2, ":", type, "+", predictor3, ":", type)
  m1 <- aov(as.formula(formula_string), data = df)
  # Create QQ plots
  plot1 <- qqnorm(resid(m1))
  plot2 <- qqline(resid(m1))
  # Follow-up with the Shapiro-Wilk test
  s <- shapiro.test(resid(m1))
  results <- list(plot1, plot2, s)
  print(results)
}

#' @title PC2ModelSelect: A function to select the best model.
#' @description This function assists with model selection, finding which predictor variables best explain the PC2 values.
#' @param df dataframe
#' @param response response variable (e.g., PC_2_value)
#' @param predictor1 predictor variable (e.g., Treatment)
#' @param predictor2 predictor variable (e.g., State)
#' @param predictor3 predictor variable (e.g., Response)
#' @param type subject type (e.g., Sex)
#' @keywords ANOVA, model assumptions, normality
#' @export
#' @examples
#' PC2ModelSelect(df = ConvictData, response = 'PC_2_value', predictor1 = 'Treatment', predictor2 = 'State', predictor3 = 'Response', type = 'Sex')

PC2ModelSelect <- function(df, response, predictor1, predictor2, predictor3, type){
  # Extract the actual variable from the dataframe
  response_var <- df[[response]]

  # Apply log transformation to the response variable
  log_response <- log(response_var)

  # Set up the full model
  m2_1 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor2, "+", predictor3, "+", type, "+", predictor1, ":", type, "+", predictor2, ":", type, "+", predictor3, ":", type)), data = df)
  # Set up other options...
  m0 <- aov(as.formula(paste("log_response", "~", 1)), data = df)
  m1 <- aov(as.formula(paste("log_response", "~", predictor1)), data = df)
  m2 <- aov(as.formula(paste("log_response", "~", predictor2)), data = df)
  m3 <- aov(as.formula(paste("log_response", "~", predictor3)), data = df)
  m4 <- aov(as.formula(paste("log_response", "~", type)), data = df)
  m5 <- aov(as.formula(paste("log_response", "~", predictor1, "+", type)), data = df)
  m6 <- aov(as.formula(paste("log_response", "~", predictor2, "+", type)), data = df)
  m7 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor2, "+", type)), data = df)
  m8 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor3, "+", type)), data = df)
  m9 <- aov(as.formula(paste("log_response", "~", predictor2, "+", predictor3, "+", type)), data = df)
  m10 <- aov(as.formula(paste("log_response", "~", predictor2, "+", type, "+", predictor2, ":", type)), data = df)
  m11 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor2, "+", predictor3, "+", type)), data = df)
  m12 <- aov(as.formula(paste("log_response", "~", predictor2, "+", predictor3, "+", type, "+", predictor2, ":", type)), data = df)
  m13 <- aov(as.formula(paste("log_response", "~", predictor2, "+", predictor3, "+", type, "+", predictor3, ":", type)), data = df)
  m14 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor3, "+", type, "+", predictor3, ":", type)), data = df)
  m15 <- aov(as.formula(paste("log_response", "~", predictor1, "+", predictor2, "+", type, "+", predictor2, ":", type)), data = df)

  (PC2s <- MASS::stepAIC(m2_1, scope = . ~ ., direction = "both"))
  PC2summary <- summary(PC2s)
  PC2mods <- AICcmodavg::aictab(list(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m2_1), c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m2_1"))
  resultsPC2 <- list(PC2summary, PC2mods)
  print(resultsPC2)
}

