##
# Make figures for the gender role inversion example in Achieving Fairness chapter
# 
# Date: 10 June 2025
# Author: Matt Turner
#
library(ggplot2)

source("R/analysis.R")

demo_figure <- function(n_agents = 50, inversion_prevalence  = 0.675, 
                        deviance_penalty = 0.8, linewidth = 1, base_size = 14,
                        save_path = "Figures/demo-series.png",
                        width = 5.5, height = 3.25, dpi = 300) {

  # Demo figure to generate and include in index.qmd with select code blocks
  # demonstrating important components of the software implementation.
  summ_prevalence <- 
    demo_single_trial(
      n_agents, 
      inversion_prevalence,
      deviance_penalty
    ) 
  p <- 
    ggplot(summ_prevalence, aes(x=Step, y=`Inverter prevalence`, color=Gender, linetype=Gender)) +
    geom_line(linewidth = linewidth) + theme_classic(base_size)
  # %>%
  #   tidyr::pivot_longer(
  #     cols = c(`Inverter prevalence`, `Mean payoff`),
  #     names_to = "Measure",
  #     values_to = "Value" 
  #   )
  # 
  # p <- 
  #   ggplot(summ_prevalence, aes(x=Step, y=Value, color=Gender, linetype=Measure)) +
  #     geom_line(linewidth = linewidth) + theme_classic(base_size)

  ggsave(save_path, p, width = width, height = height, dpi = dpi)
  
  return (summ_prevalence)
}

sp <- demo_figure()