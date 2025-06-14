##
# Make figures for the gender role inversion example in Achieving Fairness chapter
# 
# Date: 10 June 2025
# Author: Matt Turner
#
library(ggplot2)

source("R/analysis.R")

replicator_deviance_triptych <- function(deltas = c(0.0, 0.5, 0.9),
                                         save_dir = "Figures",
                                         width = 5.5, height = 3.25, dpi = 300) {
  
  for (delta in deltas) {
  
    p <- replicator_dynamics_plot(
      delta = delta, scale_low = 0.0255, scale_high = 0.0425, resolution = 15, 
      arrow_alpha = 1, steps = 50000, base_size = 16
    )
    
    ggsave(
      file.path(save_dir, paste0("replicator_delta=", delta, ".pdf")), dpi = 300
    )
  }
}

demo_figure <- function(n_agents = 50, graph = NULL, 
                        inversion_prevalence  = 0.625, 
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
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Inverter prevalence`, `Mean payoff`),
      names_to = "Measure",
      values_to = "Value"
    )

  p <-
    ggplot(summ_prevalence, aes(x=Step, y=Value, 
                                color=Gender, linetype=Measure)) +
      geom_line(linewidth = linewidth) + theme_classic(base_size)
 
  ggsave(save_path, p, width = width, height = height, dpi = dpi)
  
  return (summ_prevalence)
}


# sp <- demo_figure()