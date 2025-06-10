source("R/model.R")  # make_dance_model, genders_fixated, is_inverter

demo_single_trial <- function(n_agents = 100, 
                              inversion_prevalence = 0.7,
                              deviance_penalty = 0.1) {

  abm <- make_dance_model(n_agents, inversion_prevalence, deviance_penalty) 

  trial <- run_trial(abm, stop = genders_fixated)
  # trial <- run_trial(abm, stop = 20)
  obs <- trial$observations
  
  # Build observation and prevalence time series tables
  obs$Gender <- purrr::map_vec(
    obs$agent, \(a_chr) trial$model$get_agent(a_chr)$get_attribute("Gender")
  )

  # Use dplyr and is_inverter from R/model.R 
  obs <- 
    rowwise(obs) %>% 
    mutate(inverter = is_inverter(Gender, Behavior)) %>% 
    ungroup()

  # Calculate Inverter prevalence by gender
  summ_prevalence <- obs %>%
    group_by(Step, Gender) %>%
    summarise(`Inverter prevalence` = mean(inverter),  
              `Mean payoff` = mean(Fitness))
  
  return (summ_prevalence)
}
