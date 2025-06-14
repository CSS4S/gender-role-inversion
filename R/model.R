##
# Models and components for Ch 6 Achieving Fairness
# Author: Matt Turner
# Date: 2025-06-03
#

library(dplyr)
library(ggplot2)
library(magrittr)
library(purrr)


# To install socmod: 
# devtools::install_github("css4s/socmod")
library(socmod)


#' Partner selection for partner dance game
#'
dance_partnering <- function(focal, model) {
  # Random selection of a potential dance partner
  return (sample(focal$get_attribute("partners"), 1)[[1]])
}


#' Partner dance game interaction that yields payoffs.
#'
dance_interaction <- function(focal, partner, model) {
  payoff <- model$get_parameter("payoff_matrix")[[
    focal$get_attribute("Gender")
  ]][
    focal$get_behavior(), partner$get_behavior()
  ] 
  
  focal$fitness_current <- payoff
  focal$set_next_fitness(payoff)
}


#' Social learning within gender for gender-based coordination models
#'
gender_coordination_social_learning <- function(model) {
  
  purrr::walk(
    model$agents,
    \(agent) {
      # Each agent has teachers attribute set at initialization, i.e., 
      # same-gender neighbors and self
      teachers <- agent$get_attribute("teachers")
      
      # Get fitnesses and do success-biased learning
      fitnesses <- purrr::map_vec(teachers, \(t) t$fitness_current)
      teacher <- sample(teachers, 1, prob = fitnesses)[[1]]
      agent$set_next_behavior(teacher$behavior_current)
    }
  )
  
  # Call helper that sets all agents' current behavior and fitness to be the
  # next behavior/fitness
  learning_model_step(model)
}



initialize_dancers <- function(model, inversion_prevalence = 0.2) {
  
  # Make half the agents women, half men
  agents <- unlist(model$agents)
  N <- length(agents)
  
  women_idxs <- sample(1:N, N/2)
  men_idxs <- setdiff(1:N, women_idxs)
  
  women <- purrr::map(women_idxs, \(ii) model$get_agent(ii))
  men <- purrr::map(men_idxs, \(ii) model$get_agent(ii))
  
  purrr::walk(
    women, 
    \(w) {
      w$set_attribute("Gender", "Woman")
      w$fitness_current = 0.0
      w$set_next_fitness(0.0)
    }
  )
  purrr::walk(
    men, 
    \(m) {
      m$set_attribute("Gender", "Man")
      m$fitness_current = 0.0
      m$set_next_fitness(0.0)
    }
  )
 
  # Assign women inverters to lead, men inverters to follow
  n_inverters_per_gender <- round(inversion_prevalence * N * 0.5)
  
  women_leader_idxs <- sample(women_idxs, n_inverters_per_gender)
  women_follower_idxs <- setdiff(women_idxs, women_leader_idxs)
  
  women_leaders <- purrr::map(women_leader_idxs, \(ii) model$get_agent(ii))
  women_followers <- purrr::map(women_follower_idxs, \(ii) model$get_agent(ii))
  
  purrr::walk(
    women_leaders, 
    \(w) {
      w$set_behavior("Lead")
      w$set_next_behavior("Lead")
    }
  )
  purrr::walk(
    women_followers, 
    \(w) {
      w$set_behavior("Follow")
      w$set_next_behavior("Follow")
    }
  )
  
  men_follower_idxs <- sample(men_idxs, n_inverters_per_gender)
  men_leader_idxs <- setdiff(men_idxs, men_follower_idxs)
  
  men_followers <- purrr::map(men_follower_idxs, \(ii) model$get_agent(ii))
  men_leaders <- purrr::map(men_leader_idxs, \(ii) model$get_agent(ii))
  purrr::walk(
    men_followers, 
    \(m) {
      m$set_behavior("Follow")
      m$set_next_behavior("Follow")
    }
  )
  purrr::walk(
    men_leaders, 
    \(m) {
      m$set_behavior("Lead")
      m$set_next_behavior("Lead")
    }
  )
}


assign_gendered_partners <- function(model) {
  agents <- model$agents
  
  purrr::walk(
    agents,
    \(agent) {
      gender <- agent$get_attribute("Gender")
      
      # Get neighbors (assumed to return agent objects)
      neighbors <- agent$get_neighbors()$agents
      
      # Separate same- and other-gender neighbors
      # Build list of potential teachers of same gender, including agent itself
      same_gender_neighbors <- purrr::keep(neighbors, \(n) n$get_attribute("Gender") == gender)
      teachers <- c(same_gender_neighbors, list(agent))
      agent$set_attribute("teachers", teachers)
      
      # Build list of potential domestic partners, neighbors of opposite gender
      agent$set_attribute(
        "partners", 
        purrr::keep(neighbors, \(n) n$get_attribute("Gender") != gender)
      )
    }
  )
}


make_dance_model <- function(n_agents = 100, inversion_prevalence = 0.5, 
                             deviance_penalty = 0.0, graph = NULL) {
  
  # Create learning strategy based on learning and iteration
  # functions defined above
  learning_strategy <- 
    make_learning_strategy(
      dance_partnering,
      dance_interaction,
      gender_coordination_social_learning,
      "Dance partner learning"
    )
  
  if (is.null(graph)) {
    abm <- make_abm(
      n_agents = n_agents, 
      learning_strategy = learning_strategy,
      inversion_prevalence = inversion_prevalence,
      deviance_penalty = deviance_penalty #,
      # graph = graph
    )
  } else {
    stop("Only complete network is supported; use n_agents to build model")
  }
  
  # Initialize all dancers as leaders or followers according to initial prevalence
  initialize_dancers(abm, inversion_prevalence)
  # Pre-compute each agent's potential dance partners (opposite gender) and teachers (same)
  assign_gendered_partners(abm)
  
  # Sync a parameter that tracks all women agents to avoid repeated filtering.
  women <- purrr::keep(abm$agents, \(a) a$get_attribute("Gender") == "Woman")
  abm$set_parameter("women", women)
  # Ditto for men.
  men <- purrr::keep(abm$agents, \(a) a$get_attribute("Gender") == "Man")
  abm$set_parameter("men", men)
  
  women_payoff_matrix <- 
    matrix(
      c(0, 1, 
        1, 0),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(
        "Focal" = c("Lead", "Follow"),
        "Partner" = c("Lead", "Follow")
      )
    )
  men_payoff_matrix <- 
    matrix(
      c(0,                    1, 
        1 - deviance_penalty, 0),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(
        "Focal" = c("Lead", "Follow"),
        "Partner" = c("Lead", "Follow")
      )
    )
  
  abm$set_parameter("payoff_matrix", 
                    list(Woman = women_payoff_matrix, 
                         Man = men_payoff_matrix))
  
  return (abm)  
}


#--------------- TRIAL RUN DEVELOPMENT --------------
genders_fixated <- function(model) {
  
  women_behaviors <- map_vec(model$get_parameter("women"), ~ .x$behavior_current)
  women_fixated <- all(women_behaviors[1] == women_behaviors)
  
  men_behaviors <- map_vec(model$get_parameter("men"), ~ .x$behavior_current)
  men_fixated <- all(men_behaviors[1] == men_behaviors)
  
  # If both fixated return TRUE
  return (women_fixated && men_fixated)
}


genders_fixated_plus_gen <- function(stop_steps = 5) {
  
  return (
    
    function(model) {
      
      stop <- FALSE
      
      if (genders_fixated(model)) {
        extra_steps <- model$set_parameter("extra_steps") + 1
        model$set_parameter("extra_steps", extra_steps)
        
        stop <- extra_steps > stop_steps
      }
      
      return (stop)
    }
    
  )
}


is_woman <- function(gender) {
  return (gender == "Woman")
}


is_leader <- function(behavior) {
  return (behavior == "Lead")
}


# Helper to check if the gender-behavior combo is an inverter
is_inverter <- function(gender, behavior) {
  woman_leader <- is_woman(gender) && is_leader(behavior)
  man_follower <- !is_woman(gender) && !is_leader(behavior)
  
  return (as.numeric(woman_leader || man_follower))
}