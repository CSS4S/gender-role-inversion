##
# Models and components for Ch 6 Achieving Fairness
# Author: Matt Turner
# Date: 2025-06-03
#

library(socmod)
library(magrittr)

#' Partner selection for partner dance game
#'
dance_partnering <- function(focal, model) {
  # Random selection of a potential dance partner
  return (sample(focal$get_attribute("partners"), 1)[[1]])
}


#' Partner dance game interaction that yields payoffs.
#'
dance_interaction <- function(focal, partner, model) {
  
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
      agent$set_next_fitness(teacher$fitness_current)
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
      w$set_attribute("gender", "Woman")
    }
  )
  purrr::walk(
    men, 
    \(m) {
      m$set_attribute("gender", "Man")
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
      gender <- agent$get_attribute("gender")
      
      # Get neighbors (assumed to return agent objects)
      neighbors <- agent$get_neighbors()$agents
      
      # Separate same- and other-gender neighbors
      
      
      # Build list of potential teachers of same gender, including agent itself
      same_gender_neighbors <- purrr::keep(neighbors, \(n) n$get_attribute("gender") == gender)
      teachers <- c(same_gender_neighbors, list(agent))
      agent$set_attribute("teachers", teachers)
      
      # Build list of potential domestic partners, neighbors of opposite gender
      agent$set_attribute(
        "partners", 
        purrr::keep(neighbors, \(n) n$get_attribute("gender") != gender)
      )
    }
  )
}


make_dance_model <- function(n_agents = 40, inversion_prevalence = 0.2, 
                             deviance_penalty = 0.0, graph = NULL) {
  if (is.null(graph)) {
    abm <- make_abm(
      n_agents = n_agents, 
      inversion_prevalence = inversion_prevalence,
      deviance_penalty = deviance_penalty #,
      # graph = graph
    )
  } else {
    stop("Only complete network is supported; use n_agents to build model")
  }
  
  initialize_dancers(abm, inversion_prevalence)
  assign_gendered_partners(abm)
  
  women_payoff_matrix <- # Define payoff matrix: row 1 is for focal cooperator, row 2 focal defector
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
  men_payoff_matrix <- # Define payoff matrix: row 1 is for focal cooperator, row 2 focal defector
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
  abm$set_parameter(
    "payoff_matrix", 
    
  )
  
  return (abm)  
}



#--------- TESTING INITIALIZE_DANCERS -----------
# abm <- make_abm(n_agents = 20);

# initialize_dancers(abm, inversion_prevalence = 0.2);

# cat("\n\nWomen's table of behaviors\n")
# print(
#   table(
#     unlist(
#       purrr::map(
#         unlist(
#           abm$agents %>% purrr::keep(\(a) a$get_attribute("gender") == "Woman")
#         ),
#         \(a) a$get_behavior()
#       )
#     )
#   )
# )

# cat("\n\nMen's table of behaviors\n")
# print(
#   table(
#     unlist(
#       purrr::map(
#         unlist(
#           abm$agents %>% purrr::keep(\(a) a$get_attribute("gender") == "Man")
#         ),
#         \(a) a$get_behavior()
#       )
#     )
#   )
# )


#--------- TESTING ASSIGN_TEACHERS

# cat("\n\nTesting potential teacher and domestic partner assignment success\n")
# assign_gendered_partners(abm)
# a1 <- abm$agents[[1]]
# a1_gender <- abm$agents[[1]]$get_attribute("gender")
# cat("\na1 gender: ", a1_gender)
# a1_teachers <- a1$get_attribute("teachers")
# 
# cat("\na1 teachers: ", purrr::map_vec(a1_teachers, ~ .x$get_name()))
# cat("\nAll a1 teachers same gender?\n")
# teacher_genders <-purrr::map_vec(a1_teachers, ~ .x$get_attribute("gender"))
# print(all(teacher_genders == a1_gender))
# 
# cat("\nAll a1 potential partners opposite gender?\n")
# partners <- a1$get_attribute("partners")
# partner_genders <- purrr::map_vec(partners, ~ .x$get_attribute("gender"))
# print(all(partner_genders != a1_gender))
# 
# cat("\n\n*** Checking genders explicitly: ***\n")
# cat("\nAgent gender:\n")
# print(a1_gender)
# cat("\nTeacher genders:\n")
# print(teacher_genders)
# cat("\nDomestic partner genders:\n")
# print(partner_genders)
