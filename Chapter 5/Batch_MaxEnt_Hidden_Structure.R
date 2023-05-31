


# The
  
  # An extension to Ch. 5: Expectation-Driven Learning of Vedic?

  # Start with your Gaussian priors
  # How much overlap in area under the curve is there for the two constraints?
  # The current weight for a constraint is always its mean, then all constraints have a 
# The probability that C2 actually has a higher weight than C1 = current weight of C2 assuming its true mean is the weight of C1, given the specified sigma^2
  
  # Prickett & Pater 2019
    # Expectation Maximization: estimating the probability of the data (UR --> SR mappings) when not all relevant 
    # information is present (here, constraint violations)
    # EM under MaxEnt lets you estimate the probability of each structure compatible with the datum, given
      # the current constraint weights
    # So with OVERT form [ZZZ] matching candidates b. and. c., then given current weights and the violation profiles of those
    # candidates, the probability of parse(b.) and parse(c.) = Maxent probability of b. bzw. c. given the UR.

# Prickett & Pater 2022
  # Get the observed vector by multiplying the RIP winner's violations by the probability of that RIP winner
  # Get the expected vector by multiplying the entire tableau of violations by the probabilities of the candidates, then
  # summing them

my_tableaux <- prepare_data("Peninitial_Stress_Check_2.txt") # expects an OT-Soft formatted table, with candidates as RIPs in the Praat representation with \-> separating the overt from from the RIP
my_overt_forms <- read.table("Peninitial_Overts.txt", header=T, sep="\t") # expects a tab-delimited text file with 2 columns
  # frequencies should be whole integers. Remember that the overt forms in this table must precisely match the overt forms in the tableaux!

get_compatibles <- function(OT_Tableau, overt_forms){
  temp_OT_Tableau <- OT_Tableau
  for(i in 1:length(overt_forms[,1])){ # select each overt form
    for(j in 1:length(OT_Tableau[[3]])){ # iterate over the candidates of overt --> RIP for each word
      temp_overt_matches <- grep(paste(overt_forms[i,1], " ", sep= ""), OT_Tableau[[3]][[j]]) # get the indices of the candidates containing that overt form
      if(length(temp_overt_matches) > 0){
        OT_Tableau[[1]][[j]][c(temp_overt_matches)] <- overt_forms[i, 2] # put the frequency of that overt form into the frequency for that OVERT-->Parsed pair
      }
     }
  }
  
  compatible_list <- list()
  for(i in 1:length(OT_Tableau[[1]])){
    compatible_inidices <- c()
    for(j in 1:length(OT_Tableau[[1]][[i]])){
      if(OT_Tableau[[1]][[i]][j] > 0){
        compatible_inidices <- append(compatible_inidices, j)
        compatible_list[[i]] <- compatible_inidices
      }
    }
  }
  compatible_quantities <- c()
  for(i in 1:length(compatible_list)){
    compatible_quantities <- append(compatible_quantities, length(compatible_list[[i]]))
  }
  number_combinations <- 1
  for(i in 1:length(compatible_quantities)){
    number_combinations <- number_combinations*compatible_quantities[i]
  }
  combination_list <- list()
  for(i in 1:(number_combinations*1000)){ # I generated the combinations in a Monte Carlo Fashion, because I was too stupid to successfully enumerate them.
    temp_combination <- c()
    for(j in 1:length(compatible_list)){
      temp_item <- sample(compatible_list[[j]], 1)
      temp_combination <- append(temp_combination, temp_item)
    }
    combination_list[[i]] <- temp_combination
  }
  combination_list  <- unique(combination_list) # up to here is a list of viable combinations of each winner
  log_likelihoods <- c()
  convergences <- c()
  weights <- list()
  results <- list()
  
  for(combo in 1:length(combination_list)){
    current_combination <- combination_list[[combo]]
    for(winners in 1:length(temp_OT_Tableau[[1]])){
      temp_OT_Tableau[[1]][[winners]] <- rep(0, length(temp_OT_Tableau[[1]][[winners]])) # fill the winners with zeros
      temp_OT_Tableau[[1]][[winners]][current_combination[winners]] <- 1 # set the one specified winner to 1
    }
    temp_output <- maxent_batch_weights(tableaux = temp_OT_Tableau)
    log_likelihoods <- append(log_likelihoods, temp_output$value)
    convergences <- append(convergences, temp_output$convergence)
    weights[[combo]] <- temp_output$par
  }
  results <- list(combination_list, log_likelihoods, convergences)
  results_matrix <- matrix(0, nrow=length(combination_list), ncol=3)
  for(item in 1:length(combination_list)){
    results_matrix[item, ] <- c(paste(as.character(combination_list[[item]]), collapse = " "), log_likelihoods[item], convergences[item])
  }
  results_frame <- as.data.frame(results_matrix)
  return(results)
  
  
    # remove the rows 
  # for(i in 1:4){# there are always exactly four list elements
  #   for(j in 1:length(OT_Tableau[[1]])){
  #     if(i == 2){ # condition for the matrices in list object 2
  #       temp_OT_Tableau[[i]][[j]] <- OT_Tableau[[i]][[j]][(-c(to_remove_list[[j]])), ]
  #   }
  #     else{
  #     temp_OT_Tableau[[i]][[j]] <- OT_Tableau[[i]][[j]][(-c(to_remove_list[[j]]))]
  #     }
  #   }
  # }
  return(temp_OT_Tableau)
}

my_compatibles <- get_compatibles(my_tableaux, my_overt_forms)

log_likelihoods <- c()
get_log_likelihoods <- function(my_full_tableaux, compatibles_tableaux){
  for(i in 1:length(compatibles_tableaux[[1]])){
    compatible_quantities <- append(compatible_quantities, length(compatibles_tableaux[[1]][[i]]))
  }
  number_combinations <- 1
  for(i in 1:length(compatible_quantities)){
    number_combinations <- number_combinations*compatible_quantities[i]
  }
  combination_list <- list()
  for(i in 1:(number_combinations*1000)){ # I generated the combinations in a Monte Carlo Fashion, because I was to stupid to successfully enumerate them.
    temp_combination <- c()
    for(j in 1:length(compatibles_tableaux[[1]])){
      temp_item <- sample(1:compatible_quantities[j], 1)
      temp_combination <- append(temp_combination, temp_item)
    }
    combination_list[[i]] <- temp_combination
  }
  combination_list  <- unique(combination_list) # up to here is a list of viable combinations of each winner
  for(combo in 1:length(combination_list)){
    current_combination <- combination_list[[combo]]
    for(winners in 1:length(my_full_tableaux[[1]])){
      my_full_tableaux[[1]][[winners]] <- rep(0, length(my_full_tableaux[[1]][[winners]])) # fill the winners with zeros
      my_full_tableaux[[1]][[winners]][current_combination[winners]] <- 1 # set the one specified winner to 1
    }
    temp_output <- maxent_batch_weights(tableaux = my_full_tableaux)
    log_likelihoods <- append(log_likelihoods, temp_output$value)
  }
  return(log_likelihoods)
}
  
  
  
  tableau_combinations <- vector(mode = "list", length = length(combination_list)) # compose a new tableaux with new candidate frequencies and violations
  for(combo in 1:length(combination_list)){
    current_combination <- combination_list[[combo]]
    current_candidate_frequencies <- list()
    current_violations <- list()
    for(item in 1:length(current_combination)){
      temp_item <- current_combination[item]
      current_candidate_frequencies[[item]] <- compatibles_tableaux[[1]][[item]][current_combination[item]]
      if(is.vector(compatibles_tableaux[[2]][[item]]) == TRUE){
        current_violations[[item]] <- compatibles_tableaux[[2]][[item]] 
      }
      else{
      current_violations[[item]] <- compatibles_tableaux[[2]][[item]][current_combination[item], ] # This crashes if there is only one row in the 
      }
    }
    tableau_combinations[[combo]][[1]] <- current_candidate_frequencies
    tableau_combinations[[combo]][[2]] <- current_violations
  }
  for(current_tableau in tableau_combinations){
    temp_output <- maxent_batch_weights(tableaux = current_tableau, hidden_structure_combos = T)
    log_likelihoods <- append(log_likelihoods, temp_output$value)
  }
  return(log_likelihoods)
}

 
   
  compatible_quantities <- c()
  for(i in 1:length(compatibles_tableaux[[1]])){
    compatible_quantities <- append(compatible_quantities, length(compatibles_tableaux[[1]][[i]]))
  }
  number_combinations <- 1
  for(i in 1:length(compatible_quantities)){
    number_combinations <- number_combinations*compatible_quantities[i]
  }
  temp_matrix <- matrix(0, nrow=number_combinations, ncol=length(compatible_quantities))
  for(i in 1:length(compatible_quantities)){
    temp_fraction <- number_combinations/compatible_quantities[i]
    temp_column <- rep(combn(1:compatible_quantities[i], compatible_quantities[i]),number_combinations/compatible_quantities[i])
    temp_matrix[, i] <- temp_column
  }
  
    # counter <- 1
    # old_temp_fraction <- temp_fraction
    #   for(j in 1:compatible_quantities[i]){
    #     temp_fraction <- j*old_temp_fraction
    #     #counter <- counter +1
    #     for(k in counter:temp_fraction){
    #       temp_matrix[k, i] <- j
    #       counter <- counter+1
    #      }
    #     }
    #   }
}

# What you want to be able to do is to handle both parsing (i.e., considering the possible parses of overt surface forms) 
# and variation (i.e., cases where there are different surface forms for the same UR)
# If there is no variation, then you just iterate over the possible combinations of parses
  # compatible with the 

#########
# Online Hidden Structure in MaxEnt



my_tableaux <- prepare_data("Peninitial_Stress_Check_2.txt") # expects an OT-Soft formatted table, with candidates as RIPs in the Praat representation with \-> separating the overt from from the RIP
my_overt_forms <- read.table("Peninitial_Overts.txt", header=T, sep="\t") # expects a tab-delimited text file with 2 columns
# frequencies should be whole integers. Remember that the overt forms in this table must precisely match the overt forms in the tableaux!
# How to sample overt forms
prob_overt_forms <- c(my_overt_forms[which(my_overt_forms[, 2] > 0), 2])/(sum(c(my_overt_forms[which(my_overt_forms[, 2] > 0), 2])))
sample(my_overt_forms[which(my_overt_forms[, 2] > 0), 1], 1, prob = prob_overt_forms)



get_cand_prob <- function(violations, weights){
  # Because vectors are "recycled" by column, violation vectors need to be transposed to be arranged by column, rather than row, then transposed back.
  applied_weights <- t(t(violations)*weights) # Calculate the "force" of each violation
  harmonies = apply(applied_weights, 1, sum) # Sum across each row to obtain the Harmony (H) of each candidate.
  e_harmonies = exp(-1*harmonies) # Take the exponential of the harmonies (e-harmony)
  Z = sum(e_harmonies) # Sum the e-harmonies to obtain the denominator (Z) for calculating candidate probabilities
  cand_probs <- e_harmonies/Z # Candidate probabilites are the e-harmony of each candidate divided by their sum.
  return(cand_probs)
}

initial_learner_candidate_probs <- function(learner_violations = learner_violations, learner_weights = learner_weights){
  learner_candidate_probs <- list()
  for(i in 1:length(learner_violations)){
    current_candidate_probs <- get_cand_prob(violations = learner_violations[[i]], weights = learner_weights)
    learner_candidate_probs[[i]] <- current_candidate_probs
  }
  return(learner_candidate_probs)
}

update_rule <- function(teacher_winner, learner_winner, rate, learner_weights){
  change_in_weights <- rate*(learner_winner - teacher_winner) # learner-teacher or teacher-learner -- I think the former for positive weights; cf. Jarosz 2016: 204
  #negatives <- which(change_in_weights < 0)
  #change_in_weights[negatives] <- 0
  learner_weights <- learner_weights + change_in_weights
  negatives <- which(learner_weights < 0) # GLA allows negative weights, I think
  learner_weights[negatives] <- 0
  return(learner_weights)
}


simple_single_learning_loop <- function(tableaux, number_iterations = 5000, learning_rate = 0.01){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  #old_weights_ident_prom <- c(0)
  number_updates <- 0
  no_learning <- 0
  # Set initial weights to 0
  learner_weights <- rep(0,ncol(tableaux[[2]][[1]]))
  learner_violations <- tableaux[[2]]
  learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
  for(i in 1:number_iterations){
    # The first sampling behaviors will need to be changed and updated
    sampled_item <- sample(my_overt_forms[which(my_overt_forms[, 2] > 0), 1], 1, prob = prob_overt_forms)
    sampled_item_neutralized <- gsub("[0-9]", "", sampled_item)
    sampled_tableau_number <- grep(paste("/", sampled_item_neutralized, "/", sep=""),  names(tableaux[[3]]))
    # For 2nd generations
    # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    possible_winner_numbers <- grep(paste(sampled_item, " ", sep=""), tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    
    learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
    learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)])
      teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ]
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights)
      #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      number_updates <- number_updates + 1
      #no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
    # if(no_learning > (number_iterations * 0.01)){
    #   break
    # }
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, number_updates) #  old_weights_ident_prom
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
  names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
  for(i in 1:length(final_learner_outcome[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}

# assumes an output as a list with all tableaux in element 1 of the list
# also generally assumes that each tableau has only one most probable winner
get_winners <- function(output_list){
  winners <- c()
  for(n_tableaux in 1:length(output_list[[1]])){
    current_winner <- which(output_list[[1]][[n_tableaux]] == max(output_list[[1]][[n_tableaux]]))
    winners <- append(winners, current_winner)
  }
  return(winners)
}

final_grammars <- list()
for(iteration in 1:100){
  my_output <- simple_single_learning_loop(my_tableaux, number_iterations = 10000, learning_rate = 0.1)
  current_iteration_winners <- get_winners(my_output)
  final_grammars[[iteration]] <- current_iteration_winners
}


