
# MaxEnt Learning 3

  #1. prepare_data(): reads in an OTSoft-formatted file, returns a list object with four elements:
      # 1. lists named by inputs containing vectors of winner frequencies
      # 2. lists named by inputs containing matrices of violation profiles
      # 3. lists named by inputs containing the names of candidates
      # 4. a vector of constraint names

  #2. make_tableaux_probabilities(): calculate the relative frequency of each type in the input data

  #3. sample_candidate(): function for sampling "winning" candidates from probability distributions provided in the data.





#1. prepare_data(): reads in an OTSoft-formatted file,
prepare_data <- function(OT_Table_File){
raw_file <- read.table(file =  OT_Table_File, sep="\t", header=F, fill=T, stringsAsFactors = F, skip=2, quote=NULL) # read in an OTSoft-formatted file, ignoring the first two rows with constaint names
raw_file[is.na(raw_file)] <- 0 # replace all empty constraint evaluations with 0
header <- unlist(strsplit(scan(OT_Table_File, what="char", sep="\n")[1], split="\t")) # get the constraint names from the same files
colnames(raw_file) <- header # add the constraint names as a header to the candidate data frame
constraint_names <- header[4:length(header)] # get the names of the constraints
number_of_constraints <- length(constraint_names) # get the number of constraints

cand_freqs <- list() # initialize an empty list object to contain the frequencies of the candidates
current_tableau <- 0 # initialize a counter
for(i in which(raw_file[,1] != "")){
  current_tableau <- current_tableau +1
  current_cand_freqs <- c() # initialize an empty vector for candidate frequencies
  current_cand_freqs <- append(current_cand_freqs, raw_file[i,3]) # put the frequency of the first candidate for the current tableaux in the empty vector
  iterator <- i # set an iterator equal to current iterator object i
  while((raw_file[iterator+1,1] == "")){ # as long as the first column in the table is empty (i.e., no new candidate is given), proceed through the candidate frequencies
    current_cand_freqs <- append(current_cand_freqs, raw_file[iterator+1,3]) # append the frequency of the candidate given in column 3
    if(iterator+1 < nrow(raw_file)){ # check that the current line of the table is not at the end of the table
      iterator <- iterator + 1
    }
    else{ # if we have reached the end of the table, exit the while-loop
      break
    }
  }
  cand_freqs[[current_tableau]] <- current_cand_freqs # store the current collected candidate frequencies in the current tableau
}

violations <- list()
current_tableau <- 0
for(i in which(raw_file[,1] != "")){
  number_candidates <- 1
  current_tableau <- current_tableau +1
  current_tableau_violations <- c()
  current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[i,4:ncol(raw_file)]))
  iterator <- i
  while((raw_file[iterator+1,1] == "")){
    current_tableau_violations <- append(current_tableau_violations, as.numeric(raw_file[iterator+1,4:ncol(raw_file)]))
    number_candidates <- number_candidates + 1
    if(iterator+1 < nrow(raw_file)){
      iterator <- iterator + 1
    }
    else{
      break
    }
  }
  total_tableau_violations <- matrix(current_tableau_violations, nrow=number_candidates, byrow = T)
  violations[[current_tableau]] <- total_tableau_violations
}

candidates <- list()
current_tableau <- 0
for(i in which(raw_file[,1] != "")){
  current_tableau <- current_tableau +1
  current_candidates <- c()
  current_candidates <- append(current_candidates, raw_file[i,2])
  iterator <- i
  while((raw_file[iterator+1,1] == "")){
    current_candidates <- append(current_candidates, raw_file[iterator+1,2])
    if(iterator+1 < nrow(raw_file)){
      iterator <- iterator + 1
    }
    else{
      break
    }
  }
  candidates[[current_tableau]] <- current_candidates
}

tableaux <- list(cand_freqs, violations, candidates)

names(tableaux[[1]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
names(tableaux[[2]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
names(tableaux[[3]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])

candidate_probabilities <- list()
current_tableau <- 0
for(i in 1:length(tableaux[[1]])){
  current_tableau <- current_tableau + 1
  current_tableau_probabilities <- c()
  total_token_frequency <- sum(tableaux[[1]][[i]])
  for(j in 1:length(tableaux[[1]][[i]])){
    current_candidate_probability <- tableaux[[1]][[i]][j]/total_token_frequency
    current_tableau_probabilities <- append(current_tableau_probabilities, current_candidate_probability)
  }
  candidate_probabilities[[current_tableau]] <- current_tableau_probabilities
}

tableaux[[4]] <- candidate_probabilities


names(tableaux[[4]]) <- as.character(raw_file[which(raw_file[,1] != ""), 1])
tableaux[[5]] <- constraint_names




return(tableaux)

}

# 2. make_tableaux_probabilities(): calculate the relative frequency of each type in the input data
make_tableaux_probabilities <- function(tableaux){
  tableaux_probs <- c() # initialize empty vector
  sum_type_frequencies <- sum(unlist(tableaux[[1]]))
  for(i in 1:length(tableaux[[1]])){
    current_tableau_probability <- sum(tableaux[[1]][[i]])/sum_type_frequencies
    tableaux_probs <- append(tableaux_probs, current_tableau_probability)
  }
  return(tableaux_probs)
}


#3. sample_candidates()

sample_candidate <- function(tableaux, token_freqs_of_types=F, tableaux_probs = tableaux_probs){ # requires a tableaux object created with prepare_data() above. If the types are to be sampled with their relative frequencies, then a tableaux_probs object created with make_tableaux_probilities() must be provided.
  # Can be improved by adding token frequencies of the types and then using that as a probability for the tableaux sampling
  if(token_freqs_of_types == T){
    current_tableaux <- sample(length(tableaux[[1]]), size = 1, prob = tableaux_probs)
  }
  else{
    current_tableaux <- sample(length(tableaux[[1]]), size = 1)
  }
  current_candidate <- sample(unlist(tableaux[[3]][current_tableaux]), size = 1, prob = unlist(tableaux[[1]][current_tableaux]))
  names(current_candidate) <- names(tableaux[[3]][current_tableaux])
  return(current_candidate)
}


# 4. get_candidate_prob(): calculate the MaxEnt probability of a candidate (the exponential harmony of the candidate divided by the sum of the exponential harmonies)

get_cand_prob <- function(violations, weights){
  # Because vectors are "recycled" by column, violation vectors need to be transposed to be arranged by column, rather than row, then transposed back.
  applied_weights <- t(t(violations)*weights) # Calculate the "force" of each violation
  harmonies = apply(applied_weights, 1, sum) # Sum across each row to obtain the Harmony (H) of each candidate.
  e_harmonies = exp(-1*harmonies) # Take the exponential of the harmonies (e-harmony)
  Z = sum(e_harmonies) # Sum the e-harmonies to obtain the denominator (Z) for calculating candidate probabilities
  cand_probs <- e_harmonies/Z # Candidate probabilites are the e-harmony of each candidate divided by their sum.
  return(cand_probs)
}

# 5. get_plog(): obtain the PLOG (log probability) of each candidate, multipled by its frequency, and then summed. This is the "objective function" used in batch processing of data.
  # Relies on the function get_cand_prob() above.

get_plog <- function(weights, violations, cand_freqs, mu = prespecified_weights, sigma2 = prespecified_deviations){ # Note: it doesn't matter what the starting weights are for the purposes of optimization. The get_plog function will return a PLOG for a given set of weights, but the optimization function looks for the minimum possible PLOG.
  ## mu must be a vector of prespecified weights
  ## sigma must be a vector of prespecified standard deviations
  plogs <- c()
  for(i in 1:length(cand_freqs)){
    current_cand_freqs <- cand_freqs[[i]] # take the specific frequencies of the candidates from the tableaux object (element 1 in the list = tableaux[[1]])
    temp_cand_probs <- get_cand_prob(violations[[i]], weights) # get the probabilities of the candidates, given some weights and a matrix of violations
    temp_cand_prob_logs <- log(temp_cand_probs) # take the natural logarithm of the candidate probabilities
    temp_plog <- sum(temp_cand_prob_logs*current_cand_freqs) # append to a vector with other plogs
    gaussian_prior <- sum((weights-mu)^2 / (2*sigma2)) # calculate the prior for the given prespecified weights and standard deviations of the constraints
    temp_gaussian_plog <- temp_plog - gaussian_prior
    plogs <- append(plogs, temp_gaussian_plog)
  }
  return(sum(plogs)) # return the sum of all the individual PLOGs
}

# 6. maxent_batch_weights(): return the optimized weights based on minimizing the PLOG calculated using get_plog() above, using the "L-BFGS-B" optimization method
  maxent_batch_weights <- function(user_initial_weights = F, user_initial_cand_freqs = F, tableaux = tableaux, lower_weight = 0, upper_weight = 50, my_initial_weights = c(), my_target_weights = c(), my_initial_deviations = c(), my_candidate_frequencies = c()){#, hidden_structure_combos = F
    if(user_initial_cand_freqs == T){
      cand_freqs = my_candidate_frequencies
    }
    else{
      cand_freqs = tableaux[[1]]
    }
    # Only if user_initial_weights is set to T does it accept a prespecified mu and sigma for constraint weights
    if(user_initial_weights == T){
      initial_weights = my_initial_weights
      mu = my_target_weights
      sigma2 = my_initial_deviations
    }
    # if(hidden_structure_combos == T){
    #   initial_weights <- rep(0,length(tableaux[[2]][[1]]))
    #   mu = initial_weights
    #   sigma2 = rep(10000, length(tableaux[[2]][[1]]))
    # }
    else{
      initial_weights <- rep(0,ncol(tableaux[[2]][[1]]))
      mu = initial_weights
      sigma2 = rep(10000, ncol(tableaux[[2]][[1]]))
    }
    optimized_out <- optim(par = initial_weights, fn = get_plog, violations = tableaux[[2]], cand_freqs = cand_freqs, mu = mu, sigma2 = sigma2, method="L-BFGS-B", lower = lower_weight, upper = upper_weight, control = list(fnscale=-1))
    return(optimized_out)
  }

# 7. get_learning_data(): sample a specified number of outputs from a teacher tableaux using sample_candidate()
  get_learning_data <- function(teacher_tableaux = tableaux, number_tokens = 100, token_freqs_of_types=F, tableaux_probs = tableaux_probs){
    learning_tokens <- c()
    for(i in 1:number_tokens){
      learning_tokens <- append(learning_tokens, sample_candidate(teacher_tableaux, token_freqs_of_types = token_freqs_of_types, tableaux_probs = tableaux_probs))
    }
    return(learning_tokens)
  }

#8. make_candidate_frequencies(): obtain the frequencies of candidates after sampling from learning data using function 7., get_learning_data()
  make_candidate_frequencies <- function(learning_data, tableaux = tableaux){
    candidate_frequencies_from_data <- tableaux[[1]]
    for(i in 1:length(candidate_frequencies_from_data)){
      for(j in 1:length(candidate_frequencies_from_data[[i]])){
        candidate_frequencies_from_data[[i]][j] <- 0
      }
    }
    for(i in 1:length(learning_data)){
      current_input_tableau <- which(names(tableaux[[3]]) == names(learning_data[i]))
      current_candidate_number <- which(tableaux[[3]][[current_input_tableau]] == learning_data[i])
      candidate_frequencies_from_data[[current_input_tableau]][current_candidate_number] <- candidate_frequencies_from_data[[current_input_tableau]][current_candidate_number] + 1
    }
    return(candidate_frequencies_from_data)
  }
    

# 9. incremented_batch_learning(): iteratively applies batch learning to samples drawn from an original candidate distribution
  incremented_batch_learning <- function(input_tableaux = tableaux, number_increments = 10, increment_size = 50){ 
    weight_progression <- list()
    for(i in 1:number_increments){
      temp_learning_data <- get_learning_data(teacher_tableaux = input_tableaux, number_tokens = increment_size, token_freqs_of_types = T, tableaux_probs = tableaux_probs)
      temp_candidate_frequencies <- make_candidate_frequencies(temp_learning_data, tableaux = input_tableaux)
      if(i == 1){
        temp_learned_weights <- maxent_batch_weights(user_initial_weights = T, user_initial_cand_freqs = T, tableaux = input_tableaux, upper_weight = 50, my_initial_weights = mu, my_target_weights = mu, my_initial_deviations = sigma2, my_candidate_frequencies = temp_candidate_frequencies)$par
        weight_progression[[i]] <- temp_learned_weights
      }
      if(i > 1){
        temp_learned_weights <- maxent_batch_weights(user_initial_weights = T, tableaux = tableaux, upper_weight = 50, my_initial_weights = weight_progression[[i-1]], my_target_weights = mu, my_initial_deviations = sigma2, my_candidate_frequencies = temp_candidate_frequencies)$par
        weight_progression[[i]] <- temp_learned_weights
      }
    }
    return(weight_progression)
  }

# Do the progression with increasingly large N  

# Test with simple lexical stress grammar described at 3.6 of the Habilitation  

setwd("/Users/rpsandell/Documents/Dropbox/GitHub/Sandell-LMU-Habilitation/Chapter 3/Simple Lexical Stress")
    
simple_stress <- prepare_data("Simple_Lexical_RIP.txt")
mu = c(0, 10, 10, 10,10, 10, 10, 10, 10, 10)
sigma2 = rep(0.6, 10)
tokens_initial <- sum(simple_stress[[1]][[1]])
tokens_final <- sum(simple_stress[[1]][[2]])
n_generations = 50 # set the number of learning generations
generation_weights <- list() # keep a list of the weights for each generation of learning
proportion_error <- c(0)

# With "changed tokens" added to the outputs of tableau 1 (underlying initial stress)
for(i in 1:n_generations){
  round1 <- maxent_batch_weights(user_initial_weights = T, tableaux = simple_stress, my_initial_weights = mu, my_target_weights = mu, my_initial_deviations = sigma2) # get weights for a learning generation, given specified mu and sigma2; note that tableaux = simple_stress, where simple_stress contains the input tableaux
  generation_weights[[i]] <- round1$par # save these weights in the list of weights 
  initial_stress_probs <- get_cand_prob(simple_stress[[2]][[1]], round1$par) # get the probabilities of the parses for the four candidates on the tableau with underlying initial stress
  final_stress_probs <- get_cand_prob(simple_stress[[2]][[2]], round1$par) # get the probabilities of the parses for the four candidates on the tableau with underlying final stress
  proportion_error <- append(proportion_error, sum(get_cand_prob(simple_stress[[2]][[2]], round1$par)[3:4])) # 3 and 4 are the candidates with initial stress given a final stress input
  changed_tokens <- sum(simple_stress[[1]][[2]])*sum(get_cand_prob(simple_stress[[2]][[2]], round1$par)[3:4])/2 # take the proportion of outputs having final stress and multiply that by the total number of tokens assumed to correspond to lexemes with final stress, and divide by 2, so that the amount can be distributed over two parses for initial stres
  simple_stress[[1]][[1]][1] <- simple_stress[[1]][[1]][1]+changed_tokens # add this number of changed tokens to an initial stress parse of the input with initial stress
  simple_stress[[1]][[1]][3] <- simple_stress[[1]][[1]][3]+changed_tokens # add this number of changed tokens to an initial stress parse of the input with initial stress
  simple_stress[[1]][[2]][1] <- simple_stress[[1]][[2]][1]-changed_tokens # subtract this number of changed tokens from a final stress parse of the input with final stress
  simple_stress[[1]][[2]][2] <- simple_stress[[1]][[2]][2]-changed_tokens # subtract this number of changed tokens from a final stress parse of the input with final stress
  tokens_initial <- append(tokens_initial, sum(simple_stress[[1]][[1]]))
  tokens_final <- append(tokens_final, sum(simple_stress[[1]][[2]]))
}

# With "changed tokens" added to the outputs of tableau 2 (underlying final stress)
for(i in 1:n_generations){
  round1 <- maxent_batch_weights(user_initial_weights = T, tableaux = simple_stress, my_initial_weights = mu, my_target_weights = mu, my_initial_deviations = sigma2) # get weights for a learning generation, given specified mu and sigma2; note that tableaux = simple_stress, where simple_stress contains the input tableaux
  generation_weights[[i]] <- round1$par # save these weights in the list of weights 
  initial_stress_probs <- get_cand_prob(simple_stress[[2]][[1]], round1$par) # get the probabilities of the parses for the four candidates on the tableau with underlying initial stress
  final_stress_probs <- get_cand_prob(simple_stress[[2]][[2]], round1$par) # get the probabilities of the parses for the four candidates on the tableau with underlying final stress
  proportion_error <- append(proportion_error, sum(get_cand_prob(simple_stress[[2]][[2]], round1$par)[3:4])) # 3 and 4 are the candidates with initial stress given a final stress input
  changed_tokens <- sum(simple_stress[[1]][[2]])*sum(get_cand_prob(simple_stress[[2]][[2]], round1$par)[3:4])/2 # take the proportion of outputs having final stress and multiply that by the total number of tokens assumed to correspond to lexemes with final stress, and divide by 2, so that the amount can be distributed over two parses for initial stres
  simple_stress[[1]][[2]][3] <- simple_stress[[1]][[2]][3]+changed_tokens # add this number of changed tokens to an initial stress parse of the input with initial stress
  simple_stress[[1]][[2]][4] <- simple_stress[[1]][[2]][4]+changed_tokens # add this number of changed tokens to an initial stress parse of the input with initial stress
  simple_stress[[1]][[2]][1] <- simple_stress[[1]][[2]][1]-changed_tokens # subtract this number of changed tokens from a final stress parse of the input with final stress
  simple_stress[[1]][[2]][2] <- simple_stress[[1]][[2]][2]-changed_tokens # subtract this number of changed tokens from a final stress parse of the input with final stress
  tokens_initial <- append(tokens_initial, sum(simple_stress[[1]][[1]]) + sum(simple_stress[[1]][[2]][3:4]) )
  tokens_final <- append(tokens_final, sum(simple_stress[[1]][[2]][1:2]))
}

stress_type <- c(rep("Intial", 51), rep("Final", 51))
stress_tokens <- c(tokens_initial, tokens_final)
stress_experiment_data <- data.frame(generation = c(1:51), proportion_error, tokens_initial, tokens_final, stress_type, stress_tokens)

library(ggplot2)
proportion_error_plot <- ggplot(data = stress_experiment_data, aes(x = generation, y = proportion_error)) + geom_line(color="blue") + geom_point() + ggtitle("Proportion  Error of Tokens with Final Stress")+
  xlab("Generation") + ylab("Proportion Error")

tokens_plot <- ggplot(data = stress_experiment_data, aes(x = generation)) + geom_line(aes(y=tokens_initial), color="blue") + geom_line(aes(y=tokens_final), color="red") + ggtitle("Token Distribution of Initial and Final Stress")+
  xlab("Generation") + ylab("Tokens")

tokens_plot_2 <-  ggplot(data = stress_experiment_data, aes(x = generation, y=stress_tokens, color=stress_type)) + geom_line() + ggtitle("Token Distribution of Initial and Final Stress")+
  xlab("Generation") + ylab("Tokens") + labs() + guides(color = guide_legend(title="Stress Type")) # yes!

# You will probably want to set initial weights and a standard deviation for them, in most instances
# This is done outside the function call
my_learner_weights <- rep(5 ,ncol(my_tableaux[[2]][[1]])) # For all weights high

batch_maxent_RIP_metrics <- function(tableaux = my_tableaux, overt_forms = my_overt_forms, initial_weights = my_learner_weights, my_mu = c(), my_sigma2 = c(), number_chews = 1){
  
  # Set the frequency of all candidates for all tableaux to 0. If everything below works properly, then at least 1 candidate 
  # in each tableaux should receive a token frequency of at least 1.
  for(i in 1:length(tableaux[[1]])){
    tableaux[[1]][[i]] <- rep(0, length(tableaux[[1]][[i]]))
  }
  
  my_learner_violations <- my_tableaux[[2]]
  learner_cand_probs <- initial_learner_candidate_probs(my_learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
  
  for(overt_id in 1:length(overt_forms)){
    sampled_item <- overt_forms[overt_id]
    sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
    sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]]))
    possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
    learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]]) # This will use the probabilities of the candidates in the initial state.
    if(learner_winner_number %in% possible_winner_numbers){ # This choice will bias the learner towards the most probably candidates in the initial state.
      tableaux[[1]][[sampled_tableau_number]][learner_winner_number] <- tableaux[[1]][[sampled_tableau_number]][learner_winner_number] + 1
    }
    else{
      if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
        teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
      }
      else{ # else sample from the possible winners
        teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners as the winner from the learner's perspective, based on what they think is most probable
      }
      tableaux[[1]][[sampled_tableau_number]][teacher_winner_number] <- tableaux[[1]][[sampled_tableau_number]][teacher_winner_number] + 1
    }
  }
  # Add a check to ensure that no vector of candidate frequencies consists solely of 0s
  for(i in 1:length(tableaux[[1]])){
    temp_sum <- sum(tableaux[[1]][[i]])
    if(temp_sum == 0){
      tableaux[[1]][[i]] <- rep(1, length(tableaux[[1]][[i]] ))
    }
  }
  # set the initial and target weights with a given standard deviation
  # for current metrics setup, recommended is:
    # sigma2 = rep(0.6, length(tableaux[[5]]))
    # mu = my_learner_weights
  mu = my_mu
  sigma2 <- my_sigma2
  if(length(mu) == 0){
    temp_optimized_output <- maxent_batch_weights(user_initial_weights = F, tableaux = tableaux, my_initial_weights = mu, my_target_weights = mu, my_initial_deviations = sigma2)
  }
  else{
    temp_optimized_output <- maxent_batch_weights(user_initial_weights = T, tableaux = tableaux, my_initial_weights = mu, my_target_weights = mu, my_initial_deviations = sigma2)
  }
  output_cand_probs <- initial_learner_candidate_probs(tableaux[[2]], temp_optimized_output$par)
  names(temp_optimized_output$par) <- tableaux[[5]]
  current_learner_output <- list(output_cand_probs, temp_optimized_output$par)
  names(current_learner_output[[1]]) <- names(tableaux[[1]])
  for(i in 1:length(current_learner_output[[1]])){
    names(current_learner_output[[1]][[i]]) <- tableaux[[3]][[i]]
  }
  learner_weights_frame <- as.data.frame(rbind(temp_optimized_output$par))
  colnames(learner_weights_frame) <- tableaux[[5]]
  previous_chew_learner_cand_probs <- current_learner_output[[1]]
  chew_results[[1]] <- current_learner_output
  current_chew_output_probabilities <- c()
  for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
    #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
    current_output_probabilities <- get_position_probabilities(current_learner_output, i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
    # you really only need the output probabilities 
    current_chew_output_probabilities <- append(current_chew_output_probabilities, current_output_probabilities)
  }
  if(length(current_chew_output_probabilities) == 502){
    stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
  }
  else{
    stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
  }
  current_SSE_results <- c() 
  for(i in 3:ncol(stress_patterns_for_SSE)){
    temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_chew_output_probabilities)
    current_SSE_results <- append(current_SSE_results, temp_SSE_results)
  }
  total_SSE_results <- as.data.frame(rbind(current_SSE_results))
  colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
  if(number_chews > 1){
    current_chew = 2
  }
  while(current_chew > 1 & current_chew <= number_chews){
    
  }
}




