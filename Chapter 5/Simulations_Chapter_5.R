### SIMULATIONS FOR CHAPTER FIVE

  # Dependent on functions from "MaxEnt_Learning_2.R":
    # prepare_data()
    # sample_candidate()
    # get_cand_prob()
    # initial_learner_candidate_probs()
    # update_rule()


### 1. Simulations with no lexical stress

# Preliminary Step: Read in the tableaux and overt forms
setwd("/Users/rpsandell/Documents/Dropbox/GitHub/Sandell-LMU-Habilitation/Chapter 5")
#tableaux <- prepare_data("RIP_Vedic_No_Lexical_Final_V2_fixed.txt") # read in tableaux
tableaux <- prepare_data("C_Skt_for_RIP_corrected_3.txt") # read in tableaux
#tableaux <- prepare_data("RIP_Vedic_No_Lexical_Final_V3.txt")

all_prosodic_shapes_frame <- read.table("RV_All_prosodic_shapes2.txt", header=T, sep="\t") # read in table of stress-marked prosodic shapes with their token frequencies
all_prosodic_shapes_types_frame <- read.table("RV_All_prosodic_shapes_types.txt", header=T, sep="\t") # same with type frequencies, though this is not likely to be used

# Restrict data to 2-5 syllable words
limited_2_3_4_5_indices <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", all_prosodic_shapes_frame$all_prosodic_shapes) # extract the row numbers of 2-5 syllable words
limited_2_3_4_5_indices_types <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique)
# Sample a token from the probability distribution. Test scripts for sampling to include in function below 
sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
# by type frequencies; for using the type frequencies in simulations, you just have to plug this line in 
sampled_item <- sample(all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique[limited_2_3_4_5_indices_types], 1, prob = (all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types]/sum(all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types])))

sampled_item # see what the token is
sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the stress
sampled_item_no_stress # check it
sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
sampled_tableau_number # check that it exists; there should be exactly one winner

possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate

# Check that all overt forms can be matched to a tableau
test_amount <- 30000
missing_items <- c()
for(i in 1:test_amount){
  sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
  #sampled_item # see what the token is
  sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the stress
  #sampled_item_no_stress # check it
  sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
  #sampled_tableau_number # check that it exists; there should be exactly one winner
  if(length(sampled_tableau_number) == 0){
    missing_items <- append(missing_items, sampled_item)
  }
}
unique(missing_items) # Works!


# Learning function for simulations with no lexical stress
# This assumes the presence of an object with the name "tableaux" in the global environment, and the availability of the indices for 2-5 syllable words in "limited_2_3_4_5_indices"
simple_single_learning_loop_no_lexical <- function(number_iterations = 5000, learning_rate = 0.01, number_generations = 1){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  iteration_weights <- list()
  #old_weights_ident_prom <- c(0)
  number_updates <- 0 # keep track of how many total updates to weights have occurred
  no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
  for(i in 1:number_iterations){
    # The first sampling behaviors will need to be changed and updated
    #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
    #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
    # For 2nd generations
    # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
    sampled_item # see what the token is
    sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
    sampled_item_no_stress # check it
    sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
    sampled_tableau_number # check that it exists; there should be exactly one winner
    
    possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
    learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]]) # sample a winner for the learner
    learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
      iteration_weights[[i]] <- learner_weights # save current weights
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)]) # choose one of the possible winners
      teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights) # update the constraint weights
      #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
      iteration_weights[[i]] <- learner_weights # save current weights
      number_updates <- number_updates + 1 # 
      no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
    #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
    # break
    #}
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
  names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
  for(i in 1:length(final_learner_outcome[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}

### Code for multigenerational single-agent learning
## GO HERE
simple_learning_loop_no_lexical <- function(number_iterations = 5000, learning_rate = 0.01, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilties and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to obtain a prosodic shape
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        #print("teacher winner number ok!")
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c()
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- rbind(total_SSE_results, current_SSE_results)
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains four items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; a dataframe with the SSEs for some possible stress patterns
    return(final_generation_results)
  }
}


### Generating a dataframe with the final predictions of a simulation
# Input Shape # Output # Probability # Frequency in original data
make_frequency_frame <- function(generation_results = generation_results, desired_generation = 1){
  final_output_frequencies <- as.data.frame(cbind(0,0,0,0))
  for(i in 1:length(learner_cand_probs)){
    input_shape <- names(generation_results[[desired_generation]][[1]])[i]
    current_output_probabilities <- get_position_probabilities(generation_results[[desired_generation]], i)
    output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities))
    for(output in 1:length(current_output_probabilities)){
      frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])]
      to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt)
      final_output_frequencies <- rbind(final_output_frequencies, to_be_bound)
    }
  }
  final_output_frequencies <- final_output_frequencies[-c(1), ]
  colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq")
  final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability
  frequency_input_shapes <- c()
  for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
    quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], final_output_frequencies$Original_Freq[input_shape])
    frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
  }
  table_input_shape_frequencies <- table(frequency_input_shapes)
  
  predicted_new_frequencies <- c()
  for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
    current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
    input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
    predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
    predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
  }
  final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies)
  return(final_output_frequencies)
}


## Simulation 1
# Single round of learning
# Set initial state
learner_weights <- rep(0 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 0
learner_violations <- tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.

small_test_SSEs <- simple_learning_loop_no_lexical(number_iterations = 1000, learning_rate = 0.01, number_generations = 3) 
big_test_SSEs_1 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50)
big_test_SSEs_2 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50)


# It seems important for the learning rate to be sufficiently high to change much of anything

## Five initial single generation simulations

ptm <- proc.time()
out_fifty <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50) 
proc.time() - ptm # runs in less than 20 seconds on my system when number_iterations = 5000

# Five generations of learning
learner_weights <- rep(0 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 0
learner_violations <- tableaux[[2]] # extract the violation profiles of all tableaux
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.
for_my_overts <- simple_learning_loop_no_lexical(number_iterations = 1000, learning_rate = 0.1, number_generations = 1)
out_two_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 2)

out_three_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 3)

out_five_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 5)



out_ten_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 10)


out_20_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 20)

# Current out_ten_gen was converging on a WSP, right-oriented grammar
# Current out_20_gen was converging on a left-edge oriented grammar

### 10 50-generation simulations
ptm <- proc.time()
out_50_gen_1 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
proc.time() - ptm # runs in less than 20 seconds on my system when number_iterations = 5000
out_50_gen_1_slow <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.05, number_generations = 50)
# gen 1 is an interesting and peculiar pattern, not really categorical though
out_50_gen_2 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
# gen 2 is an initial stress
out_50_gen_3 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
# gen 3 is initial stress
out_50_gen_4 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
# gen 4 Tendentially left edge, but some peculiarities
out_50_gen_5 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)

out_50_gen_6 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
out_50_gen_7 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
out_50_gen_8 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
out_50_gen_9 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)
out_50_gen_10 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50)


#
out_50_gen_1_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50)

five_single_generation_simulations <- list()
sim_quantity <- 5
for(sim in 1:sim_quantity){
  five_single_generation_simulations[[sim]] <- simple_learning_loop_no_lexical(number_iterations = 20000, learning_rate = 0.01, number_generations = 1)
}

iteration_weights_sim_1 <- do.call(rbind.data.frame, five_single_generation_simulations[[1]][[1]][[1]][[3]])
colnames(iteration_weights_sim_1) <- tableaux[[5]]
iteration_weights_sim_1 <- as.data.frame(iteration_weights_sim_1)
plot(1:20000, iteration_weights_sim_1$WSP)
plot(1:20000, iteration_weights_sim_1$NonFinality)
plot(1:20000, iteration_weights_sim_1$Trochee)
plot(1:20000, iteration_weights_sim_1$Iamb)
plot(1:20000, iteration_weights_sim_1$`All-Ft-L`)
plot(1:20000, iteration_weights_sim_1$Rightmost)
token_step <- c(1:20000)
iteration_weights_sim_1 <- cbind(iteration_weights_sim_1, token_step)

### Some plots for section 5.3.2.1

#iteration_weights_plot <- ggplot(data = iteration_weights_sim_1, aes(x = token_step)) + geom_line(aes(y=WSP), color="blue") + geom_line(aes(y=NonFinality), color="darkred") + geom_line(aes(y=Trochee), color="yellow") +
#  geom_line(aes(y=`All-Ft-L`), color="green") + geom_line(aes(y=Rightmost), color="purple") + ggtitle("Change in Weights")+ xlab("Token") + ylab("Constraint Weight")

iteration_weights_plot <- ggplot(data = iteration_weights_sim_1, aes(x = token_step)) + geom_line(aes(y=WSP, color="WSP")) + geom_line(aes(y=NonFinality, color="NonFinality")) + geom_line(aes(y=Trochee, color="Trochee")) +
  geom_line(aes(y=`All-Ft-L`, color="All-Ft-L")) + geom_line(aes(y=Rightmost, color="Rightmost")) + ggtitle("Change in Constraint Weights") + labs(x="Token", y="Constraint Weight", color="Constraint") #xlab("Token") + ylab("Constraint Weight")

gen_1c_SSEs <-  big_test_SSEs_7_001[[4]]
Generation <- c(1:50)
gen_1c_SSEs <- cbind(gen_1c_SSEs, Generation)
gen_1c_penult_plot <- ggplot(data = gen_1c_SSEs, aes(x = Generation)) + geom_line(aes(y=penult, color="penult")) + geom_line(aes(y=DTS_R_FSM, color="DTS_R_FSM")) + labs(x="Generation", y="SSE", color="Stress Pattern") #xlab("Generation") + ylab("SSE") + gglegend("Pattern")
#ggtitle("SSE in Simulation 1.c for QI Penult and DTS-R-FSM Patterns")

# 100 generation simulation: do tonight!
big_test_SSEs_100_gens_001 <- simple_learning_loop_no_lexical(number_iterations = 20000, learning_rate = 0.01, number_generations = 100)


ten_fifty_generation_simulations_2 <- list()
sim_quantity <- 10
for(sim in 1:sim_quantity){
  ten_fifty_generation_simulations_2[[sim]] <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50)
}

# Check out_fifty_X for simulations with 20000 tokens per generation


# Now you need a way of coding the capture of results
# Use tables rather than lists
# Pack dataframes in a list: one dataframe with weights, 
# then others with probabilities of interesting 5-syllable forms


# General conclusion from initial low weights: no coherent behavior across generations -- 
# the data is too heterogenous overall
# Or, the unrestricted model is just able to frequency-match too well
# There is no long-term trend whereby candidates of particular sorts become more frequent



#ex

#list_positions <- c(3, 5, 7, 8, 10, 14)

get_position_probabilities <- function(learning_output, tableau_number = 1){
  candidates_no_ft_structure <- gsub("[\\(\\)]", "", names(learning_output[[1]][[tableau_number]]))
  learning_output_temp <- learning_output
  names(learning_output_temp[[1]][[tableau_number]]) <- candidates_no_ft_structure
  candidates_no_ft_structure <- unique(candidates_no_ft_structure)
  surface_output_probabilities <- c()
  for(candidate in candidates_no_ft_structure){
    summed_probs <- sum(learning_output_temp[[1]][[tableau_number]][which(names(learning_output_temp[[1]][[tableau_number]]) == candidate)])
    surface_output_probabilities <- append(surface_output_probabilities, summed_probs)
  }
  names(surface_output_probabilities) <- candidates_no_ft_structure
  return(surface_output_probabilities)
}



#c(324, 161, 213)/698


### Simulation 2: All markedness high
learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 10
learner_violations <- tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.

## Initial 50
markedness_high_5_SSE_1 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 1, initial_weights = learner_weights)
markedness_high_5_SSE_2 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 1, initial_weights = learner_weights)
markedness_high_5_SSE_3 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 1, initial_weights = learner_weights)
markedness_high_5_SSE_4 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 1, initial_weights = learner_weights)
markedness_high_5_SSE_5 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 1, initial_weights = learner_weights)
markedness_high_5_gen_1_weights <- as.data.frame(rbind(markedness_high_5_SSE_1[[2]], markedness_high_5_SSE_2[[2]], markedness_high_5_SSE_3[[2]],markedness_high_5_SSE_4[[2]], markedness_high_5_SSE_5[[2]]))
markedness_high_5_gen_1_weights <- as.data.frame(t(markedness_high_5_gen_1_weights))
markedness_high_5_gen_1_weights <- cbind(markedness_high_5_gen_1_weights, apply(X = as.matrix(markedness_high_5_gen_1_weights), 1, FUN=mean))
xtable(markedness_high_5_gen_1_weights)

markedness_high_5_gen_1_SSEs <- as.data.frame(rbind(markedness_high_5_SSE_1[[4]], markedness_high_5_SSE_2[[4]], markedness_high_5_SSE_3[[4]],markedness_high_5_SSE_4[[4]], markedness_high_5_SSE_5[[4]]))
markedness_high_5_gen_1_SSEs <- as.data.frame(t(markedness_high_5_gen_1_SSEs))
xtable(markedness_high_5_gen_1_SSEs)

### Simulation 2, 10 50 Generation Simulations
markedness_high_5_SSE_50_1 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # This is DTS-L with final syllable extrametricality except in disyllables
markedness_high_5_SSE_50_2 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # This is non-convergent, but similar to a DTO-L with final syllable extrametricality, so SSE is most similar to QI initial
markedness_high_5_SSE_50_3 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights)
markedness_high_5_SSE_50_4 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) #2.f. First towards right trochees, then right iambs 
markedness_high_5_SSE_50_5 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # 2.g. converged on QI initial stress in generation 5 and remained stably there
markedness_high_5_SSE_50_6_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # 2a. In the range of QI initial or DTS-L-3sFSM patterns 
markedness_high_5_SSE_50_7_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) #2b. converging towards QI Penult
markedness_high_5_SSE_50_8_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # 2c.
markedness_high_5_SSE_50_9_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # 2d. 
markedness_high_5_SSE_50_10_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # 2e.
markedness_high_5_SSE_50_11 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # 2.h. 
markedness_high_5_SSE_50_12 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # 2.i.
markedness_high_5_SSE_50_13 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # 2.j.


# Figures with SSEs for QI Penult, DTS-R-FSM, DTO-R-FSM, C. Lat. rule, C. Skt. rule in simulations 2b and 2e
gen_2b_SSEs <-  markedness_high_5_SSE_50_7_001[[4]]
Generation <- c(1:50)
gen_2b_SSEs <- cbind(gen_2b_SSEs, Generation)
gen_2b_SSE_plot <- ggplot(data = gen_2b_SSEs, aes(x = Generation)) + geom_line(aes(y=penult, color="penult")) + geom_line(aes(y=DTS_R_FSM, color="DTS_R_FSM")) + geom_line(aes(y=DTO_R_FSM, color="DTO_R_FSM")) +
  geom_line(aes(y=C_Lat_Rule, color="C_Lat_Rule")) + geom_line(aes(y=C_Skt_Rule, color="C_Skt_Rule")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(gen_2b_SSE_plot)

gen_2e_SSEs <-  markedness_high_5_SSE_50_10_001[[4]]
Generation <- c(1:50)
gen_2e_SSEs <- cbind(gen_2e_SSEs, Generation)
gen_2e_SSE_plot <- ggplot(data = gen_2e_SSEs, aes(x = Generation)) + geom_line(aes(y=penult, color="penult")) + geom_line(aes(y=DTS_R_FSM, color="DTS_R_FSM")) + geom_line(aes(y=DTO_R_FSM, color="DTO_R_FSM")) +
  geom_line(aes(y=C_Lat_Rule, color="C_Lat_Rule"))+ geom_line(aes(y=C_Skt_Rule, color="C_Skt_Rule")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(gen_2e_SSE_plot)

gen_2a_SSEs <-  markedness_high_5_SSE_50_6_001[[4]]
Generation <- c(1:50)
gen_2a_SSEs <- cbind(gen_2a_SSEs, Generation)
colnames(gen_2a_SSEs)[16] <- "DTS_L_3sFSM"
gen_2a_SSE_plot <- ggplot(data = gen_2a_SSEs, aes(x = Generation)) + geom_line(aes(y=penult, color="penult")) + geom_line(aes(y=DTS_R_FSM, color="DTS_R_FSM")) + geom_line(aes(y=initial, color="initial")) +
  geom_line(aes(y=DTS_L_3sFSM, color="DTS_L_3sFSM")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(gen_2a_SSE_plot)


# One simulation for 100 generations at 001
markedness_high_5_SSE_100_gen_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 100, initial_weights = learner_weights) # In the range of QI initial or DTS-L-3sFSM patterns 
markedness_high_5_SSE_100_gen_001_V2
markedness_high_5_SSE_100_gen_001_V2

# More data per generation
markedness_high_5_SSE_30_gen_001_20000_tokens <- simple_learning_loop_no_lexical(number_iterations = 20000, learning_rate = 0.01, number_generations = 30, initial_weights = learner_weights) # In the range of QI initial or DTS-L-3sFSM patterns 


# 100 Simulations with η = 0.1; run before bed tonight
collector_100_simulations <- list()
best_SSE_100_simulations <- c()
number_simulations <- 100
for(i in 93:number_simulations){
  temp_simulation <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights)
  collector_100_simulations[[i]] <- temp_simulation
  temp_best_SSE <- names(sort(temp_simulation[[4]][50, ], decreasing =F))[1]
  best_SSE_100_simulations <- append(best_SSE_100_simulations, temp_best_SSE)
}
collector_100_simulations_copy <- collector_100_simulations
best_SSE_100_simulations_copy <- best_SSE_100_simulations

SSE_markedness_high_50_01_frame_list <- list()
best_overall_name_vector <- c()
best_overall_value_vector <- c()
for(i in 1:100){ # get best at 50, best at 1, best at any point
  temp_best_overall_index <- which(collector_100_simulations[[i]][[4]] == min(collector_100_simulations[[i]][[4]]), arr.ind = TRUE)
  best_overall_value <- collector_100_simulations[[i]][[4]][temp_best_overall_index[1], temp_best_overall_index[2]]
  best_overall_name <- colnames(collector_100_simulations[[i]][[4]])[temp_best_overall_index[2]]
  SSE_markedness_high_50_01_frame_list[[i]] <- cbind(best_SSE_100_simulations[i], min(collector_100_simulations[[i]][[4]][50, ]), names(sort(collector_100_simulations[[i]][[4]][1, ], decreasing = F))[1], min(collector_100_simulations[[i]][[4]][1, ]), best_overall_name, best_overall_value)  
  best_overall_name_vector <- append(best_overall_name_vector, best_overall_name)
  best_overall_value_vector <- append(best_overall_value_vector, best_overall_value)
}
minimum_end <- c() 
for(i in 1:100){
  minimum_end <- append(minimum_end, min(collector_100_simulations[[i]][[4]][50, ]))
}


for(i in 1:100){
  if(i == 1){
    complete_SSE_markedness_high_frame <- rbind(SSE_markedness_high_50_01_frame_list[[i]])
  }
  else{
    complete_SSE_markedness_high_frame <- rbind(complete_SSE_markedness_high_frame, SSE_markedness_high_50_01_frame_list[[i]])
  }
}

best_equal_gen_50 <- 0
best_gen_1_best_gen_50 <- 0
best_gen_1_best_overall <- 0
for(i in 1:100){
  if(complete_SSE_markedness_high_frame$V1[i] == complete_SSE_markedness_high_frame$best_overall_name[i]){
    best_equal_gen_50 <- best_equal_gen_50+1
  }
  if(complete_SSE_markedness_high_frame$V1[i] == complete_SSE_markedness_high_frame$V3[i]){
    best_gen_1_best_gen_50 <- best_gen_1_best_gen_50+1
  }
  if(complete_SSE_markedness_high_frame$V3[i] ==  complete_SSE_markedness_high_frame$best_overall_name[i]){
    best_gen_1_best_overall <- best_gen_1_best_overall+1
  }
}


# If there is a sort of categoricity bias included, then we might more often obtain convergence
# A categoricity bias could be modeled by rounding the probability of the highest probability overt form up to the nearest tenth,
# or maybe simply adding 5% of its likelihood to its likelihood, and subtracting the same amount from the likelihood of all other overt forms
# In exactly half of all simulations, the best performing grammar in gen 50 had an SSE of less than 50. In some instances, 

# cor



out_mark_10 <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 1, initial_weights = learner_weights)
out_mark_10_5_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 5, initial_weights = learner_weights)
out_mark_10_10_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 10, initial_weights = learner_weights)
out_mark_10_20_gen <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 20, initial_weights = learner_weights)

out_mark_10_50_gen_1 <- simple_learning_loop_no_lexical(number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)


## Simulation 3: specific biases
learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 5

# The constraints that  should be biased are NonFin-σ (4), NonFin-ϝ (5), WSP (7), and trochee (8)
learner_weights[c(4,5,7,8)] <- 10 # set the weights of the biased constraints to 10

# need to include a modified update rule for slower learning rate with biased constraints.
biased_rates <- rep(0.1, ncol(tableaux[[2]][[1]]))
biased_rates[c(4,5,7,8)] <- 0.05

update_rule_biased_constraints <- function(teacher_winner, learner_winner, rate, rates = learning_rate, current_weights){
  #rates <- biased_rates_rate # this applies the biased rates set in the global environment
  change_in_weights <- rates*(learner_winner - teacher_winner) # learner-teacher or teacher-learner -- I think the former for positive weights; cf. Jarosz 2016: 204
  #negatives <- which(change_in_weights < 0)
  #change_in_weights[negatives] <- 0
  learner_weights <- current_weights + change_in_weights
  negatives <- which(learner_weights < 0) # GLA allows negative weights, I think
  learner_weights[negatives] <- 0
  return(learner_weights)
}

### Here include a version of the learning loop using the "update_rule_biased_constraints" update rule
simple_learning_loop_no_lexical_biased <- function(learning_rate = biased_rates, number_iterations = 5000, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_biased_constraints(teacher_winner, learner_winner, rates = learning_rate, current_weights = learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilties and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to obtain a prosodic shape
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        #print("teacher winner number ok!")
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_biased_constraints(teacher_winner, learner_winner, rates = learning_rate, current_weights = learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c()
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- rbind(total_SSE_results, current_SSE_results)
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains four items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; a dataframe with the SSEs for some possible stress patterns
    return(final_generation_results)
  }
}


learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 5
# The constraints that  should be biased are NonFin-σ (4), NonFin-ϝ (5), WSP (7), and trochee (8)
learner_weights[c(4,5,7,8)] <- 10 # set the weights of the biased constraints to 10

# need to include a modified update rule for slower learning rate with biased constraints.
biased_rates <- rep(0.1, ncol(tableaux[[2]][[1]]))
biased_rates[c(4,5,7,8)] <- 0.05
l
biased_five_single_generations <- list()
for(i in 1:5){
  biased_five_single_generations[[i]] <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 1, initial_weights = learner_weights)
}
biased_five_single_generations_weights <- rbind(biased_five_single_generations[[1]][[2]], biased_five_single_generations[[2]][[2]], biased_five_single_generations[[3]][[2]], biased_five_single_generations[[4]][[2]], biased_five_single_generations[[5]][[2]])
biased_five_single_generations_SSEs <- rbind(biased_five_single_generations[[1]][[4]], biased_five_single_generations[[2]][[4]], biased_five_single_generations[[3]][[4]], biased_five_single_generations[[4]][[4]], biased_five_single_generations[[5]][[4]])


# here with learning rate of biased constraints down to 0.01
biased_five_single_generations_V2 <- list()
for(i in 1:5){
  biased_five_single_generations_V2[[i]] <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 1, initial_weights = learner_weights)
}

biased_markedness_high_5_SSE_1_50_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # This is DTS-L with final syllable extrametricality except in disyllables
biased_markedness_high_5_SSE_2_50_001 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # This is non-convergent, but similar to a DTO-L with final syllable extrametricality, so SSE is most similar to QI initial
biased_markedness_high_5_SSE_3_50_001  <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights)
biased_markedness_high_5_SSE_4_50_001  <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) #2.f. First towards right trochees, then right iambs 
biased_markedness_high_5_SSE_5_50_001  <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.01, number_generations = 50, initial_weights = learner_weights) # 2.g. converged on QI initial stress in generation 5 and remained stably there
biased_markedness_high_5_SSE_50_6_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # In the range of QI initial or DTS-L-3sFSM patterns 
biased_markedness_high_5_SSE_50_7_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights) # converging towards QI Penult
biased_markedness_high_5_SSE_50_8_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights)
biased_markedness_high_5_SSE_50_9_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights)
biased_markedness_high_5_SSE_50_10_01 <- simple_learning_loop_no_lexical(number_iterations = 10000, learning_rate = 0.1, number_generations = 50, initial_weights = learner_weights)
# maybe just do 10 simulations with 0.1?

biased_10_sim_50_generations <- list()
for(i in 1:10){
  biased_10_sim_50_generations[[i]] <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)
}

for(i in 1:10){
  if(i == 1){
    biased_generation_50_SSEs_10 <- rbind(biased_10_sim_50_generations[[i]][[4]][50, ])
  }
  else{
    biased_generation_50_SSEs_10 <- rbind(biased_generation_50_SSEs_10, biased_10_sim_50_generations[[i]][[4]][50, ])
  }
}
View(biased_generation_50_SSEs_10)


# Another round with slower learning rate
learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 5
# The constraints that  should be biased are NonFin-σ (4), NonFin-ϝ (5), WSP (7), and trochee (8)
learner_weights[c(4,5,7,8)] <- 10 # set the weights of the biased constraints to 10

# need to include a modified update rule for slower learning rate with biased constraints.
biased_rates <- rep(0.01, ncol(tableaux[[2]][[1]]))
biased_rates[c(4,5,7,8)] <- 0.005



biased_10_sim_50_generations_001 <- list()
for(i in 1:10){
  biased_10_sim_50_generations_001[[i]] <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)
}

for(i in 1:10){
  if(i == 1){
    biased_generation_50_001_SSEs_10 <- rbind(biased_10_sim_50_generations_001[[i]][[4]][50, ])
  }
  else{
    biased_generation_50_001_SSEs_10 <- rbind(biased_generation_50_001_SSEs_10, biased_10_sim_50_generations_001[[i]][[4]][50, ])
  }
}
View(biased_generation_50_001_SSEs_10)

gen_3q_SSEs <-  biased_10_sim_50_generations_001[[7]][[4]]
Generation <- c(1:50)
gen_3q_SSEs <- cbind(gen_3q_SSEs, Generation)
colnames(gen_3q_SSEs)[16] <- "DTS_L_3sFSM"
gen_3q_SSE_plot <- ggplot(data = gen_3q_SSEs, aes(x = Generation)) + geom_line(aes(y=C_Skt_Rule, color="C_Skt_Rule")) + geom_line(aes(y=DTS_R_FSM, color="DTS_R_FSM")) + geom_line(aes(y=DTS_L, color="DTS_L")) +
  geom_line(aes(y=DTS_L_3sFSM, color="DTS_L_3sFSM")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(gen_3q_SSE_plot)

biased_1_sim_100_generations_001 <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)


learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 5
# The constraints that  should be biased are NonFin-σ (4), NonFin-ϝ (5), WSP (7), and trochee (8)
learner_weights[c(4,5,7,8)] <- 10 # set the weights of the biased constraints to 10

# need to include a modified update rule for slower learning rate with biased constraints.
biased_rates <- rep(0.1, ncol(tableaux[[2]][[1]]))
biased_rates[c(4,5,7,8)] <- 0.05

biased_collector_100_simulations <- list()
biased_best_SSE_100_simulations <- c()
number_simulations <- 100
for(i in 90:100){
  temp_simulation <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)
  biased_collector_100_simulations[[i]] <- temp_simulation
  temp_best_SSE <- names(sort(temp_simulation[[4]][50, ], decreasing =F))[1]
  biased_best_SSE_100_simulations <- append(biased_best_SSE_100_simulations, temp_best_SSE)
}


biased_best_SSE_overall_100_simulations <- c()
for(i in 1:100){
  temp_index <- which(biased_collector_100_simulations[[i]][[4]] == min(biased_collector_100_simulations[[i]][[4]]), arr.ind = T)
  temp_name <- colnames(biased_collector_100_simulations[[i]][[4]])[temp_index[2]]
  biased_best_SSE_overall_100_simulations <- append(biased_best_SSE_overall_100_simulations, temp_name)
}

biased_min_SSE_gen_50 <- c()
for(i in 1:100){
  biased_min_SSE_gen_50 <- append(biased_min_SSE_gen_50, min(biased_collector_100_simulations[[i]][[4]][50, ]))
}

biased_best_gen_50_frame <- as.data.frame(cbind(biased_best_SSE_100_simulations, biased_min_SSE_gen_50))

biased_min_SSE_overall <- c()
for(i in 1:100){
  biased_min_SSE_overall <- append(biased_min_SSE_overall, min(biased_collector_100_simulations[[i]][[4]]))
}

biased_best_all_frame <- as.data.frame(cbind(biased_best_SSE_100_simulations, biased_min_SSE_gen_50, biased_best_SSE_overall_100_simulations, biased_min_SSE_overall))

biased_collector_10_simulations_2500_tokens <- list()
biased_best_SSE_gen_50_10_simulations_2500_tokens <- c()
biased_min_SSE_gen_50_10_simulations_2500_tokens <- c()
biased_best_SSE_overall_10_simulations_2500_tokens <- c()
biased_min_best_SSE_overall_10_simulations_2500_tokens <- c()
for(i in 30:1){
  temp_simulation <- simple_learning_loop_no_lexical_biased(learning_rate = biased_rates, number_iterations = 2500, number_generations = 50, initial_weights = learner_weights)
  biased_collector_10_simulations_2500_tokens[[i]] <- temp_simulation
  temp_best_SSE <- names(sort(temp_simulation[[4]][50, ], decreasing =F))[1]
  biased_best_SSE_gen_50_10_simulations_2500_tokens <- append(biased_best_SSE_gen_50_10_simulations_2500_tokens, temp_best_SSE)
  biased_min_SSE_gen_50_10_simulations_2500_tokens <- append(biased_min_SSE_gen_50_10_simulations_2500_tokens, min(temp_simulation[[4]][50, ]))
  temp_best_overall_SSE <- colnames(temp_simulation[[4]])[which(temp_simulation[[4]] ==  min(temp_simulation[[4]]), arr.ind=TRUE)[2]]
  biased_best_SSE_overall_10_simulations_2500_tokens <- append(biased_best_SSE_overall_10_simulations_2500_tokens, temp_best_overall_SSE)
  biased_min_best_SSE_overall_10_simulations_2500_tokens <- append(biased_min_best_SSE_overall_10_simulations_2500_tokens, temp_simulation[[4]][which(temp_simulation[[4]] ==  min(temp_simulation[[4]]), arr.ind=TRUE)])
  #if(i == i){
  biased_best_frame_2500_tokens <- as.data.frame(cbind(biased_best_SSE_gen_50_10_simulations_2500_tokens, biased_min_SSE_gen_50_10_simulations_2500_tokens, biased_best_SSE_overall_10_simulations_2500_tokens, biased_min_best_SSE_overall_10_simulations_2500_tokens))
  #}
}

### The type frequency learner
## First, getting the type frequencies of the Vedic data.
## Creating a table with the type frequencies


all_prosodic_shapes_types_frame <- read.table("RV_All_prosodic_shapes_types.txt", header=T, sep="\t")

# Restrict data to 2-5 syllable words
#limited_2_3_4_5_indices <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", all_prosodic_shapes_frame$all_prosodic_shapes) # extract the row numbers of 2-5 syllable words
limited_2_3_4_5_indices_types <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique)
# Sample a token from the probability distribution. Test scripts for sampling to include in function below 
#sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
# by type frequencies; for using the type frequencies in simulations, you just have to plug this line into the learning loop function where tokens are sampled
sampled_item <- sample(all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique[limited_2_3_4_5_indices_types], 1, prob = (all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types]/sum(all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types])))

## Here is the modified loop for biased frequencies
simple_learning_loop_no_lexical_biased_type_freq <- function(learning_rate = biased_rates, number_iterations = 5000, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique[limited_2_3_4_5_indices_types], 1, prob = (all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types]/sum(all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types]))) # sample an item from the freqs of token freqs
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_biased_constraints(teacher_winner, learner_winner, rates = learning_rate, current_weights = learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilties and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to obtain a prosodic shape
      sampled_item <- sample(all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique[limited_2_3_4_5_indices_types], 1, prob = (all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types]/sum(all_prosodic_shapes_types_frame$Freq[limited_2_3_4_5_indices_types])))
      
      #sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        #print("teacher winner number ok!")
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_biased_constraints(teacher_winner, learner_winner, rates = learning_rate, current_weights = learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # sometimes there are 502, sometimes 503, items in current_generation_output_probabilities
    if(length(current_generation_output_probabilities) == 502){
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    }
    else{
      stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    }
    current_SSE_results <- c()
    for(i in 3:ncol(stress_patterns_for_SSE)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_copy[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- rbind(total_SSE_results, current_SSE_results)
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE[, 3:ncol(stress_patterns_for_SSE)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_types_frame$Freq[which(all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains four items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; a dataframe with the SSEs for some possible stress patterns
    return(final_generation_results)
  }
}

learner_weights <- rep(5 ,ncol(tableaux[[2]][[1]])) # set all weights of all (markedness) constraints to 5
# The constraints that  should be biased are NonFin-σ (4), NonFin-ϝ (5), WSP (7), and trochee (8)
learner_weights[c(4,5,7,8)] <- 10 # set the weights of the biased constraints to 10
learner_violations <- tableaux[[2]]

# need to include a modified update rule for slower learning rate with biased constraints.
biased_rates <- rep(0.1, ncol(tableaux[[2]][[1]]))
biased_rates[c(4,5,7,8)] <- 0.05


type_frequency_learning_1_gen <-  simple_learning_loop_no_lexical_biased_type_freq(learning_rate = biased_rates, number_iterations = 10000, number_generations = 1, initial_weights = learner_weights)


type_collector_100_simulations <- list()
type_best_SSE_gen_50_100_simulations <- c()
type_min_SSE_gen_50_100_simulations <- c()
type_best_SSE_overall_100_simulations <- c()
type_min_best_SSE_overall_100_simulations <- c()
for(i in 89:100){
  temp_simulation <- simple_learning_loop_no_lexical_biased_type_freq(learning_rate = biased_rates, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights)
  type_collector_100_simulations[[i]] <- temp_simulation
  temp_best_SSE <- names(sort(temp_simulation[[4]][50, ], decreasing =F))[1]
  type_best_SSE_gen_50_100_simulations <- append(type_best_SSE_gen_50_100_simulations, temp_best_SSE)
  type_min_SSE_gen_50_100_simulations <- append(type_min_SSE_gen_50_100_simulations, min(temp_simulation[[4]][50, ]))
  temp_best_overall_SSE <- colnames(temp_simulation[[4]])[which(temp_simulation[[4]] ==  min(temp_simulation[[4]]), arr.ind=TRUE)[2]]
  type_best_SSE_overall_100_simulations <- append(type_best_SSE_overall_100_simulations, temp_best_overall_SSE)
  type_min_best_SSE_overall_100_simulations <- append(type_min_best_SSE_overall_100_simulations, temp_simulation[[4]][which(temp_simulation[[4]] ==  min(temp_simulation[[4]]), arr.ind=TRUE)])
  #if(i == i){
  type_best_frame_100_simulations <- as.data.frame(cbind(type_best_SSE_gen_50_100_simulations, type_min_SSE_gen_50_100_simulations, type_best_SSE_overall_100_simulations, type_min_best_SSE_overall_100_simulations))
  #}
}


types_prosodic_shapes_2_5_syll <- all_prosodic_shapes_types_frame$all_prosodic_shapes_stressed_rv_unique[limited_2_3_4_5_indices_types]
for(i in 1:length(types_prosodic_shapes_2_5_syll))
  
  
  ## Simulations with MAX-PROM: Lexical Stress is in!
  # This assumes, on the teacher's side, that the mapping of an overt form with primary stress on a given syllable to a parsed output with
  # primary stress on a different syllable is disallowed. That is, mappings like [H1 L V] --> H (L1 V) are not included whatsoever. In other
  # words, the data is always taken seriously: an overt form with stress in a given position must map onto a parse in that position
  # Nonetheless, learners are permitted to consider the possibility that a lexical representation /H1 L V/ could have an output H (L1 V), if the 
  # markedness constraints militating in favor of the latter are strong enough and lexical stress is sufficiently weak. 
  
  
  # identifying all 1-, 6-, 7- and 8-syllable forms and removing them
# you created an object called "all_prosodic_shapes_stressed_rv", which is a named vector of characters with length 129459
#rv_overt_2_to_5_syllable_forms <- c()

to_remove_short_long <- c()
for(i in 1:length(all_prosodic_shapes_stressed_rv)){
  token_length <- length(unlist(strsplit(all_prosodic_shapes_stressed_rv[[i]], split=" ")))
  if((token_length == 1) | (token_length > 5)){
    to_remove_short_long <- append(to_remove_short_long, i)
  }
}
rv_overt_2_to_5_syllable_forms <- all_prosodic_shapes_stressed_rv[-c(to_remove_short_long)]

# set up a base learner lexicon with all types, all originally having a frequency of 0
rv_overt_2_to_5_syllable_forms_types_freq_table <- as.data.frame(sort(table(names(rv_overt_2_to_5_syllable_forms)), decreasing=T))
colnames(rv_overt_2_to_5_syllable_forms_types_freq_table) <- c("Type", "Token Freq")
# the number of 2- to 5-syllable forms sums to 108488
initial_learner_lexicon <- rv_overt_2_to_5_syllable_forms_types_freq_table
initial_learner_lexicon$`Token Freq` <- rep(0, nrow(initial_learner_lexicon))

# you need to b able to reverse engineer a surface stress for a word-type based on outputs
# That is, if the token is supposed to be aBi;, but the teacher after the previous generation of learning would put out a;Bi,  


current_token <- sample(rv_overt_2_to_5_syllable_forms, 1) # sample a really occurring stressed surface toke


#current_token_shape <- parse_prosodic_shape(current_token)  # parse it into an abstract prosodic shape
#split_token_shape <- strsplit(current_token_shape, " ")

### Simulation 1: Overwhelming MAX-PROM effect, different learning rates for MAX-PROM
## Here, no reference to token frequency is ever made
## Every teacher winner does not violate Max-Prom, every learner loser does
## Hence, it is to be expected that the weight of MAX-Prom rises with every weight update, and will do so until candidates violating Max-Prom become impossible

## Here the function for the learning loop
## The function update_rule needs to be adjusted so that the learning rate for MAX-PROM is potentially different
update_rule_MAX_PROM <- function(teacher_winner, learner_winner, rate, max_prom_rate, learner_weights){
  rates <- c(rep(rate, ncol(vedic_lexical_tableaux[[2]][[1]])-1), max_prom_rate) # this assumes that MAX-PROM is the rightmost constraint in the tableaux
  change_in_weights <- rates*(learner_winner - teacher_winner) # learner-teacher or teacher-learner -- I think the former for positive weights; cf. Jarosz 2016: 204
  #negatives <- which(change_in_weights < 0)
  #change_in_weights[negatives] <- 0
  learner_weights <- learner_weights + change_in_weights
  negatives <- which(learner_weights < 0) # GLA allows negative weights, I think
  learner_weights[negatives] <- 0
  return(learner_weights)
}

simple_learning_loop_lexical_1 <- function(tableaux = my_lexical_tableaux, number_iterations = 5000, learning_rate = 0.01, learning_rate_max_prom = 0.005, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_MAX_PROM(teacher_winner, learner_winner, rate = learning_rate, max_prom_rate = learning_rate_max_prom, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilties and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to obtain a prosodic shape
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        #print("teacher winner number ok!")
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_MAX_PROM(teacher_winner, learner_winner, rate = learning_rate, max_prom_rate = learning_rate_max_prom, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(total_SSE_results, current_SSE_results))
    #colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains three items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; SSEs of categorical grammars
    return(final_generation_results)
  }
}
## Set initial learner weights, learner candidate probs
vedic_lexical_tableaux <- prepare_data("C_Skt_RIP_Lexical_Stress_corrected.txt")
learner_weights <- c(rep(5, ncol(vedic_lexical_tableaux[[2]][[1]])-1), 0) # set all weights of all (markedness) constraints to 0
learner_violations <- vedic_lexical_tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(lexical_learner_violations, lexical_learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.

# Seeing what behavior results if MAX-PROM is then simply removed from the grammar
temp_weights <- lexical_out_1_gen_1[[2]]
temp_weights <- temp_weights[-c(18)]
temp_weights <- as.numeric(temp_weights)
tableux <- prepare_data("C_Skt_for_RIP_corrected_3.txt")

temp_learner_violations <- tableaux[[2]]
temp_learner_cand_probs <- initial_learner_candidate_probs(temp_learner_violations, temp_weights)



# WORKING HERE
# Test setting the markedness constraints at differed weights, test different learning rates for MAX-PROM
lexical_out_1_gen_1 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 1, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
iteration_weights_sim_lexical_1 <- do.call(rbind.data.frame, lexical_out_1_gen_1[[1]][[1]][[3]])
colnames(iteration_weights_sim_lexical_1) <- vedic_lexical_tableaux[[5]]

colnames(iteration_weights_sim_lexical_1)[10] <- "All-Ft-L"
colnames(iteration_weights_sim_lexical_1)[17] <- "*CLASH"
colnames(iteration_weights_sim_lexical_1)[18] <- "MAX-PROM"

token_step <- c(1:10000)
iteration_weights_sim_lexical_1 <- cbind(iteration_weights_sim_lexical_1, token_step)

iteration_weights_lexical_plot <- ggplot(data = iteration_weights_sim_lexical_1, aes(x = token_step)) + geom_line(aes(y=`MAX-PROM`, color="MAX-PROM")) + geom_line(aes(y=WSP, color="WSP")) + geom_line(aes(y=Trochee, color="Trochee")) +
  geom_line(aes(y=`All-Ft-L`, color="All-Ft-L")) + geom_line(aes(y=Rightmost, color="Rightmost")) + ggtitle("Change in Constraint Weights: Lexical Stress") + labs(x="Token", y="Constraint Weight", color="Constraint") #xlab("Token") + ylab("Constraint Weight")
plot(iteration_weights_lexical_plot)

lexical_out_50_gen_1_weight_5_sim_1 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
lexical_out_50_gen_1_weight_5_sim_2 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 50, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)

SSEs_4_b <- lexical_out_50_gen_1_weight_5_sim_1[[4]]
SSEs_4_b <- cbind(SSEs_4_b, Generation)
colnames(SSEs_4_b)[17] <- "Perfect Lexical"
colnames(SSEs_4_b)[1] <- "QI Initial"
colnames(SSEs_4_b)[4] <- "QI Penult"
colnames(SSEs_4_b)[13] <- "C. Skt. Rule"
SSE_plot_sim_4_b <- ggplot(data=SSEs_4_b, aes(x=Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Penult`, color="QI Penult")) +
  geom_line(aes(y=`QI Initial`, color="QI Initial")) + geom_line(aes(y=`C. Skt. Rule`, color="C. Skt. Rule")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSE_plot_sim_4_b)

lexical_out_50_gen_1_weight_5_sim_3_2500 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 2500, number_generations = 50, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
# This one is clearly getting QS right edge systems -- but is this just an illusion of the initial state? 4c
lexical_out_50_gen_1_weight_5_sim_4_5000 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 5000, number_generations = 50, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
SSEs_4_c <- lexical_out_50_gen_1_weight_5_sim_4_5000[[4]]
SSEs_4_c <- cbind(SSEs_4_c, Generation) 
colnames(SSEs_4_c)[17] <- "Perfect Lexical"
colnames(SSEs_4_c)[13] <- "C. Skt. Rule"
colnames(SSEs_4_c)[4] <- "QI Penult"
colnames(SSEs_4_c)[12] <- "C. Lat. Rule"
colnames(SSEs_4_c)[10] <- "DTS-R-FSM"

SSE_plot_sim_4_c <- ggplot(data=SSEs_4_c, aes(x=Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Penult`, color="QI Penult")) +
  geom_line(aes(y=`DTS-R-FSM`, color="DTS-R-FSM")) + geom_line(aes(y=`C. Skt. Rule`, color="C. Skt. Rule")) + geom_line(aes(y=`C. Lat. Rule`, color="C. Lat. Rule")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSE_plot_sim_4_c)

# 4d
lexical_out_100_gen_1_weight_5_sim_5_5000 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 5000, number_generations = 100, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
SSEs_4_d <- lexical_out_100_gen_1_weight_5_sim_5_5000[[4]]
Generation <- c(1:100)
SSEs_4_d <- cbind(SSEs_4_d, Generation)
colnames(SSEs_4_d)[17] <- "Perfect Lexical"
colnames(SSEs_4_d)[1] <- "QI Initial"
colnames(SSEs_4_d)[6] <- "DTS-L"
colnames(SSEs_4_d)[14] <- "Left Iambs"
colnames(SSEs_4_d)[16] <- "DTS-L-3σ-FSM"
SSE_plot_sim_4_d <- ggplot(data=SSEs_4_d, aes(x=Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Initial`, color="QI Initial")) +
  geom_line(aes(y=`DTS-L`, color="DTS-L")) + geom_line(aes(y=`Left Iambs`, color="Left Iambs")) + geom_line(aes(y=`DTS-L-3σ-FSM`, color="DTS-L-3σ-FSM")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSE_plot_sim_4_d)



# 4e
lexical_out_200_gen_1_weight_5_sim_6_5000 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 5000, number_generations = 200, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
SSEs_4_e <- lexical_out_200_gen_1_weight_5_sim_6_5000[[4]]
Generation <- c(1:200)
SSEs_4_e <- cbind(SSEs_4_e, Generation)
colnames(SSEs_4_e)[17] <- "Perfect Lexical"
colnames(SSEs_4_e)[13] <- "C. Skt. Rule"
colnames(SSEs_4_e)[4] <- "QI Penult"
colnames(SSEs_4_e)[2] <- "QI Peninitial"
colnames(SSEs_4_e)[14] <- "Left Iambs"
SSE_plot_sim_4_e <- ggplot(data=SSEs_4_e, aes(x=Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Penult`, color="QI Penult")) +
  geom_line(aes(y=`QI Peninitial`, color="QI Peninitial")) + geom_line(aes(y=`C. Skt. Rule`, color="C. Skt. Rule")) + geom_line(aes(y=`Left Iambs`, color="Left Iambs")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSE_plot_sim_4_e)


# 10 150 gen sims # maybe run this again tonight
lexical_sim_collector_150_gens <- list()
for(i in 1:10){
  temp_sim_output <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 5000, number_generations = 150, initial_weights = learner_weights, learning_rate = 0.1, learning_rate_max_prom = 0.01)
  lexical_sim_collector_150_gens[[i]] <- temp_sim_output
  lexical_sim_collector_150_gens_copy <- lexical_sim_collector_150_gens
}


### Proto-Germanic simulations: a proportion of learning tokens is randomly given initial stress
germanic_lexical_loop <- function(tableaux = my_lexical_tableaux, number_iterations = 5000, learning_rate = 0.01, learning_rate_max_prom = 0.005, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      sampled_stress_deaf_teacher <- sample(c(1:10), 1) # sample a value between 1 and ten
      if(sampled_stress_deaf_teacher < 3){ # set values to create a proportion of initial stress in the data
        sampled_item_stressless <- gsub("1", "", sampled_item)
        sampled_item_initial_stress <- gsub("^([HL])", "\\11", sampled_item_stressless)
        sampled_item <- sampled_item_initial_stress
      }
      sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled candidate
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_MAX_PROM(teacher_winner, learner_winner, rate = learning_rate, max_prom_rate = learning_rate_max_prom, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilties and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to obtain a prosodic shape
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_stress_deaf_teacher <- sample(c(1:10), 1) # sample a value between 1 and ten
      if(sampled_stress_deaf_teacher < 3){ # set values to create a proportion of initial stress in the data
        sampled_item_stressless <- gsub("1", "", sampled_item)
        sampled_item_initial_stress <- gsub("^([HL])", "\\11", sampled_item_stressless)
        sampled_item <- sampled_item_initial_stress
      }      
      sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
      learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        #print("teacher winner number ok!")
        teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_MAX_PROM(teacher_winner, learner_winner, rate = learning_rate, max_prom_rate = learning_rate_max_prom, learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(total_SSE_results, current_SSE_results))
    #colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains three items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; SSEs of categorical grammars
    return(final_generation_results)
  }
}

vedic_lexical_tableaux <- prepare_data("C_Skt_RIP_Lexical_Stress_corrected.txt")
learner_weights <- c(rep(5, ncol(vedic_lexical_tableaux[[2]][[1]])-1), 0) # set all weights of all (markedness) constraints to 0
learner_violations <- vedic_lexical_tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(lexical_learner_violations, lexical_learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.

# 10%
germanic_sim_1 <- germanic_lexical_loop(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_iterations = 5000, number_generations = 25, initial_weights = learner_weights)
germanic_collection_loop_1 <- list()
for(i in 1:10){
  germanic_sim_temp <- germanic_lexical_loop(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_iterations = 5000, number_generations = 25, initial_weights = learner_weights)
  germanic_collection_loop_1[[i]] <- germanic_sim_temp
  germanic_collection_loop_1_copy <- germanic_collection_loop_1
}
# 20%
germanic_sim_2 <- germanic_lexical_loop(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_iterations = 5000, number_generations = 50, initial_weights = learner_weights)

# 20% 3000 tokens per gen, 
germanic_sim_3 <- germanic_lexical_loop(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_iterations = 3000, number_generations = 50, initial_weights = learner_weights)

# 100 Germanic simulations, 20% change to initial stress

germanic_collector_100_simulations <- list()
germanic_best_SSE_gen_50_100_simulations <- c()
germanic_min_SSE_gen_50_100_simulations <- c()
germanic_best_SSE_overall_100_simulations <- c()
germanic_min_best_SSE_overall_100_simulations <- c()
for(i in 1:100){
  germanic_simulation <- germanic_lexical_loop(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_iterations = 3000, number_generations = 50, initial_weights = learner_weights)
  germanic_collector_100_simulations[[i]] <- germanic_simulation
  germanic_best_SSE <- names(sort(germanic_simulation[[4]][50, ], decreasing =F))[1]
  germanic_best_SSE_gen_50_100_simulations <- append(germanic_best_SSE_gen_50_100_simulations, germanic_best_SSE)
  germanic_min_SSE_gen_50_100_simulations <- append(germanic_min_SSE_gen_50_100_simulations, min(germanic_simulation[[4]][50, ]))
  germanic_best_overall_SSE <- colnames(germanic_simulation[[4]])[which(germanic_simulation[[4]] ==  min(germanic_simulation[[4]]), arr.ind=TRUE)[2]]
  germanic_best_SSE_overall_100_simulations <- append(germanic_best_SSE_overall_100_simulations, germanic_best_overall_SSE)
  germanic_min_best_SSE_overall_100_simulations <- append(germanic_min_best_SSE_overall_100_simulations, germanic_simulation[[4]][which(germanic_simulation[[4]] ==  min(germanic_simulation[[4]]), arr.ind=TRUE)])
  print(i)
  #if(i == i){
  germanic_best_frame_100_simulations <- as.data.frame(cbind(germanic_best_SSE_gen_50_100_simulations, germanic_min_SSE_gen_50_100_simulations, germanic_best_SSE_overall_100_simulations, germanic_min_best_SSE_overall_100_simulations))
  #}
}

SSEs_germ_sim_1_c <- germanic_collector_100_simulations[[3]][[4]]


Generation <- c(1:50)
SSEs_germ_sim_1_c <- cbind(SSEs_germ_sim_1_c, Generation)
colnames(SSEs_germ_sim_1_c)[17] <- "Perfect Lexical"
colnames(SSEs_germ_sim_1_c)[1] <- "QI Initial"
colnames(SSEs_germ_sim_1_c)[6] <- "DTS-L"
SSE_plot_germ_sim_1_c <- ggplot(data=SSEs_germ_sim_1_c, aes(x=Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Initial`, color="QI Initial")) +
  geom_line(aes(y=`DTS-L`, color="DTS-L")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSE_plot_germ_sim_1_c)


#lexical_out_50_gen_1_weight_10 <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 15000, number_generations = 50)

### Simulation 2: Token frequency effects with Max-Prom
# Violations of MAX-Prom are multiplied by the log of a lexeme's token frequency
# individual lexical items can be restructured
# learning rate for MAX-Prom is set at the default for a token frequency of 1 or 2, otherwise divided by the log of a lexeme's token frequency
# for this, you will need to make some adjustments to 
#rv_overt_2_to_5_syllable_forms_types_freq_table # contains the token frequencies of each surface type
View(initial_learner_lexicon)

# practically, you only need to do this with onset maximization
syllabify_and_stressify <- function(form){ # get syllabification and possible stress positions for all types, # then all of these are packed into a lexicon where all options exist
  destressed_form <- gsub(";", "", form)
  syllable_boundaries <- gsub(paste("(", V, "|", v, ")", ";?", "(", C, "|", V, "|", v, ")", ";?", sep=""), "\\1\\.\\2", destressed_form)
  syllable_boundaries <- gsub(paste("(", V, "|", v, ")", ";?", "(", C, "|", V, "|", v, ")", ";?", sep=""), "\\1\\.\\2", syllable_boundaries)
  syllable_split <- unlist(strsplit(syllable_boundaries, split="\\."))
  stress_options_vector <- c()
  for(syllable in 1:length(syllable_split)){
    stressed_syllable <- gsub(paste("(", V,"|",v,")", sep=""), "\\1;" ,syllable_split[syllable])
    temp_stressed_split <- syllable_split
    temp_stressed_split[syllable] <- stressed_syllable
    stressed_form <- paste(temp_stressed_split, collapse="")
    stress_options_vector <- append(stress_options_vector, stressed_form)
  }
  stress_options_vector <- grep(";", stress_options_vector, value=T) # only keep options where a stress has been added
  return(stress_options_vector)
}

length(rv_overt_2_to_5_syllable_forms_types_freq_table$Type)
complete_stress_types <- c()
#complete_individual_prosodic_shapes <- c()
for(form in 1:length(rv_overt_2_to_5_syllable_forms_types_freq_table$Type)){
  current_stress_options <- syllabify_and_stressify(rv_overt_2_to_5_syllable_forms_types_freq_table$Type[form])
  complete_stress_types <- append(complete_stress_types, current_stress_options)
  #complete_individual_prosodic_shapes <- append(complete_individual_prosodic_shapes, parse_prosodic_shape(current_stress_options))
}
# the first version of syllabify_and_stressify resulted in two ;; in words ending in v;.
complete_stress_types <- unique(complete_stress_types)
complete_segmental_shapes <- gsub(";", "", complete_stress_types)
complete_individual_prosodic_shapes <- parse_prosodic_shape(complete_stress_types)
complete_initial_learner_lexicon <- as.data.frame(cbind(complete_stress_types, complete_segmental_shapes, complete_individual_prosodic_shapes))
complete_initial_learner_lexicon <- cbind(complete_initial_learner_lexicon, rep(0, length(complete_stress_types)))
colnames(complete_initial_learner_lexicon) <- c("Type", "Segmental", "Prosodic Shape", "Frequency") 

# there are then a total of 79628 "stress types" of 2- 5-syllable words

#parse_prosodic_shape(syllabify_and_stressify(form)) # this will provide all possible stress positions for all surface word types in Vedic, and give their abstract prosodic shape

### Here is the modified version of the update rule for the token-frequency-sensitive learner
update_rule_log_token_freq <- function(teacher_winner, learner_winner, rate, general_max_prom_rate, item_token_freq, learner_weights){
  if(item_token_freq < 3){
    max_prom_rate <- general_max_prom_rate
  }
  else{
    max_prom_rate <- (general_max_prom_rate/log(item_token_freq))
  }
  rates <- c(rep(rate, ncol(vedic_lexical_tableaux[[2]][[1]])-1), max_prom_rate) # this assumes that MAX-PROM is the rightmost constraint in the tableaux
  change_in_weights <- rates*(learner_winner - teacher_winner) # learner-teacher or teacher-learner -- I think the former for positive weights; cf. Jarosz 2016: 204
  #negatives <- which(change_in_weights < 0)
  #change_in_weights[negatives] <- 0
  learner_weights <- learner_weights + change_in_weights
  negatives <- which(learner_weights < 0) # GLA allows negative weights, I think
  learner_weights[negatives] <- 0
  return(learner_weights)
}

### Below here the learning loop for the token-frequency-sensitive learner
learning_loop_lexical_freq_1 <- function(tableaux = my_lexical_tableaux, number_iterations = 5000, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  #rv_overt_2_to_5_syllable_forms_types_freq_table contains the original token frequencies of all surface word types in the RV
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
    complete_initial_learner_lexicon <- as.data.frame(cbind(complete_stress_types, complete_segmental_shapes, complete_individual_prosodic_shapes))
    complete_initial_learner_lexicon <- cbind(complete_initial_learner_lexicon, rep(0, length(complete_stress_types)))
    colnames(complete_initial_learner_lexicon) <- c("Type", "Segmental", "Prosodic Shape", "Frequency")
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights) # get initial candidate probabilities based on the prespecified initial weights
    learner_weights <- initial_weights # set the initial weights to the starting weights for the first generation of learning
    iteration_weights <- list() # keep track of the weight evolution within the learning generation
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    sampled_tokens <- c()
    trap_forms <- c() # to catch tokens that cause errors
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      sampled_item <- sample(rv_overt_2_to_5_syllable_forms_types_freq_table$Type, 1, prob = rv_overt_2_to_5_syllable_forms_types_freq_table$`Token Freq`/sum(rv_overt_2_to_5_syllable_forms_types_freq_table$`Token Freq`)) # sample an overt token
      sampled_tokens <- append(sampled_tokens, sampled_item)
      # add 1 to the token frequency of that type in the learner's lexicon
      row_in_lexicon <- which(complete_initial_learner_lexicon$Type == sampled_item) # look up the row in the lexicon
      complete_initial_learner_lexicon$Frequency[row_in_lexicon] <- (complete_initial_learner_lexicon$Frequency[row_in_lexicon] + 1) # add 1 to the Frequency column
      #sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      # convert the sampled item to an abstract prosodic shape in order to find its tableau
      sampled_item_prosodified <- parse_prosodic_shape(sampled_item)
      sampled_tableau_number <- grep(paste("/", sampled_item_prosodified, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      possible_winner_numbers <- grep(sampled_item_prosodified, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the sampled token
      # Adjust the violations of MAX-PROM based on the token frequency of the form; column 18 corresponds to MAX-Prom
      temp_learner_tableau <- tableaux[[2]][[sampled_tableau_number]] # make a temporary tableau
      temp_multiplied <- temp_learner_tableau[, 18] * log(complete_initial_learner_lexicon$Frequency[row_in_lexicon]) # multiply the column of MAX-PROM by the log of the token freq of the current item
      if(length(temp_multiplied) == 0){
        trap_forms <- append(sampled_item, trap_forms)
        next
      }
      temp_learner_tableau[, 18] <- temp_multiplied
      
      # update the candidate probabilities based on the new violation profile
      temp_all_tableaux <- tableaux # make a copy of the tableaux object
      temp_all_tableaux[[2]][[sampled_tableau_number]] <- temp_learner_tableau # add the temporary learner tableau here
      temp_learner_cand_probs <- initial_learner_candidate_probs(temp_all_tableaux[[2]], learner_weights) # update probabilities of candidates using current weights; index 2 in the tableaux object has violation profiles
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = temp_learner_cand_probs[[sampled_tableau_number]]) # sample a learner's winner based on the temporary probabilities
      learner_winner <- temp_all_tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = temp_learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners
        }
        teacher_winner <- temp_all_tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_log_token_freq(teacher_winner, learner_winner, rate = learning_rate, general_max_prom_rate = learning_rate_max_prom, item_token_freq = complete_initial_learner_lexicon$Frequency[row_in_lexicon], learner_weights) # update the constraint weights, given the token frequency of the current item
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates, complete_initial_learner_lexicon) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- as.data.frame(rbind(learner_weights))
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    current_generation <- current_generation + 1
  }
  while(current_generation > 1 & current_generation <= number_generations){
    # initialize a new lexicon with all token frequencies set to 0
    complete_initial_learner_lexicon <- as.data.frame(cbind(complete_stress_types, complete_segmental_shapes, complete_individual_prosodic_shapes))
    complete_initial_learner_lexicon <- cbind(complete_initial_learner_lexicon, rep(0, length(complete_stress_types)))
    colnames(complete_initial_learner_lexicon) <- c("Type", "Segmental", "Prosodic Shape", "Frequency")
    learner_cand_probs <- initial_learner_candidate_probs(learner_violations, initial_weights)
    learner_weights <- initial_weights
    iteration_weights <- list()
    #old_weights_ident_prom <- c(0)
    number_updates <- 0 # keep track of how many total updates to weights have occurred
    no_learning <- 0 # keep track of how many cases there were in which a token was sampled and no learning occurred
    sampled_tokens <- c()
    trap_forms <- c()
    for(i in 1:number_iterations){
      # The first sampling behaviors will need to be changed and updated
      #sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
      #sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
      
      #possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
      #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
      # First sample from the original empirical distribution to a lexical item -- the probability of the unprosodified lexical items remains constant
      sampled_item <- sample(rv_overt_2_to_5_syllable_forms_types_freq_table$Type, 1, prob = rv_overt_2_to_5_syllable_forms_types_freq_table$`Token Freq`/sum(rv_overt_2_to_5_syllable_forms_types_freq_table$`Token Freq`)) # sample an overt token
      sampled_tokens <- append(sampled_tokens, sampled_item)
      #sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      #sampled_item # see what the token is
      #sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      #sampled_item_no_stress # check it
      
      # Before looking up the the relevant tableau, it is necessary to get the output of the previous generation for a lexical item of that shape
      # convert the sampled item to an abstract prosodic shape in order to find its tableau
      sampled_item_prosodified <- parse_prosodic_shape(sampled_item)
      sampled_tableau_number <- grep(paste("/", sampled_item_prosodified, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      #sampled_tableau_number # check that it exists; there should be exactly one winner
      
      # Now get a teacher output based on the probability distribution of the preceding generation
      # Remember to convert it into an overt form as well by removing junk
      teacher_token_prosodic_shape <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]]) # this should be able to find cases where the learner deviates
      #print("teacher token ok!")
      teacher_token_prosodic_shape <- gsub(".*> ", "", teacher_token_prosodic_shape) # remove everything to the left of the arrow and space
      teacher_token_prosodic_shape <- gsub("[\\(\\)]", "", teacher_token_prosodic_shape) # remove parentheses
      #teacher_token <- gsub("2", "", teacher_token) # remove secondary stress
      
      # In order to update the token frequency for that lexical item, you have to find the segmentally matching type and the matching prosodic shape
      sampled_item_no_stress <- gsub(";", "", sampled_item)
      matching_row <- which(complete_initial_learner_lexicon$Segmental == sampled_item_no_stress)
      row_in_lexicon <- matching_row[which(complete_initial_learner_lexicon$`Prosodic Shape`[c(matching_row)] == teacher_token_prosodic_shape)] # look up the row in the lexicon
      complete_initial_learner_lexicon$Frequency[row_in_lexicon] <- (complete_initial_learner_lexicon$Frequency[row_in_lexicon] + 1) # add 1 to the Frequency column
      
      possible_winner_numbers <- grep(teacher_token_prosodic_shape, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
      #print("possible winners ok!")
      # Adjust the violations of MAX-PROM based on the token frequency of the form; column 18 corresponds to MAX-Prom
      temp_learner_tableau <- tableaux[[2]][[sampled_tableau_number]] # make a temporary tableau
      temp_multiplied <- temp_learner_tableau[, 18] * log(complete_initial_learner_lexicon$Frequency[row_in_lexicon]) # multiply the column of MAX-PROM by the log of the token freq of the current item
      if(length(temp_multiplied) == 0){
        trap_forms <- append(sampled_item, trap_forms)
        next
      }
      temp_learner_tableau[, 18] <- temp_multiplied
      # update the candidate probabilities based on the new violation profile
      temp_all_tableaux <- tableaux # make a copy of the tableaux object
      temp_all_tableaux[[2]][[sampled_tableau_number]] <- temp_learner_tableau # add the temporary learner tableau here
      temp_learner_cand_probs <- initial_learner_candidate_probs(temp_all_tableaux[[2]], learner_weights) # update probabilities of candidates using current weights; index 2 in the tableaux object has violation profiles
      learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = temp_learner_cand_probs[[sampled_tableau_number]]) # sample a learner's winner based on the temporary probabilities
      learner_winner <- temp_all_tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
      if(learner_winner_number %in% possible_winner_numbers){
        # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        no_learning <- no_learning + 1
        iteration_weights[[i]] <- learner_weights # save current weights
      }
      else{
        # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
        if(length(possible_winner_numbers) == 1){ # if there is only one possible winner, 
          teacher_winner_number <- possible_winner_numbers # assign it directly to the teacher winner number
        }
        else{ # else sample from the possible winners
          teacher_winner_number <- sample(possible_winner_numbers, size=1, prob = temp_learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)] ) # choose one of the possible winners under the current temporary violation profile and candidate probabilities
        }
        #print("teacher winner number ok!")
        teacher_winner <- temp_all_tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ] # take the violation profile of that winner
        # Update constraint weights
        learner_weights <- update_rule_log_token_freq(teacher_winner, learner_winner, rate = learning_rate, general_max_prom_rate = learning_rate_max_prom, item_token_freq = complete_initial_learner_lexicon$Frequency[row_in_lexicon], learner_weights) # update the constraint weights
        #old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
        # Update candidate probabilities
        learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # update the candidate probabilities
        iteration_weights[[i]] <- learner_weights # save current weights
        number_updates <- number_updates + 1 # 
        no_learning <- 0
      }
      # Store the final candidate probabilities and weights in a list
      #if(no_learning > (number_iterations * 0.01)){ # cut off learning if there are many successive instances of no updates
      # break
      #}
    }
    final_learner_outcome <- list(learner_cand_probs, learner_weights, iteration_weights, number_updates, complete_initial_learner_lexicon) # old_weights_ident_prom,
    names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
    names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
    for(i in 1:length(final_learner_outcome[[1]])){
      names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
    }
    learner_weights_frame <- rbind(learner_weights_frame, learner_weights) # Add learner weights from the subsequent generation to the data frame of weights
    colnames(learner_weights_frame) <- tableaux[[5]]
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]] # you had forgotten this!
    generation_results[[current_generation]] <- final_learner_outcome
    # Getting SSEs for stress patterns in the current generation
    current_generation_output_probabilities <- c() # create an empty vector for the probability of each possible overt form
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      #input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[current_generation]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      # you really only need the output probabilities 
      current_generation_output_probabilities <- append(current_generation_output_probabilities, current_output_probabilities)
    }
    # if(length(current_generation_output_probabilities) == 502){
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE[-c(223), ]
    # }
    # else{
    #   stress_patterns_for_SSE_copy <- stress_patterns_for_SSE
    # }
    current_SSE_results <- c() 
    for(i in 3:ncol(stress_patterns_for_SSE_lexical)){
      temp_SSE_results <- get_SSE(stress_patterns_for_SSE_lexical[, i], current_generation_output_probabilities)
      current_SSE_results <- append(current_SSE_results, temp_SSE_results)
    }
    total_SSE_results <- as.data.frame(rbind(total_SSE_results, current_SSE_results))
    colnames(total_SSE_results) <- colnames(stress_patterns_for_SSE_lexical[, 3:ncol(stress_patterns_for_SSE_lexical)])
    print(current_generation)
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
    ### Here, a dataframe with final output frequencies is created, which will allow for straightforward comparison to the distributions in the initial state.
    final_output_frequencies <- as.data.frame(cbind(0,0,0,0)) # The dataframe needs four columns: the underlying abstract prosodic shape, corresponding overt forms, the output probability of the overt form, and the original (RV) token frequency
    for(i in 1:length(learner_cand_probs)){ # iterate over the number of possible inputs
      input_shape <- names(generation_results[[number_generations]][[1]])[i] # extract the UR
      current_output_probabilities <- get_position_probabilities(generation_results[[number_generations]], i) # take the probabilities of the overt forms; this crucially depends on the helper function get_position_probabilities
      output_overt_forms <- gsub("^.*> ", "", names(current_output_probabilities)) # convert the overt forms to just a single shape, rather than a mapping
      for(output in 1:length(current_output_probabilities)){ # iterate again over the number of possible inputs
        frequency_original_overt <- all_prosodic_shapes_frame$Freq[which(all_prosodic_shapes_frame$all_prosodic_shapes == output_overt_forms[output])] # find the original frequency of the current over forms
        to_be_bound <- c(input_shape, output_overt_forms[output], current_output_probabilities[output], frequency_original_overt) # put all four pieces in a vector
        final_output_frequencies <- rbind(final_output_frequencies, to_be_bound) # bind the vector to the dataframe
      }
    }    
    final_output_frequencies <- final_output_frequencies[-c(1), ] # remove the first row of the dataframe that contains only 0's
    colnames(final_output_frequencies) <- c("Input_Shape", "Output_Overt", "Output_Prob", "Original_Freq") # add column names
    final_output_frequencies$Original_Freq[which(is.na(as.numeric(final_output_frequencies$Original_Freq)) == TRUE)] <- 0 # replace all NAs with 0. These are cases where the output has a non-zero-probability, but the overt form did not occur in the RV. # you will see warning messages
    frequency_input_shapes <- c() # Now, a vector with the frequencies of the input UR shapes in the RV will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      quantity_input_shape <- rep(final_output_frequencies$Input_Shape[input_shape], as.numeric(final_output_frequencies$Original_Freq[input_shape]))
      frequency_input_shapes <- append(frequency_input_shapes, quantity_input_shape)
    }
    table_input_shape_frequencies <- table(frequency_input_shapes) # a table is created to sum these frequencies
    predicted_new_frequencies <- c() # Now, a vector with the predicted new frequencies will be populated
    for(input_shape in 1:length(final_output_frequencies$Input_Shape)){
      current_input_shape <- final_output_frequencies$Input_Shape[input_shape]
      input_shape_frequency <- table_input_shape_frequencies[current_input_shape]
      predicted_frequency <- (as.numeric(final_output_frequencies$Output_Prob[input_shape])*input_shape_frequency)
      predicted_new_frequencies <- append(predicted_new_frequencies, round(predicted_frequency,0) )
    }
    final_output_frequencies <- cbind(final_output_frequencies, predicted_new_frequencies) # These final new frequencies are bound to the dataframe
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies, total_SSE_results) # The final list contains three items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies; SSEs of categorical grammars
    return(final_generation_results)
  }
}
# what you should really include is a specific probability for specific lexical items, since their stress faithfulness depends on their token frequency
# indeed, you might consider generating the predicted token frequencies for each input...



vedic_lexical_tableaux <- prepare_data("C_Skt_RIP_Lexical_Stress_corrected.txt") 
learner_weights <- c(rep(5, ncol(vedic_lexical_tableaux[[2]][[1]])-1), 0) # set all weights of all (markedness) constraints to 0
learner_violations <- vedic_lexical_tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.


# The run-time is even slower than for other things, but it will do
test_lexical_freq <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 3) # with so few tokens, the weight of Max-Prom increases so slowly, that hardly anything happens
lexical_freq_1_gen_10000 <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 1)
# 5.a.
lexical_freq_1_gen_10000_sim_2 <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 1)

gen_1_iteration_weights_sim_lexical_freq_1 <- do.call(rbind.data.frame, lexical_freq_1_gen_10000_sim_2[[1]][[1]][[3]])
colnames(gen_1_iteration_weights_sim_lexical_freq_1) <- vedic_lexical_tableaux[[5]]

colnames(gen_1_iteration_weights_sim_lexical_freq_1)[10] <- "All-Ft-L"
colnames(gen_1_iteration_weights_sim_lexical_freq_1)[17] <- "*CLASH"
colnames(gen_1_iteration_weights_sim_lexical_freq_1)[18] <- "MAX-PROM"

token_step <- c(1:10000)
iteration_weights_sim_lexical_1 <- cbind(gen_1_iteration_weights_sim_lexical_freq_1, token_step)

iteration_weights_lexical_plot <- ggplot(data = gen_1_iteration_weights_sim_lexical_freq_1, aes(x = token_step)) + geom_line(aes(y=`MAX-PROM`, color="MAX-PROM")) + geom_line(aes(y=WSP, color="WSP")) + geom_line(aes(y=Trochee, color="Trochee")) +
  geom_line(aes(y=`All-Ft-L`, color="All-Ft-L")) + geom_line(aes(y=Rightmost, color="Rightmost")) + ggtitle("Change in Constraint Weights: Frequency-Sensitive Lexical Stress") + labs(x="Token", y="Constraint Weight", color="Constraint") #xlab("Token") + ylab("Constraint Weight")
plot(iteration_weights_lexical_plot)

# 5.b.
lexical_freq_50_gen_10000_sim_3 <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 50)

generation_weights_sim_6_b <- lexical_freq_50_gen_10000_sim_3[[2]]
Generation <- 1:50
generation_weights_sim_6_b <- cbind(generation_weights_sim_6_b, Generation)
colnames(generation_weights_sim_6_b)[10] <- "All-Ft-L"
colnames(generation_weights_sim_6_b)[18] <- "MAX-PROM"

generation_weights_sim_6_b_plot <- ggplot(data = generation_weights_sim_6_b, aes(x = Generation)) + geom_line(aes(y=`MAX-PROM`, color="MAX-PROM")) + geom_line(aes(y=WSP, color="WSP")) + 
  geom_line(aes(y=`All-Ft-L`, color="All-Ft-L")) + ggtitle("Change in Constraint Weights: Frequency-Sensitive Lexical Stress") + labs(x="Generation", y="Constraint Weight", color="Constraint")
plot(generation_weights_sim_6_b_plot)

SSEs_sim_6_b <- lexical_freq_50_gen_10000_sim_3[[4]]
SSEs_sim_6_b <- cbind(SSEs_sim_6_b, Generation)
colnames(SSEs_sim_6_b)[1] <- "QI Initial"
colnames(SSEs_sim_6_b)[2] <- "QI Peninitial"
colnames(SSEs_sim_6_b)[16] <- "DTS-L-3σFSM"
colnames(SSEs_sim_6_b)[17] <- "Perfect Lexical"

SSEs_sim_6_b_plot <- ggplot(data = SSEs_sim_6_b, aes(x = Generation)) + geom_line(aes(y=`Perfect Lexical`, color="Perfect Lexical")) + geom_line(aes(y=`QI Initial`, color="QI Initial")) + 
  geom_line(aes(y=`QI Peninitial`, color="QI Peninitial")) + geom_line(aes(y=`DTS-L-3σFSM`, color="DTS-L-3σFSM")) + labs(x="Generation", y="SSE", color="Stress Pattern")
plot(SSEs_sim_6_b_plot)


# 5.c

test_lexical_freq_50_gen_weight_5 <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 50) # fifty generations with markedness at 5 to start
test_lexical_freq_50_gen_weight_10_01 <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, learning_rate = 0.1, number_iterations = 10000, number_generations = 3) # fifty generations with markedness at 5 to start



# One generation with lots of data -- how high can MAX-PROM go?

test_lexical_freq_50_big # markedness at 10
test_lexical_freq_50_big_weight_5 # markedness at 5
# as expected, when initial weights of markedness constraints are lower, it is easier for MAX-PROM to be driven up to a point that even highly marked structures 
# (primary stress on a final light syllable) can be guaranteed. An initial weight of 5, at least for the 
# The total number of tokens that condition updating is not large: under 8%
# Not
test_cand_probs <- initial_learner_candidate_probs(tableaux_copy[[2]], learned_weights_5[1, ])
initial_learner_candidate_probs

learner_weights <- unlist(test_lexical_freq_50_big_weight_5[[1]][[1]][[2]])# set all weights of all (markedness) constraints to 0
learner_weights <- learner_weights[-c(18)]
learner_violations <- tableaux_copy[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # s