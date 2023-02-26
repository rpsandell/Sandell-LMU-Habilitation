

### Simulation 2: Token frequency effects
# Violations of MAX-Prom are multiplied by the log of a lexeme's token frequency
# individual lexical items can be restructured
# learning rate for MAX-Prom is set at the default for a token frequency of 1 or 2, otherwise divided by the log of a lexeme's token frequency
# for this, you will need to make some adjustments to 
#rv_overt_2_to_5_syllable_forms_types_freq_table # contains the token frequencies of each surface type
#View(initial_learner_lexicon)

# practically, you only need to do this with onset maximization
# syllabify_and_stressify <- function(form){ # get syllabification and possible stress positions for all types, # then all of these are packed into a lexicon where all options exist
#   destressed_form <- gsub(";", "", form)
#   syllable_boundaries <- gsub(paste("(", V, "|", v, ")", ";?", "(", C, "|", V, "|", v, ")", ";?", sep=""), "\\1\\.\\2", destressed_form)
#   syllable_boundaries <- gsub(paste("(", V, "|", v, ")", ";?", "(", C, "|", V, "|", v, ")", ";?", sep=""), "\\1\\.\\2", syllable_boundaries)
#   syllable_split <- unlist(strsplit(syllable_boundaries, split="\\."))
#   stress_options_vector <- c()
#   for(syllable in 1:length(syllable_split)){
#     stressed_syllable <- gsub(paste("(", V,"|",v,")", sep=""), "\\1;" ,syllable_split[syllable])
#     temp_stressed_split <- syllable_split
#     temp_stressed_split[syllable] <- stressed_syllable
#     stressed_form <- paste(temp_stressed_split, collapse="")
#     stress_options_vector <- append(stress_options_vector, stressed_form)
#   }
#   stress_options_vector <- grep(";", stress_options_vector, value=T) # only keep options where a stress has been added
#   return(stress_options_vector)
# }
# 
# length(rv_overt_2_to_5_syllable_forms_types_freq_table$Type)
# complete_stress_types <- c()
# #complete_individual_prosodic_shapes <- c()
# for(form in 1:length(rv_overt_2_to_5_syllable_forms_types_freq_table$Type)){
#   current_stress_options <- syllabify_and_stressify(rv_overt_2_to_5_syllable_forms_types_freq_table$Type[form])
#   complete_stress_types <- append(complete_stress_types, current_stress_options)
#   #complete_individual_prosodic_shapes <- append(complete_individual_prosodic_shapes, parse_prosodic_shape(current_stress_options))
# }
# # the first version of syllabify_and_stressify resulted in two ;; in words ending in v;.
# complete_stress_types <- unique(complete_stress_types)
# complete_segmental_shapes <- gsub(";", "", complete_stress_types)
# complete_individual_prosodic_shapes <- parse_prosodic_shape(complete_stress_types)
# complete_initial_learner_lexicon <- as.data.frame(cbind(complete_stress_types, complete_segmental_shapes, complete_individual_prosodic_shapes))
# complete_initial_learner_lexicon <- cbind(complete_initial_learner_lexicon, rep(0, length(complete_stress_types)))
# colnames(complete_initial_learner_lexicon) <- c("Type", "Segmental", "Prosodic Shape", "Frequency") 

# there are then a total of 79628 "stress types" of 2- 5-syllable words

#parse_prosodic_shape(syllabify_and_stressify(form)) # this will provide all possible stress positions for all surface word types in Vedic, and give their abstract prosodic shape

### Here is the modified version of the update rule for the token-frequency-sensitive learner
update_rule_log_token_freq <- function(teacher_winner, learner_winner, rate, item_token_freq, learner_weights){
  if(item_token_freq < 3){
    max_prom_rate <- rate
  }
  else{
    max_prom_rate <- (rate/log(item_token_freq))
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
learning_loop_lexical_freq_1 <- function(tableaux = my_lexical_tableaux, number_iterations = 5000, learning_rate = 0.01, learning_rate_max_prom = 0.005, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
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
      temp_learner_tableau[, 18] <- temp_learner_tableau[, 18] * log(complete_initial_learner_lexicon$Frequency[row_in_lexicon]) # multiply the column of MAX-PROM by the log of the token freq of the current item
      
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
        learner_weights <- update_rule_log_token_freq(teacher_winner, learner_winner, rate = learning_rate, item_token_freq = complete_initial_learner_lexicon$Frequency[row_in_lexicon], learner_weights) # update the constraint weights, given the token frequency of the current item
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
      temp_learner_tableau[, 18] <- temp_learner_tableau[, 18] * log(complete_initial_learner_lexicon$Frequency[row_in_lexicon]) # multiply the column of MAX-PROM by the log of the token freq of the current item
      
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
        learner_weights <- update_rule_log_token_freq(teacher_winner, learner_winner, rate = learning_rate, item_token_freq = complete_initial_learner_lexicon$Frequency[row_in_lexicon], learner_weights) # update the constraint weights
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
    final_generation_results <- list(generation_results, learner_weights_frame, final_output_frequencies) # The final list contains three items: a list with detailed results from each generation of learning; a dataframe with the weights at the end of each generation; a dataframe with final probabilities and overt frequencies
    return(final_generation_results)
  }
}
# what you should really include is a specific probability for specific lexical items, since their stress faithfulness depends on their token frequency
# indeed, you might consider generating the predicted token frequencies for each input...

# We need to make a table of categorical grammars for getting the SSEs where lexical stress is in



vedic_lexical_tableaux <- prepare_data("C_Skt_RIP_Lexical_Stress.txt") 
learner_weights <- c(rep(10, ncol(vedic_lexical_tableaux[[2]][[1]])-1), 0) # set all weights of all (markedness) constraints to 0
learner_violations <- vedic_lexical_tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.


# The run-time is even slower than for other things, but it will do
test_lexical_freq <- learning_loop_lexical_freq_1(tableaux = vedic_lexical_tableaux, number_iterations = 10000, number_generations = 10)


