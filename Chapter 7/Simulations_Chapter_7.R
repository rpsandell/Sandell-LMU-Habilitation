



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