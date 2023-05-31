
options(scipen = 999) # turn off scientific notation

# Cleaning up Vedic abstract stress for RIP, removing the different lexical stress inputs

vedic_no_lexical <- read.table("RIP_Vedic_No_Lexical_Stress.txt", skip = 2, header=F, sep="\t")

vedic_no_lexical$V1 <- gsub("1", "", vedic_no_lexical$V1)

rows_to_remove <- c()
for(input in 1:length(vedic_no_lexical$V1)){
  if(input == 1){
    current_input <- ""
    previous_input <- ""
  }

  if(vedic_no_lexical$V1[input] != ""){
    previous_input <- current_input
    current_input = vedic_no_lexical$V1[input]
  }
  
  if(previous_input == current_input){
    rows_to_remove <- append(rows_to_remove, input)
  }
  
   
}

vedic_no_lexical <- vedic_no_lexical[-c(rows_to_remove), ]

rows_to_remove_2 <- c()
already_seen <- c()
for(input in 1:length(vedic_no_lexical$V1)){
  if(input == 1){
    current_input <- ""
    previous_input <- ""
    input_already_seen <- FALSE
  }
  
  if(vedic_no_lexical$V1[input] != ""){
    previous_input <- current_input
    already_seen <- append(already_seen, previous_input)
    current_input = vedic_no_lexical$V1[input]
    if(current_input %in% already_seen){
      input_already_seen <- TRUE
    }
    else{
      input_already_seen <- FALSE
    }
  }
  
  if(input_already_seen == TRUE){
    rows_to_remove_2 <- append(rows_to_remove_2, input)
  }
  
  
}

vedic_no_lexical_final <- vedic_no_lexical[-c(rows_to_remove_2), ]
# Inspected the resulting table manually; looks in order

#binary_double_heavy_feet <- grep("\\(H1? H1?\\)", vedic_no_lexical_final$V2, value = T) # maybe want to remove these, except that some have the input in the leftmost column...


# after writing out the table, remember to 
write.table(vedic_no_lexical_final, file = "RIP_Vedic_No_Lexical_Final.txt", quote=FALSE, sep="\t", row.names=FALSE, na= "")

### Goal: Eliminate binary feet with two heavy syllables
  # In some cases, these will need to be replaced by feet with a single heavy syllable
vedic_no_lexical_fixing <- read.table("RIP_Vedic_No_Lexical_Final_V2.txt", skip = 2, header=F, sep="\t")
for(UR in 1:length(vedic_no_lexical_fixing$V1)){
  if(vedic_no_lexical_fixing$V1[UR] != ""){
    current_UR <- vedic_no_lexical_fixing$V1[UR]
  }
  else{
    vedic_no_lexical_fixing$V1[UR] <- current_UR
  }
}
double_heavy_candidates <- grep("\\(H1? (H1?|V:1?|V1?C|V:1?C)\\)", vedic_no_lexical_fixing$V2)
vedic_no_lexical_fixing <- vedic_no_lexical_fixing[-c(double_heavy_candidates), ]
previous_UR <- ""
for(UR in 1:length(vedic_no_lexical_fixing$V1)){
  if(vedic_no_lexical_fixing$V1[UR] != previous_UR){
    previous_UR <- vedic_no_lexical_fixing$V1[UR]
  }
  else{
    vedic_no_lexical_fixing$V1[UR] <- ""
  }
}

write.table(vedic_no_lexical_fixing, file = "RIP_Vedic_No_Lexical_Final_V2_fixed.txt", quote=FALSE, sep="\t", row.names=FALSE, na= "")



vedic_lexical_candidates <- read.table("RIP_Vedic_Abstract_Lexical.txt", skip = 2, header=F, sep="\t")
just_candidates <- vedic_lexical_candidates$V2

# Sequences to search for: (H1 H), (H H1), (H1 V:), (H V:1), (H1 VC), (H V1C), (H1 V:C), (H V:1C)
h1_h <- grep("\\(H1 H\\)", just_candidates)

just_candidates <- gsub("\\(H1 H\\)", "\\(H1\\) H", just_candidates)

for(candidate in h1_h){
  
}

### Writing the function for multiple generations of learning
simple_learning_loop_no_lexical <- function(number_iterations = 5000, learning_rate = 0.01, number_generations = 1, initial_weights = learner_weights){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  current_generation <- 1
  generation_results <- list()
  if(current_generation == 1){
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
      sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
      sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      sampled_item_no_stress # check it
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      sampled_tableau_number # check that it exists; there should be exactly one winner
    
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
    previous_generation_learner_cand_probs <- final_learner_outcome[[1]]
    generation_results[[current_generation]] <- final_learner_outcome
    current_generation <- current_generation + 1
  }
  if(current_generation > 1 & current_generation <= number_generations){
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
      sampled_item # see what the token is
      sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
      sampled_item_no_stress # check it
      # Find the relevant tableau
      sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
      sampled_tableau_number # check that it exists; there should be exactly one winner
      
        # Now get a teacher output based on the probability distribution of the preceding generation
        # Remember to convert it into an overt form as well by removing junk
      teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
      #print("teacher token ok!")
      teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
      teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
      
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
        teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)]) # choose one of the possible winners
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
    generation_results[[current_generation]] <- final_learner_outcome
    current_generation <- current_generation + 1    
  }
  if(current_generation > number_generations){
  return(generation_results)
  }
}



## Testing my RegEx
teacher_token <- gsub(".*> ", "", names(previous_generation_learner_cand_probs[[50]])) # remove everything to the left of the arrow and space
teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
teacher_token


## Testing the sampling procedure for subsequent generations
test_amount <- 10000
for(i in 1:test_amount){
  sampled_item <- sample(all_prosodic_shapes_frame$all_prosodic_shapes[limited_2_3_4_5_indices], 1, prob = (all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices]/sum(all_prosodic_shapes_frame$Freq[limited_2_3_4_5_indices])))
  #sampled_item # see what the token is
  sampled_item_no_stress <- gsub("1", "", sampled_item) # remove the primary stress mark stress
  #sampled_item_no_stress # check it
  # Find the relevant tableau
  sampled_tableau_number <- grep(paste("/", sampled_item_no_stress, "/", sep=""),  names(tableaux[[3]])) # see what UR it corresponds to
  #sampled_tableau_number # check that it exists; there should be exactly one winner
  
  # Now get a teacher output based on the probability distribution of the preceding generation
  # Remember to convert it into an overt form as well by removing junk
  teacher_token_names <- names(previous_generation_learner_cand_probs[[sampled_tableau_number]])
  teacher_token_probabilities <- previous_generation_learner_cand_probs[[sampled_tableau_number]]
  if(length(teacher_token_names) != length(teacher_token_probabilities)){
    print("teacher lengths don't match")
    print(sampled_tableau_number)
  }
  learner_winner_rows <- c(1:nrow(tableaux[[2]][[sampled_tableau_number]]))
  number_learner_cand_probs <- learner_cand_probs[[sampled_tableau_number]]
  if(length(learner_winner_rows) != length(number_learner_cand_probs)){
    print("learner lengths don't match")
    print(sampled_tableau_number)
  }
  
  #teacher_token <- sample(names(previous_generation_learner_cand_probs[[sampled_tableau_number]]), 1, prob = previous_generation_learner_cand_probs[[sampled_tableau_number]])
  
  
  #teacher_token <- gsub(".*> ", "", teacher_token) # remove everything to the left of the arrow and space
  #teacher_token <- gsub("[\\(\\)]", "", teacher_token) # remove parentheses
  
  #possible_winner_numbers <- grep(teacher_token, tableaux[[3]][[sampled_tableau_number]]) # find the indices of the overt forms that match the teacher's token candidate
  #learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
  #learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
}

#### Making overt forms with the C. Skt. rule



