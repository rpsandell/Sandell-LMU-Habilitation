number_iterations <- 10000
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
}

#### Making lexically accented tableaux
setwd("/Users/rpsandell/Documents/Dropbox/GitHub/Sandell-LMU-Habilitation/Chapter 6")
prosodic_tableaux <- read.table("C_Skt_for_RIP_corrected_3.txt", sep="\t", skip=1, header=T)

# add column for MAX-Prom
MAX_PROM <- rep(0, nrow(prosodic_tableaux))
prosodic_tableaux_2 <- cbind(prosodic_tableaux, MAX_PROM)
# make a list object where each element is a tableau; iterate over the list and make all of the tableau copies, appending them
# to prosodic_tableux_2; then delete the first 1735 rows of prosodic_tableaux_2
prosodic_tableaux_list <- list()
tableaux_counter <- 1
for(i in 1:nrow(prosodic_tableaux_2)){ # there are 1735 rows
  if(prosodic_tableaux_2$X[i] != ""){
    temp_tableaux <- rbind(prosodic_tableaux_2[i, ])
  }
  else if(i == 1734){
    temp_tableaux <- rbind(temp_tableaux, prosodic_tableaux_2[i, ])
    prosodic_tableaux_list[[tableaux_counter]] <- temp_tableaux
  }
  else{
    temp_tableaux <- rbind(temp_tableaux, prosodic_tableaux_2[i, ])
    if(prosodic_tableaux_2$X[i+1] != ""){
      prosodic_tableaux_list[[tableaux_counter]] <- temp_tableaux
      tableaux_counter <- tableaux_counter+1
    }
  }
}

new_stressed_tableaux <- list()
stressed_tableaux_counter <- 1
# to create possible stress positions, just do a strsplit on " ", then iterate over the vector, add a "1" to the end of
# the current element (unless the current element ends in "/", in which case add the one after a V or V:), then combine with
# all of the other elements. Do a temporary substitution of the element that is supposed to have the "1", then do paste(... collapse = " ").
# Finally, add violations into MAX-PROM as appropriate
for(i in 1:length(prosodic_tableaux_list)){
  my_current_tableau <- prosodic_tableaux_list[[i]]
  current_input <- my_current_tableau$X[1]
  current_input_split <- unlist(strsplit(current_input, split=" "))
  for(syllable in 1:length(current_input_split)){
    current_syllable <- current_input_split[syllable]
    stressed_syllable <- gsub("([LHV]:?)(C?/?)$", "\\11\\2", current_syllable) # add the 1 to mark that syllable as stressed
    current_input_stressed <- current_input_split
    current_input_stressed[syllable] <- stressed_syllable # insert the stressed syllable at the right position in the vector
    current_input_stressed <- paste(current_input_stressed, collapse = " ")
    current_temp_tableau <- my_current_tableau
    current_temp_tableau$X[1] <- current_input_stressed
    # checking violations of MAX_PROM
    for(candidate in 1:nrow(current_temp_tableau)){
      output <- gsub(" \\\\.+", "", current_temp_tableau$X.1[candidate])
      output <- gsub("\\[|\\]", "", output)
      input <- gsub("/", "", current_temp_tableau$X[1])
      if(input != output){
        current_temp_tableau$MAX_PROM[candidate] <- 1
      }
    }
    new_stressed_tableaux[[stressed_tableaux_counter]] <- current_temp_tableau
    stressed_tableaux_counter <- stressed_tableaux_counter+1
  }
}

# make dataframe of stressed tableaux
all_stressed_tableaux <- rbind(new_stressed_tableaux[[1]])
for(i in 2:length(new_stressed_tableaux)){
  all_stressed_tableaux <- rbind(all_stressed_tableaux, new_stressed_tableaux[[i]])
}

write.table(all_stressed_tableaux, "C_Skt_RIP_Lexical_Stress_corrected_3.txt", sep="\t", quote=FALSE, row.names = FALSE, na="0")
# after writing out, duplicate the top row of headers
# recall now that the freq column is pretty much useless




