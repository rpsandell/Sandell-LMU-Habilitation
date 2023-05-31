  # vectors for abbreviations in regular expressions
C <- "[bBcCdDfFgGhHjJkKlLmMnNpPrsStTvyYzZwWxX]"
v <- "[aiuRQ]"
V <- "[AIUeoEO]"


# Read in and create frequency table based on Zurich text
setwd("~/Documents/Dropbox/Rigvedic Formularity/Corpora/Parsed Corpus from Kevin")
rv_zurich <- read.table("processed_zurich.txt", header=T, sep="\t")
tokens_rv <- rv_zurich$Pada

C <- "[bBcCdDfFgGhHjJkKlLmMnNpPrsStTvyYzZwWxX]" #Consonants
v <- "[aiuRQ]" # short vowels
V <- "[AIUeoEO]" # long vowels / diphthongs

# replace the diphthongs ai and au with E and O, because matching ai and au sequences gets too complicated.
# Other digraphs must be replaced too.
tokens_rv <- gsub("ai", "E", tokens_rv)
tokens_rv <- gsub("au", "O", tokens_rv)
#tokens_rv <- gsub("([ptckbdjgTD])h", "\\1h", tokens_rv)
# NB T, D = retroflexes; G = velar nasal
tokens_rv <- gsub("ph", "P", tokens_rv)
tokens_rv <- gsub("th", "F", tokens_rv) 
tokens_rv <- gsub("ch", "C", tokens_rv)
tokens_rv <- gsub("kh", "K", tokens_rv)
tokens_rv <- gsub("bh", "B", tokens_rv)
tokens_rv <- gsub("dh", "Z", tokens_rv)
tokens_rv <- gsub("jh", "J", tokens_rv)
tokens_rv <- gsub("gh", "w", tokens_rv) # Add to list; G already used
tokens_rv <- gsub("Th", "x", tokens_rv) # Add to list; T already used
tokens_rv <- gsub("Dh", "X", tokens_rv) # Add to list; D already used
# remove H and L since you need those in the prosodic shape parsing
tokens_rv <- gsub("H", "7", tokens_rv) # Add to list; T already used
tokens_rv <- gsub("L", "8", tokens_rv) # Add to list; D already used
tokens_rv <- gsub("RR", "3", tokens_rv) # Add to list of V
tokens_rv <- gsub("-", "", tokens_rv)
tokens_rv <- gsub("\\+", "", tokens_rv)
tokens_rv <- gsub("\\*", "", tokens_rv)
tokens_rv <- gsub("!", "", tokens_rv)
tokens_rv <- gsub("\\?", "", tokens_rv)
tokens_rv <- gsub("=", "", tokens_rv)


C <- "[bBcCdDfFgGhjJkKlmMnNpPrsStTvyYzZwWxX78]"
v <- "[aiuRQ]"
V <- "[AIUeoEO3]"

# The forms with independent svarita! These contain \\ in their entries
tokens_rv <- gsub("y([aiuRQAIUeoEO])\\\\", "y\\1;", tokens_rv) # you converted the forms with independent svarita to stressed on the vowel with the svarita!
# don't use this


stressed_tokens_rv <- tokens_rv[grep(";", tokens_rv)]
single_stressed_tokens_rv <- stressed_tokens_rv[-c(grep(";.*;", stressed_tokens_rv))]
multiple_stressed_tokens_rv <- tokens_rv[grep(";.*;", tokens_rv)]
no_stress_tokens_rv <- tokens_rv[-c(grep(";", tokens_rv))] # remove any forms with an udātta
no_stress_tokens_rv <- no_stress_tokens_rv[-c(grep("\\\\", no_stress_tokens_rv))]
#no_stress_tokens_rv <- tokens_rv[grep("[^;]+", tokens_rv)]

### For type frequencies
tokens_rv_unique <- unique(tokens_rv) # 31593
stressed_tokens_rv_unique <- tokens_rv_unique[grep(";", tokens_rv_unique)]
single_stressed_tokens_rv_unique <- stressed_tokens_rv_unique[-c(grep(";.*;", stressed_tokens_rv_unique))]
multiple_stressed_tokens_rv_unique <- tokens_rv_unique[grep(";.*;", tokens_rv_unique)]
no_stress_tokens_rv_unique <- tokens_rv_unique[-c(grep(";", tokens_rv_unique))] # remove any forms with an udātta
no_stress_tokens_rv_unique <- no_stress_tokens_rv_unique[-c(grep("\\\\", no_stress_tokens_rv_unique))] # remove any forms with an independent svarita
svarita_tokens_rv_unique <- tokens_rv_unique[grep("\\\\", tokens_rv_unique)]



parse_prosodic_shape <- function(word){
  word <- gsub(paste(v, ";", "$", sep=""), "V1", word)
  word <- gsub(paste(v, "$", sep=""), "V", word)
  word <- gsub(paste(v, ";", C, "$", sep=""), "V1C", word)
  word <- gsub(paste(v, C, "$", sep=""), "VC", word)
  word <- gsub(paste(V, ";", "$", sep=""), "V:1", word)
  word <- gsub(paste(V, "$", sep=""), "V:", word)
  word <- gsub(paste(V, ";", C,  "$", sep=""), "V:1C", word)
  word <- gsub(paste(V, C, "$", sep=""), "V:C", word)
  # treat forms ending in two consonants as equivalent to V:C
  word <- gsub(paste(v, ";", C, C, "$", sep=""), "V:1C", word)
  word <- gsub(paste(v, C, C, "$", sep=""), "V:C", word)
  word <- gsub(paste(V, ";", C, C,  "$", sep=""), "V:1C", word)
  word <- gsub(paste(V, C, C, "$", sep=""), "V:C", word)
  
  word <- gsub(paste(v, ";", C, C, "+", sep=""), "H1 ", word)
  word <- gsub(paste(v,C, C, "+", sep=""), "H ", word)
  
  
  word <- gsub(paste(V, ";", C, C, "+", sep=""), "H1 ", word)
  word <- gsub(paste(V,C, C, "+", sep=""), "H ", word)
  
  
  word <- gsub(paste(V, ";", C, "?", sep=""), "H1 ", word)
  word <- gsub(paste(V,C, "?", sep=""), "H ", word)
  
  word <- gsub(paste(v, ";", C, "?", "~", "?", sep=""), "L1 ", word)
  word <- gsub(paste(v, C, "?", "~", "?", sep=""), "L ", word)

  
  #C <- "[bBcCdDfFgGhjJkKlmMnNpPrsStTvyYzZwWxX78]"
  word <- gsub(paste("^", C, "+", sep=""), "", word)
  return(word)
}

# all prosodic shapes for tokens with a single stress
all_prosodic_shapes_2 <- c()
for(i in 1:length(single_stressed_tokens_rv)){
  temp_result <- parse_prosodic_shape(single_stressed_tokens_rv[i])
  all_prosodic_shapes_2 <- append(all_prosodic_shapes, temp_result)
} 
# actually better to use sapply, because then you can see the names in the list, and it doesn't screw up the numbers
all_prosodic_shapes_stressed_rv <- sapply(single_stressed_tokens_rv, parse_prosodic_shape)

all_prosodic_shapes_table <- sort(table(all_prosodic_shapes), decreasing = T)
setwd("~/Documents/Dropbox/RyanTex/Sandell Habilitation/Prominence Frequencies/Vedic")
all_prosodic_shapes_frame <- as.data.frame(all_prosodic_shapes_table)
write.table(all_prosodic_shapes_frame, "RV_All_prosodic_shapes2.txt", quote = FALSE, sep="\t", row.names = FALSE)

# all prosodic shapes for tokens with no udātta
all_prosodic_shapes_no_stress <- c()
for(i in 1:length(no_stress_tokens_rv)){
  temp_result <- parse_prosodic_shape(no_stress_tokens_rv[i])
  all_prosodic_shapes_no_stress <- append(all_prosodic_shapes_no_stress, temp_result)
} 

# prosodic shapes by type frequency
all_prosodic_shapes_stressed_rv_unique <- sapply(single_stressed_tokens_rv_unique, parse_prosodic_shape)
all_prosodic_shapes_stressed_rv_unique_table <- sort(table(all_prosodic_shapes_stressed_rv_unique ), decreasing =T)
all_prosodic_shapes_stressed_rv_unique_frame <- as.data.frame(all_prosodic_shapes_stressed_rv_unique_table)
write.table(all_prosodic_shapes_stressed_rv_unique_frame, "RV_All_prosodic_shapes_types.txt", quote = FALSE, sep="\t", row.names = FALSE)

all_prosodic_shapes_unstressed_rv_unique <- sapply(no_stress_tokens_rv_unique, parse_prosodic_shape)
all_prosodic_shapes_unstressed_rv_unique_table <- sort(table(all_prosodic_shapes_unstressed_rv_unique ), decreasing = T)
# getting type frequencies by syllable length and stress position
matrix_type_udatta_positions <- matrix(0, nrow=8, ncol=8)
for(i in 1:length(all_prosodic_shapes_stressed_rv_unique_table)){
  temp_split <- unlist(strsplit(names(all_prosodic_shapes_stressed_rv_unique_table[i]), split=" "))
  temp_length <- length(temp_split)
  temp_stress_position <- grep("1", temp_split)
  matrix_type_udatta_positions[temp_length, temp_stress_position] <- (matrix_type_udatta_positions[temp_length, temp_stress_position] + all_prosodic_shapes_stressed_rv_unique_table[i])
}

type_no_stress_positions <- rep(0, 8)
for(i in 1:length(all_prosodic_shapes_unstressed_rv_unique_table)){
  temp_split <- unlist(strsplit(names(all_prosodic_shapes_unstressed_rv_unique_table[i]), split=" "))
  temp_length <- length(temp_split)
  type_no_stress_positions[temp_length] <- (type_no_stress_positions[temp_length] + all_prosodic_shapes_unstressed_rv_unique_table[i]) 
}

## C. Skt. frequency type data
  # H1 X: 1923
  # L1 V(C) 436
  # L V:(C) 434
  # Heavy penult: 5239
    # Light antepenult in trisyllables: 846
  # Heavy antepenult: 1929
  # Heavy preantepenult: 126
  # initial light 4 syll: 132
  # light antepenult five syll or more: 5

## DTS Left frequency type data
  # Heavy initial syllable: 5337
  # Heavy final, disyllables: 434+338
  # Heavy penult: 1684
    # Heavy final, trisyllables: 369 +253
  # Heavy antepenult: 599
    # Heavy final, quadrisyllables: 21 + 24
  # Heavy post-antepenult: 19
    # Heavy final, 5-syllable: 0
  # No heavy syllables on initial
    # Disyllable 278
    # 3: 116
    # 4: 8
    # 5: 



## DTS Right
  # Heavy final: 3529+2783
  # Heavy penult: 1556
  # Heavy antepenult: 234
  #Heavy preantepenult: 6
  # Heavy 5-to-last: 9
  # Default right if no heavy
    #Disyllable: 58
    # Trisyllable:24

# Left iamb
  # Initial heavy: 5337
  # Heavy final in disyllable: 338+434
  # Heavy peninitial: 1684
  # Light peninitial: 1168

# Right trochee
  # Heavy final: 3529+2783
  # penult where final is light" 1834

# C Lat.
  # Initial disyllables: 2600
  # Heavy penult in 3 or more: 3316
  # Antepenult when penult is light:2046

### Making lexically specific tableaux ###
token <- sample(stressed_tokens_rv, 1) # sample a token
token_shape <- paste("/", parse_prosodic_shape(token), "/", sep="") # parse its prosodic shape and set it between slashes
which_tableau <- which(names(tableaux[[1]]) == token_shape) # find the tableaux in the list of tableaux that matches that prosodic shape

types_vector <- c()
all_tableaux <- list()
if((token %in% types_vector) == FALSE){
  types_vector <- append(types_vector, token) # keep a running vector of types that have been seen
  all_tableaux[[(length(all_tableaux)+1)]] <- list(token, token_shape, tableaux[[1]][[which_tableau]], tableaux[[2]][[which_tableau]], tableaux[[3]][[which_tableau]] ) # add to a list object the name of the token, 
  #its prosodic shape, the vector with the preferred "winner", the matrix of violations, and the RIP candidates over prosodic shapes
  #names(all_tableaux[[(length(all_tableaux))]]) <- token_shape
  
  all_tableaux[[(length(all_tableaux))]][[4]][,18]
}
if((token %in% types_vector) == TRUE){
 tableux_number <- which(types_vector == token)
 
}





### Maxent learning ###
tableaux <- prepare_data("~/Documents/Dropbox/RyanTex/Sandell Habilitation/OT Grammars/RIP Feet/New_Berlin/RIP_Stress_Grammar_BERLIN.txt")
  # Element 1 in the list object is the input name and a vector of candidate probabilities
  # Element 2 in the list object is the tableaux: the full violation profiles of each candidate
  # Element 3 in the list object is the list of candidates.

# limited to 2- and 3-syllable words
limited_2_3 <- grep("^[HL]1? [HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))
# limited to 2-, 3-, 4-, and 5-syllable word
limited_2_3_4_5 <- grep("^[HL]1? [HL]?1? ?[HL]?1? ?[HL]?1? ?V:?1?C?$", names(all_prosodic_shapes_table))

# Find the tableaux where the UR matches the datum sampled. 
sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))

## Find the candidates that could be winners
possible_winners <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])


## BEGIN HERE ##
# Set initial weights at 10 for markedness, 0 for one faithfulness
# See file "MaxEnt_Learning_2.R" for functions
  # Set the weight of all constraints initially to 10
learner_weights <- rep(10,ncol(tableaux[[2]][[1]]))
  # Set the weight of IDENT-PROM to 0
learner_weights[length(learner_weights)] <- 0

learner_violations <- tableaux[[2]]

learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
# E.g., In tableaux number five, UR /L1 VC/, candidates 1-3 are all of equal probability, because they have equal numbers of markedness violations; 

simple_single_learning_loop <- function(number_iterations = 5000, learning_rate = 0.01){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  old_weights_ident_prom <- c(0)
  number_updates <- 0
  no_learning <- 0
   for(i in 1:number_iterations){
        # The first sampling behaviors will need to be changed and updated
    sampled_item <- sample(names(all_prosodic_shapes_table[limited_2_3_4_5]), 1, prob = as.numeric(all_prosodic_shapes_table[limited_2_3_4_5])/sum(all_prosodic_shapes_table[limited_2_3_4_5]))
    sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(tableaux[[3]]))
      # For 2nd generations
      # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    possible_winner_numbers <- grep(sampled_item, tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    
    learner_winner_number <- sample(c(1:nrow(tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
    learner_winner <- tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)])
      teacher_winner <- tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ]
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights)
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      number_updates <- number_updates + 1
      no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
   if(no_learning > (number_iterations * 0.01)){
     break
   }
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, old_weights_ident_prom, number_updates)
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names
  names(final_learner_outcome[[1]]) <- names(tableaux[[1]])
  for(i in 1:length(final_learner_outcome[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}
  # On 7000 iterations, there were only 2275 instances of constraint updating
  # Overall consistent tendency to promote NonFinalityFoot, NonInitialityFoot, Trochee, Iamb, and Ident-Prom. Everything else is demoted.
ptm <- proc.time()
out <- simple_single_learning_loop(number_iterations = 10000, learning_rate = 0.005)
proc.time() - ptm
names(out[[1]]) <- names(tableaux[[1]])

  # Intergenerationally, what changes are the probabilities of particular prosodic shapes: 

# I ran a single generation of learning with a single agent 10 times and then compared the constraint weights. I suspect that there will be little difference.
simulations_list <- list()
for(i in 1:10){
  temp_out <- simple_single_learning_loop(number_iterations = 10000, learning_rate = 0.005)
  simulations_list[[i]] <- temp_out
}

# Create data for next generation
new_generation_data <- c()
for(i in 1:length(limited_2_3_4_5)){
  prosodic_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[i]
  shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(tableaux[[3]]))
  temp_outputs <- sample(tableaux[[3]][[shape_tableau_number]], all_prosodic_shapes_table[limited_2_3_4_5][i], prob = out[[1]][[shape_tableau_number]], replace = TRUE)
  temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
  temp_outputs <- gsub("\\[", "", temp_outputs)
  temp_outputs <- gsub("\\]", "", temp_outputs)
  new_generation_data <- append(new_generation_data, temp_outputs)
}

new_generation_data_table <- table(new_generation_data)
  # But -- the number of different outputs is already vastly restricted in the second generation!
common_shapes <- all_prosodic_shapes_table[limited_2_3_4_5][which(names(all_prosodic_shapes_table[limited_2_3_4_5]) %in% names(new_generation_data_table))]
missing_shapes <- all_prosodic_shapes_table[limited_2_3_4_5][-(c(which(names(all_prosodic_shapes_table[limited_2_3_4_5]) %in% names(new_generation_data_table))))]
new_shapes <- new_generation_data_table[-c(which(names(new_generation_data_table) %in% names(all_prosodic_shapes_table[limited_2_3_4_5])))]
new_shapes_numbers <- c()
for(i in 1:length(new_shapes)){
  new_shapes_numbers <- append(new_shapes_numbers, which(names(new_generation_data_table) == names(new_shapes)[i]))
}
# remove the "new shapes" because you don't have tableaux for them
new_generation_data_table <- new_generation_data_table[-c(new_shapes_numbers)]
  # Some interesting behaviors:  H1 H L L VC >> H H1 L L VC
                                # H L H V:1C >> H L H1 V:1C
# Remove the missing inputs from the tableaux for the new generation
tableaux_to_remove <- c()
for(i in 1:length(names(all_prosodic_shapes_table[limited_2_3_4_5]))){
  temp_shape <- names(all_prosodic_shapes_table[limited_2_3_4_5])[i]
  if(temp_shape %in% names(new_generation_data_table)){
    
  }
  else{
    shape_tableau_number <- grep(paste("/", temp_shape, "/", sep=""),  names(tableaux[[3]]))
    tableaux_to_remove <- append(tableaux_to_remove, shape_tableau_number)
  }
}

new_generation_tableaux <- tableaux
new_generation_tableaux[[1]] <- new_generation_tableaux[[1]][-c(tableaux_to_remove)]
new_generation_tableaux[[2]] <- new_generation_tableaux[[2]][-c(tableaux_to_remove)]
new_generation_tableaux[[3]] <- new_generation_tableaux[[3]][-c(tableaux_to_remove)]
new_generation_tableaux[[4]] <- new_generation_tableaux[[4]][-c(tableaux_to_remove)]

# In the learning loop, replace "tableaux" with "new_generation_tableaux" and replace "all_prosodic_shapes_table[limited_2_3_4_5]" with "new_generation_data_table

# Set the weight of all constraints initially to 10
learner_weights <- rep(10,ncol(tableaux[[2]][[1]]))
# Set the weight of IDENT-PROM to 0
learner_weights[length(learner_weights)] <- 0

learner_violations <- new_generation_tableaux[[2]]

learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
simple_single_learning_loop_G2 <- function(number_iterations = 5000, learning_rate = 0.01){ # We could add a condition to decide when convergence is reached: if no learning has occurred after X consecutive samples, stop.
  old_weights_ident_prom <- c(0)
  number_updates <- 0
  no_learning <- 0
  for(i in 1:number_iterations){
    # The first sampling behaviors will need to be changed and updated
    sampled_item <- sample(names(new_generation_data_table), 1, prob = as.numeric(new_generation_data_table)/sum(new_generation_data_table))
    sampled_tableau_number <- grep(paste("/", sampled_item, "/", sep=""),  names(new_generation_tableaux[[3]]))
    if(length(sampled_tableau_number) == 0){
      next
    }
    # For 2nd generations
    # sampled_teacher_winner <- sample(tableaux[[3]][[sampled_tableau_number]], size =1, prob = out[[1]][[sampled_tableau_number]]) # where out = list of grammar from prev. generation
    
    possible_winner_numbers <- grep(sampled_item, new_generation_tableaux[[3]][[sampled_tableau_number]])
    #possible_winner_violations <- tableaux[[2]][[sampled_tableau_number]][c(possible_winner_numbers), ]
    
    learner_winner_number <- sample(c(1:nrow(new_generation_tableaux[[2]][[sampled_tableau_number]])), 1, prob = learner_cand_probs[[sampled_tableau_number]])
    learner_winner <- new_generation_tableaux[[2]][[sampled_tableau_number]][learner_winner_number, ] # get the violations of the learner's current winner
    if(learner_winner_number %in% possible_winner_numbers){
      # No learning occurs, because the output the learner made was compatible with the surface. Just store the current weight of Ident_Prom
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      no_learning <- no_learning + 1
    }
    else{
      # The learner thinks,"hm, what I want to say doesn't sound like the datum. I should check what the most likely thing that sounds like the datum would be!"
      teacher_winner_number <- sample(possible_winner_numbers, 1, prob = learner_cand_probs[[sampled_tableau_number]][c(possible_winner_numbers)])
      teacher_winner <- new_generation_tableaux[[2]][[sampled_tableau_number]][teacher_winner_number, ]
      # Update constraint weights
      learner_weights <- update_rule(teacher_winner, learner_winner, rate = learning_rate, learner_weights)
      old_weights_ident_prom <- append(old_weights_ident_prom, learner_weights[17])
      # Update candidate probabilities
      learner_cand_probs <- initial_learner_candidate_probs(learner_violations, learner_weights)
      number_updates <- number_updates + 1
      no_learning <- 0
    }
    # Store the final candidate probabilties and weights in a list
    if(no_learning > (number_iterations * 0.01)){
      break
    }
  }
  final_learner_outcome <- list(learner_cand_probs, learner_weights, old_weights_ident_prom, number_updates)
  names(final_learner_outcome[[2]]) <- tableaux[[5]] # Add names of constraints
  names(final_learner_outcome[[1]]) <- names(new_generation_tableaux[[1]])
  for(i in 1:length(new_generation_tableaux[[1]])){
    names(final_learner_outcome[[1]][[i]]) <- new_generation_tableaux[[3]][[i]]
  }
  return(final_learner_outcome)
}

out_G2 <- simple_single_learning_loop_G2(number_iterations = 10000, learning_rate = 0.005)

new_generation_data_2 <- c()
for(i in 1:length(new_generation_data_table)){
  prosodic_shape <- names(new_generation_data_table )[i]
  shape_tableau_number <- grep(paste("/", prosodic_shape, "/", sep=""),  names(new_generation_tableaux[[3]]))
  temp_outputs <- sample(new_generation_tableaux[[3]][[shape_tableau_number]], new_generation_data_table[i], prob = out_G2[[1]][[shape_tableau_number]], replace = TRUE)
  temp_outputs <- gsub(" \\\\->.*$", "", temp_outputs)
  temp_outputs <- gsub("\\[", "", temp_outputs)
  temp_outputs <- gsub("\\]", "", temp_outputs)
  new_generation_data_2 <- append(new_generation_data_2, temp_outputs)
}

new_generation_data_table_2 <- table(new_generation_data_2)

missing_shapes <- new_generation_data_table[-(c(which(names(new_generation_data_table) %in% names(new_generation_data_table_2))))]
new_shapes_2 <- new_generation_data_table_2[-c(which(names(new_generation_data_table_2) %in% names(new_generation_data_table)))]

# Now try to iterate what you have achieved here. Note that in the second generation, substantially less learning occurs -- the constraints remain closer to the initial state


  # Summary of basic simulation: With enough data -- if weights are permitted to move much closer to convergence -- it is possible to generate a set of weights that will predict 
    # winners that fit the input data with high probability.
# There are 108830 tokens of polysyllabic words (= 84%)
# Stressed monosyllables aren't common, but disyllables make up almost half (!) of the remaining words.
  # Your next step is to add tableaux for 4- and 5-syllable words
  # Then maybe add to the code to track the probabilities of candidates over the course of learning.




### COLLECTING 4- AND 5-SYLLABLE FORMS and BUILDING TABLEAUX ###
length_4_syll <- c()
length_5_syll <- c()
for(shape in names(all_prosodic_shapes_table)){
  temp_split <- unlist(strsplit(shape, split = " "))
  if(length(temp_split) == 4){
    length_4_syll <- append(length_4_syll, shape)
  }
  if(length(temp_split) == 5){
    length_5_syll <- append(length_5_syll, shape)
  }
}

length_4_5_syll <- c(length_4_syll, length_5_syll)



# I'm assuming that all inputs are 4 or 5 syllables
generate_foot_parses <- function(prosodic_shape){
  #prosodic_shape is assumed to be between / /
  #left_side <- gsub("\\[", prosodic_shape)
  #left_side <- gsub("\\]", prosodic_shape)
  
  prosodic_shape <- gsub("1", "", prosodic_shape)
  foot_parses_vector <- c()
  
  temp_split <- unlist(strsplit(prosodic_shape, split = " "))
  if(length(temp_split) == 4){
      # monosyllabic parses, plus different options for final consonants. If it is in a foot, it gets stress!
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1?", "\\(\\11\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1?", " \\(\\11\\) \\2", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? V", " \\1 \\(\\21\\) V", prosodic_shape) )
    if(length(grep("C$", prosodic_shape)) == 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
    }
    if(length(grep("C$", prosodic_shape)) > 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11\\)C", prosodic_shape) )
    }
    #disyllabic parses
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1?", "\\(\\11 \\2\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1?", "\\(\\1 \\21\\)", prosodic_shape) )
    
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])", " \\(\\11 \\2\\)", prosodic_shape) )
    foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])", " \\(\\1 \\21\\)", prosodic_shape) )
    
    if(length(grep("C$", prosodic_shape)) == 0){
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? (V:?)", " \\(\\11 \\2\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
    }
    if(length(grep("C$", prosodic_shape)) > 0){
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2\\)C", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21C\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21)C", prosodic_shape) )
    }
  }
    
    if(length(temp_split) == 5){
      # monosyllabic parses, plus different options for final consonants. If it is in a foot, it gets stress!
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1?", "\\(\\11\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\(\\11\\) \\2 \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\1 \\(\\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL])1? ([HL])1?", " \\1 \\2 \\(\\31\\)", prosodic_shape) )
      #foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? V", " \\(\\11\\) V", prosodic_shape) )
      if(length(grep("C$", prosodic_shape)) == 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?$", "\\(\\11\\)", prosodic_shape) )
      }
      if(length(grep("C$", prosodic_shape)) > 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?)1?C$", "\\(\\11\\)C", prosodic_shape) )
      }
      #disyllabic parses
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\(\\11 \\2\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\(\\1 \\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\1 \\(\\21 \\3\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub("^([HL])1? ([HL])1? ([HL])1?", "\\1 \\(\\2 \\31\\)", prosodic_shape) )
      
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\(\\11 \\2\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\(\\1 \\21\\) \\3", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\1 \\(\\21\\ \\3\\)", prosodic_shape) )
      foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? ([HL]) ([HL])1?", " \\1 \\(\\2 \\31\\)", prosodic_shape) )
      
      if(length(grep("C$", prosodic_shape)) == 0){
        foot_parses_vector <- append(foot_parses_vector, gsub(" ([HL])1? (V:?)", " \\(\\11 \\2\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("(V:?1?)$", "\\(\\11\\)", prosodic_shape) )
      }
      if(length(grep("C$", prosodic_shape)) > 0){
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\11 \\2\\)C", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21C\\)", prosodic_shape) )
        foot_parses_vector <- append(foot_parses_vector, gsub("([HL]) (V:?)1?C$", "\\(\\1 \\21)C", prosodic_shape) )
      }      
    }
  
  return(foot_parses_vector)
}

# Generate a list containing the foot parses for all of the items in the vector length_4_5_syll
list_4_5_syllable_parses <- list()
for(i in 1:length(length_4_5_syll)){
  temp_parses <- generate_foot_parses(length_4_5_syll[i])
  list_4_5_syllable_parses[[i]] <- temp_parses
}
names(list_4_5_syllable_parses) <- length_4_5_syll


generate_stress_positions <- function(prosodic_shape){
  stress_positions_vector <- c()
  temp_split <- unlist(strsplit(prosodic_shape, split = " "))
  temp_stressless <- gsub("1", "", prosodic_shape)
  temp_stressless_split <- unlist(strsplit(temp_stressless, split = " "))
  for(i in 1:length(temp_stressless_split)){
    temp_stressed_syll <- gsub("^([HLV]:?)", "\\11", temp_stressless_split[i])
    temp_stressless_split[i] <- temp_stressed_syll
    new_stressed <- paste(temp_stressless_split, collapse = " ")
    new_stressed <- gsub("^(.)", "\\[\\1", new_stressed)
    new_stressed <- gsub("(.)$", "\\1\\]", new_stressed)
    stress_positions_vector <- append(stress_positions_vector, new_stressed)
    temp_stressless <- gsub("1", "", prosodic_shape)
    temp_stressless_split <- unlist(strsplit(temp_stressless, split = " "))
  }
  return(stress_positions_vector)
}

list_4_5_syllable_stresses <- list()
for(i in 1:length(length_4_5_syll)){
  temp_stresses <- generate_stress_positions(length_4_5_syll[i])
  list_4_5_syllable_stresses[[i]] <- temp_stresses
}
names(list_4_5_syllable_stresses) <- length_4_5_syll

# list_4_5_syllable_parses, list_4_5_syllable_stresses

make_surface_to_parse_pairs <- function(list_parses, list_stresses){
  stress_to_parse_list <- list()
  for(i in 1:length(list_parses)){
    temp_pair_vector <- c()
    for(j in 1:length(list_4_5_syllable_parses[[i]])){
      temp_split <- unlist(strsplit(list_4_5_syllable_parses[[i]][[j]], split = " "))
      stressed_number <- grep("1", temp_split)
      matching_stressed_form <- list_stresses[[i]][[stressed_number]]
      temp_pair <- paste(matching_stressed_form, "\\->", list_4_5_syllable_parses[[i]][[j]], sep = " ")
      temp_pair_vector <- append(temp_pair_vector, temp_pair)
    }
    stress_to_parse_list[[i]] <- temp_pair_vector
  }
  return(stress_to_parse_list)
}
names(big_stress_parse_list) <- length_4_5_syll


#Parse-Seg	Parse-Syllable	FtBin	NonFinality	NonFinality-Foot	NonInitiality-Ft	WSP	Trochee	Iamb	All-Ft-L	All-Ft-R	Leftmost	Rightmost	Align-wd-L	Align-wd-R	Rh-Contour	Ident-Prom
generate_violation_profile <- function(prosodic_shape){ # Currently (18.01.2023) set for dealing with forms with CLASH
  default_violations <- rep(0, 17)
  prosodic_shape <- gsub(".*> ", "", prosodic_shape)
  
  # For Parse-Seg
  if(length(grep("\\)C", prosodic_shape)) > 0){
    default_violations[1] <- 1
  }
  
  # For Parse-Syllable
  temp_syllables <- unlist(strsplit(prosodic_shape, split = " "))
  temp_syllables_left <-  grep("\\([HLV]", temp_syllables)
  temp_syllables_right <-  grep("[HLV]:?1?C?\\)", temp_syllables)
  all_footed_syllables <- unique(c(temp_syllables_left, temp_syllables_right))
  default_violations[2] <- length(temp_syllables) - length(all_footed_syllables)
  # if(length(grep("\\([HLV]:?1C?\\)", prosodic_shape)) > 0 ){
  #   temp_length <- length(unlist(strsplit(prosodic_shape, split = " ")))
  #   default_violations[2] <- temp_length -1
  # }
  # else{
  #   temp_length <- length(unlist(strsplit(prosodic_shape, split = " ")))
  #   default_violations[2] <- temp_length - 2
  # }
  
  # For FtBin
  if(length(grep("\\([LV]1\\)", prosodic_shape)) > 0){
    default_violations[3] <- 1
  }
  
  # For NonFinality
  if(length(grep("V:?1", prosodic_shape)) > 0){
    default_violations[4] <- 1
  }
  
  # For NonFinality-Foot
  if(length(grep("\\)$", prosodic_shape)) > 0 ){
    default_violations[5] <- 1
  }
  
  # For NonInitiality-Ft
  if(length(grep("^\\(", prosodic_shape)) > 0 ){
    default_violations[6] <- 1
  }
  
  # For WSP
  unstressed_heavy <- c("H", "H)", "(H",  "V:", "V:)", "VC", "VC)", "V:C", "V:)C", "V:C)")
  for(i in unlist(strsplit(prosodic_shape, split = " "))){
    if(i %in% unstressed_heavy){
      default_violations[7] <- default_violations[7] + 1
    }
  }
  
  # For Trochee
  if(length(grep("\\([HL] [HL]1\\)", prosodic_shape)) > 0){
    default_violations[8] <- 1
  }
  
  # For Iamb
  if(length(grep("\\([HL] [HLV]:?\\)", prosodic_shape)) > 0){
    default_violations[9] <- 1
  }
  
  # For All-Ft-L
  left_edge <- grep("\\(", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[10] <- sum(abs(1 - left_edge))
  
  # For All-Ft-R
  right_edge <- grep("\\)", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[11] <- sum(length(unlist(strsplit(prosodic_shape, split = " "))) - right_edge)
  
  # For Leftmost
  stressed_position <- grep("1", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[[12]] <- abs(1 - stressed_position)
  
  # For Rightmost
  stressed_position <- grep("1", unlist(strsplit(prosodic_shape, split = " ")))
  default_violations[13] <- length(unlist(strsplit(prosodic_shape, split = " "))) - stressed_position
  
  # For Align-Wd-L
  if(length(grep("^\\(", prosodic_shape)) == 0){
    default_violations[14] <- 1
  }
  
  # For Align-Wd-R
  if(length(grep("\\)$", prosodic_shape)) == 0){
    default_violations[15] <- 1
  }
  
  
  # For Rh-Contour
    # do as regex
  rh_shapes <- "(\\(L L1\\))|(\\(L V1\\))|(\\(H1 L\\))|(\\(H1 V\\))"
  if(length(grep(rh_shapes, prosodic_shape)) > 0){
    default_violations[16] <- 1
  }
  
  # For Clash -- not very good
  #temp_split <- unlist(strsplit(prosodic_shape, " "))
  temp_match <- gregexpr("\\(H1?\\) \\(H1?\\)", prosodic_shape, fixed=TRUE)
  default_violations[17] <- (length(temp_match[[1]]))
  #temp_split <- unlist(strsplit(prosodic_shape, ")"))
  #default_violations[17] <- (length(temp_split) - 2)
  
  # if(length(grep("\\) \\(", prosodic_shape)) > 0){
  #   default_violations[17] <- 1
  # }
  
  # For Ident-Prom
  #temp_UR <- gsub("\\/", "", UR)
  #temp_UR <- gsub("\\\ ", "", temp_UR)
  #temp_prosodic_shape <- gsub("\\(", "", prosodic_shape)
  #temp_prosodic_shape <- gsub("\\)", "", temp_prosodic_shape)
  #if(temp_UR != temp_prosodic_shape){
    #default_violations[17] <- 1
  #}
  return(default_violations)
}


big.data.frame <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
# Generate the big tableau with all violations
for(i in 1:length(list_4_5_syllable_parses)){
  UR <- names(list_4_5_syllable_parses)[i]
  for(j in 1:length(list_4_5_syllable_parses[[i]])){
    violation_profile <- generate_violation_profile(UR, list_4_5_syllable_parses[[i]][j])
    if(j > 1){
      UR <- ""
    }
    else{
      UR <- gsub("^(.)", "/\\1", UR)
      UR <- gsub("(.)$", "\\1/", UR)
    }
    input_parse_pair <- big_stress_parse_list[[i]][j]
    if(j == 1){
      freq <- 1
    }
    else{
      freq <- ""
    }
    temp_vector <- c(UR, input_parse_pair, freq, violation_profile)
    big.data.frame <- rbind(big.data.frame, temp_vector)
  }

}
final_frame <- big.data.frame[-c(1), ]

# 
write.table(final_frame, "Syllable_4_5_Tableaux.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)

### Creating new candidates with adjacent heavy syllables and stress clash
my_new_tableaux <- read.table("RIP_Vedic_No_Lexical_Final_V2_fixed.txt", skip=1, header=T, sep="\t") # read in tableaux


CLASH <- rep(0, length(my_new_tableaux$Trochee))
my_data <- cbind(my_new_tableaux, CLASH)

# Add a first set of clashes with candidates, then if there are more left, add 

potential_clash_H_left <- grep("(H|L L) \\(H1\\)", my_data$X.1)
potential_clash_H_right <- grep("\\(H1\\) (H|L L)", my_data$X.1)
potential_clash_L_left <- grep("(H|L L) \\(L1 L\\)", my_data$X.1)
potential_clash_L_right <- grep("\\(L L1\\) (H|L L)", my_data$X.1)

potential_clashes <- c(potential_clash_H_left, potential_clash_H_right, potential_clash_L_left, potential_clash_L_right)

clash_outputs <- c()
clash_basis_original_indices <- c()
for(candidate in 1:length(potential_clashes)){
  if(length(grep("(H|L L) \\(H1\\)", my_data$X.1[potential_clashes[candidate]])) > 0){
    temp_cand <- gsub("(H|L L) \\(H1\\)", "\\(\\1\\) (H1)", my_data$X.1[potential_clashes[candidate]])
    clash_outputs <- append(clash_outputs, temp_cand)
    clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
  if(length(grep("\\(H1\\) (H|L L)", my_data$X.1[potential_clashes[candidate]])) > 0){
    temp_cand <- gsub("\\(H1\\) (H|L L)", "\\(H1\\) \\(\\1\\)", my_data$X.1[potential_clashes[candidate]])
    clash_outputs <- append(clash_outputs, temp_cand)
    clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
  if(length(grep("(H|L L) \\(L1 L\\)", my_data$X.1[potential_clashes[candidate]])) > 0 ){
    temp_cand <- gsub("(H|L L) \\(L1 L\\)", "\\(\\1\\) \\(L1 L\\)", my_data$X.1[potential_clashes[candidate]])
    clash_outputs <- append(clash_outputs, temp_cand)
    clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
  if(length(grep("\\(L L1\\) (H|L L)", my_data$X.1[potential_clashes[candidate]])) > 0){
    temp_cand <- gsub("\\(L L1\\) (H|L L)", "\\(L L1\\) \\(\\1\\)", my_data$X.1[potential_clashes[candidate]])
    clash_outputs <- append(clash_outputs, temp_cand)
    clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
}


potential_double_clash_H_left <- grep("(H|L L) \\(H1\\)", clash_outputs)
potential_double_clash_H_right <- grep("\\(H1\\) (H|L L)", clash_outputs)
#potential_double_clash_L_left <- grep("(H|L L) \\(L1 L\\)", clash_outputs, value=TRUE) # impossible
#potential_double_clash_L_right <- grep("\\(L L1\\) (H|L L)", clash_outputs, value=TRUE)

potential_double_clashes <- c(potential_double_clash_H_left, potential_double_clash_H_right)

double_clash_outputs <- c()

# Here create double clashes
for(candidate in 1:length(potential_double_clashes)){
  if(length(grep("(H|L L) \\(H1\\)", clash_outputs[potential_double_clashes[candidate]])) > 0){
    temp_cand <- gsub("(H|L L) \\(H1\\)", "\\(\\1\\) (H1)", clash_outputs[potential_double_clashes[candidate]])
    double_clash_outputs <- append(double_clash_outputs, temp_cand)
    #clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
  if(length(grep("\\(H1\\) (H|L L)", clash_outputs[potential_double_clashes[candidate]])) > 0){
    temp_cand <- gsub("\\(H1\\) (H|L L)", "\\(H1\\) \\(\\1\\)", clash_outputs[potential_double_clashes[candidate]])
    double_clash_outputs <- append(double_clash_outputs, temp_cand)
    #clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
}

potential_triple_clash_H_left <- grep("(H|L L) \\(H\\)", double_clash_outputs)
potential_triple_clash_H_right <- grep("\\(H\\) (H|L L)", double_clash_outputs)
potential_triple_clashes <- c(potential_triple_clash_H_left , potential_triple_clash_H_right)

triple_clash_outputs <- c()

# Here create triple clashes
for(candidate in 1:length(potential_triple_clashes)){
  if(length(grep("(H|L L) \\(H\\)", double_clash_outputs[potential_triple_clashes[candidate]])) > 0){
    temp_cand <- gsub("(H|L L) \\(H\\)", "\\(\\1\\) (H)", double_clash_outputs[potential_triple_clashes[candidate]])
    triple_clash_outputs <- append(triple_clash_outputs, temp_cand)
    #clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
  if(length(grep("\\(H\\) (H|L L)", double_clash_outputs[potential_triple_clashes[candidate]])) > 0){
    temp_cand <- gsub("\\(H\\) (H|L L)", "\\(H\\) \\(\\1\\)", double_clash_outputs[potential_triple_clashes[candidate]])
    triple_clash_outputs <- append(triple_clash_outputs, temp_cand)
    #clash_basis_original_indices <- append(clash_basis_original_indices, potential_clashes[candidate])
  }
}

all_clash_outputs <- c(clash_outputs, double_clash_outputs, triple_clash_outputs)

clash_violations_table <- as.data.frame(rbind(c("", "", rep(0, 17))))
for(i in 1:length(all_clash_outputs)){
  temp_violations <- generate_violation_profile(all_clash_outputs[i])
  temp_row <- c("", all_clash_outputs[i], temp_violations)
  clash_violations_table <- rbind(clash_violations_table, temp_row)
}
clash_violations_table <- clash_violations_table[-c(1), ]
write.table(clash_violations_table, "Clash_Violation_Candidates_better.txt", quote=FALSE, row.names = FALSE, sep="\t")

clash_unique_table <- unique(clash_violations_table)

# /H H1 H H V/
grep("H (H1)", my_data$V2)
#[(H) (H1) H H V]

grep("(H1) H", my_data$V2)
#[(H) (H1) (H) H V]

grep("\\(H\\) H", my_data$V2)
##[(H) (H1) (H) H V]

gsub("H", "(H)")

