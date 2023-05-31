
# USAGE: make the functions RCD(), MIRCD(), and make_ranked_tableau() available in your current working environment
  # by setting your cursor on the first line of each function and clicking "Run".
# RCD and MIRCD accept tab-separated-values files of the OTSoft format type. Anything else will result in errors.
  # Both of these functions output a list object, where each successive element in the list is a constraint stratum.
# make_ranked_tableau accepts a list object with a constraint ranking (the output of a successful RCD/MIRCD)
  # and a tableau.

# Set your working directory to a folder containing tab-separated values of the OTSoft formatting type.

# RCD Algorthim after Becker 2009; MIRCD after Yates & Zukoff 2018.
# Note for reading the Comparative Tableau: negative numbers are winner-preferrers; positive numbers are loser-preferrers.
# A table (data frame) with the constraints ordered according to a ranking can be made with the function make_ranked_tableau at the bottom of this file.

setwd("~/Desktop/OT Grammars")
#OT_Table_File = "Pre-Hittite_Test.txt"
# Assumes OTSoft-structured file
# Not suitable for stochastic data (obviously).
# Example usage: my_output <- RCD("Pre-Hittite_Test.txt")
RCD <- function(OT_Table_File){
  raw_file <- read.table(file =  OT_Table_File, sep="\t", header=F, fill=T, stringsAsFactors = F, skip=2, quote=NULL) # read in an OTSoft-formatted file, ignoring the first two rows with constaint names
  raw_file[is.na(raw_file)] <- 0 # replace all empty constraint evaluations with 0
  header <- unlist(strsplit(scan(OT_Table_File, what="char", sep="\n")[1], split="\t")) # get the constraint names from the same files
  colnames(raw_file) <- header # add the constraint names as a header to the candidate data frame
  constraint_names <- header[4:length(header)] # get the names of the constraints
  number_of_constraints <- length(constraint_names) # get the number of constraints
  
  
  number_inputs <- nrow(raw_file) # Get the total number of candidates
  
  current_input <- c()
  for(i in 1:nrow(raw_file)){
    if(raw_file[i,1] != ""){
      current_input <- raw_file[i, 1]
    }
    else{
      raw_file[i, 1] <- current_input
    }
  }
  
  # The frequency column with specified winners should always be the third column in an OTSoft file.
  winners = raw_file[which(raw_file[, 3] == 1), ] # extract the winning candidates from the big tableau
  losers = raw_file[which(raw_file[, 3] == 0), ]

  if(nrow(winners) < length(unique(raw_file[, 1]))){
    return(print("Error: Not all inputs have a single defined winner!"))
  }
  
  if(nrow(winners) > length(unique(raw_file[, 1]))){
    return(print("Error: Not all inputs have a single defined winner!"))
  }
  
  complete_comparative_tableau <- data.frame() # intialize empty data frame for the comparative tableau
  for(i in 1:nrow(winners)){
    current_winner <- winners[i, ]
    current_losers <- losers[which(losers[,1] == as.character(current_winner[1])), ]
    for(j in 1:nrow(current_losers)){
      current_loser <- current_losers[j, ]
      current_winner_loser_comparison <- current_winner[4:(number_of_constraints+3)] - current_loser[4:(number_of_constraints+3)] # add three since the number of constraint columns starts from column 4
      winner_loser_pair <- paste(current_winner[2], " ~ ", current_loser[2], sep="")
      current_comparative_tableau <- cbind(current_winner[1], winner_loser_pair, current_winner_loser_comparison)
      complete_comparative_tableau <- rbind(complete_comparative_tableau, current_comparative_tableau)
    }
  }
  


  #Identify Winner-Loser pairs that are distinctive
  unique_candidates <- complete_comparative_tableau[rownames(unique(complete_comparative_tableau[, 3:length(colnames(complete_comparative_tableau))])), ]
  informative_candidates <- c()
  
  # FOR loop for identifying informative candidates, i.e., throwing out any with only 0s
  for(i in 1:nrow(unique_candidates)){
    if(sum(abs(unique_candidates[i, (3:(number_of_constraints+2))])) > 0){
      informative_candidates <- append(informative_candidates, i)
    } 
  }
  informative_candidates <- unique_candidates[informative_candidates, ]
  
  
  final_ranking <- list()
  original_number_of_constraints <- number_of_constraints
  
  iterations <- 1
  # CONSTRAINT DEMOTION LOOP -- Following Becker (2009), put a constraint in a stratum if it has no losers and at least one winner
  while(length(constraint_names) > 0){
    # Initialize empty vectors of the current stratum, plus constraints and winner-loser-pairs that should be removed
    current_stratum <- c()
    constraints_to_remove <- c()
    pairs_to_remove <- c()
    # Iterate over the constraint columns in informative_candidates
    for(i in 3:(number_of_constraints+2)){
      if(length(which(informative_candidates[, i] > 0)) == 0 & length(which(informative_candidates[, i] < 0)) >= 1){ # Check whether a constraint has no losers and at least one winner.
        current_stratum <- append(current_stratum, colnames(informative_candidates)[i]) # Install that constraint in the current stratum.
        constraints_to_remove <- append(constraints_to_remove, i) # Add that constraint to the list of constraints to remove.
        # Add winner-loser pairs that are decided by the current constraint to list of pairs to remove
        for(j in 1:nrow(informative_candidates)){
          if(informative_candidates[j, i] < 0){
            pairs_to_remove <- append(pairs_to_remove, j)
          } # columns are i, rows are j
        }
      }
    }
    # Since a W-L pair might be decided by two constraints in the same stratum, limit pairs_to_remove to unique numbers
    if(length(current_stratum) == 0){
      break
    }
    pairs_to_remove <- unique(pairs_to_remove)
    
    informative_candidates <- informative_candidates[, -c(constraints_to_remove)]
    informative_candidates <- informative_candidates[-c(pairs_to_remove), ]
    
    # Store the current stratum in the list of final rankings
    final_ranking[[iterations]] <- current_stratum
    
    constraint_names <- constraint_names[-c(which(constraint_names %in% current_stratum))] # Remove names of constraints in the current stratum from the list of constraint names.
    number_of_constraints <- length(constraint_names)
    # If all of the Winner-Loser pairs have been explained, put any remaining constraints in the lowest stratum and break the loop
    if(nrow(informative_candidates) == 0){
      final_ranking[[iterations+1]] <- constraint_names
      constraint_names <- c()
      break
    }
    # Logically, RCD requires a number of iterations at most equal to the number of constraints. 
    # If an iteration has just been carried out which exceeds the number of constraints, break the while loop.
    if(iterations > original_number_of_constraints){
      break
    }
    iterations <- iterations + 1
    if(length(constraint_names) == 0){
      break
    }
  }
  
  if(length(constraint_names) == 0){
    return(final_ranking)
  }
  else{
    crash_list <- list("RCD Crashed!", complete_comparative_tableau, constraint_names, final_ranking)
    names(crash_list) <- c("Error Message", "Comparative Tableau", "Unranked Constraints", "Ranked Constraints")
    return(list(crash_list))
  }
  
}




# Tested on the Anatolian case -- works for Pre-Hittite as described in Yates & Zukoff.
MIRCD <- function(OT_Table_File){
  raw_file <- read.table(file =  OT_Table_File, sep="\t", header=F, fill=T, stringsAsFactors = F, skip=2, quote=NULL) # read in an OTSoft-formatted file, ignoring the first two rows with constaint names
  raw_file[is.na(raw_file)] <- 0 # replace all empty constraint evaluations with 0
  header <- unlist(strsplit(scan(OT_Table_File, what="char", sep="\n")[1], split="\t")) # get the constraint names from the same files
  colnames(raw_file) <- header # add the constraint names as a header to the candidate data frame
  constraint_names <- header[4:length(header)] # get the names of the constraints
  number_of_constraints <- length(constraint_names) # get the number of constraints
  
  
  number_inputs <- nrow(raw_file)
  
  current_input <- c()
  for(i in 1:nrow(raw_file)){
    if(raw_file[i,1] != ""){
      current_input <- raw_file[i, 1]
    }
    else{
      raw_file[i, 1] <- current_input
    }
  }
  
  # The frequency column with specified winners should always be the third column in an OTSoft file.
  winners = raw_file[which(raw_file[, 3] == 1), ] # extract the winning candidates from the big tableau
  losers = raw_file[which(raw_file[, 3] == 0), ]
  
  # Check that there are at least as many winners as inputs -- could potentially have problems if an input has more than one winner.
  if(nrow(winners) < length(unique(raw_file[, 1]))){
    return(print("Error: Not all inputs have a single defined winner!"))
  }
  
  if(nrow(winners) > length(unique(raw_file[, 1]))){
    return(print("Error: Not all inputs have a single defined winner!"))
  }
  
  
  complete_comparative_tableau <- data.frame() # intialize empty data frame for the comparative tableau
  for(i in 1:nrow(winners)){
    current_winner <- winners[i, ]
    current_losers <- losers[which(losers[,1] == as.character(current_winner[1])), ]
    for(j in 1:nrow(current_losers)){
      current_loser <- current_losers[j, ]
      current_winner_loser_comparison <- current_winner[4:(number_of_constraints+3)] - current_loser[4:(number_of_constraints+3)] # add three since the number of constraint columns starts from column 4
      winner_loser_pair <- paste(current_winner[2], " ~ ", current_loser[2], sep="")
      current_comparative_tableau <- cbind(current_winner[1], winner_loser_pair, current_winner_loser_comparison)
      complete_comparative_tableau <- rbind(complete_comparative_tableau, current_comparative_tableau)
    }
  }
  
  
  
  #Identify Winner-Loser pairs that are distinctive
  unique_candidates <- complete_comparative_tableau[rownames(unique(complete_comparative_tableau[, 3:length(colnames(complete_comparative_tableau))])), ]
  informative_candidates <- c()
  
  # FOR loop for identifying informative candidates, i.e., throwing out any with only 0s
  for(i in 1:nrow(unique_candidates)){
    if(sum(abs(unique_candidates[i, (3:(number_of_constraints+2))])) > 0){
      informative_candidates <- append(informative_candidates, i)
    } 
  }
  informative_candidates <- unique_candidates[informative_candidates, ]
  
  
  final_ranking <- list()
  original_number_of_constraints <- number_of_constraints # Set this original number to compare iterations for breaking out of the while loop
  
  iterations <- 1
  # CONSTRAINT DEMOTION LOOP -- Following Becker (2009), put a constraint in a stratum if it has no losers and at least one winner
  while(length(constraint_names) > 0){
    # Initialize empty vectors of the current stratum, plus constraints and winner-loser-pairs that should be removed
    current_stratum <- c()
    maximally_informative_constraints <- c()
    constraints_to_remove <- c()
    pairs_to_remove <- c()
    # Iterate over the constraint columns in informative_candidates
    for(i in 3:(number_of_constraints+2)){
      if(length(which(informative_candidates[, i] > 0)) == 0 & length(which(informative_candidates[, i] < 0)) >= 1){ # Check whether a constraint has no losers and at least one winner.
        current_stratum <- append(current_stratum, colnames(informative_candidates)[i]) # Install that constraint in the current stratum.
        constraints_to_remove <- append(constraints_to_remove, i) # Add that constraint to the list of constraints to remove.
        # Add winner-loser pairs that are decided by the current constraint to list of pairs to remove
        for(j in 1:nrow(informative_candidates)){
          if(informative_candidates[j, i] < 0){
            pairs_to_remove <- append(pairs_to_remove, j)
          } # columns are i, rows are j
        }
      }
    }
    if(length(current_stratum) > 1){
      constraints_to_compare <- informative_candidates[, constraints_to_remove]
      # Change values greater or less than 1 to 1 and -1
      constraints_to_compare[constraints_to_compare > 1] <- 1
      constraints_to_compare[constraints_to_compare < -1] <- -1
      constraint_max_scores <- lapply(abs(constraints_to_compare), sum)
      most_informative_constraint <- names(which(constraint_max_scores == max(unlist(constraint_max_scores))))
      
      # Adjust the current stratum, constraints to remove, and pairs to remove
      current_stratum <- most_informative_constraint
      constraints_to_remove <- which(colnames(informative_candidates) %in% most_informative_constraint)
      pairs_to_remove <- c()
      for(constraint in constraints_to_remove){ # Iterates over the constraints in constraints_to_remove in case of a tie for the most-informative constraint
        for(j in 1:nrow(informative_candidates)){
          if(informative_candidates[j, constraint] < 0){
            pairs_to_remove <- append(pairs_to_remove, j)
          }
        }
      }
      if(length(current_stratum) == 0){
        break
      }
      # Complete the removal of constraints and W-L pairs and store the current stratum.
      # Since a W-L pair might be decided by two constraints in the same stratum, limit pairs_to_remove to unique numbers
      pairs_to_remove <- unique(pairs_to_remove)
        
      informative_candidates <- informative_candidates[, -c(constraints_to_remove)]
      informative_candidates <- informative_candidates[-c(pairs_to_remove), ]
        
      # Store the current stratum in the list of final rankings
      final_ranking[[iterations]] <- current_stratum
        
      constraint_names <- constraint_names[-c(which(constraint_names %in% current_stratum))] # Remove names of constraints in the current stratum from the list of constraint names.
      number_of_constraints <- length(constraint_names)      
          
    }
    else{
      if(length(current_stratum) == 0){
        break
      }
    # Since a W-L pair might be decided by two constraints in the same stratum, limit pairs_to_remove to unique numbers
      pairs_to_remove <- unique(pairs_to_remove)
    
      informative_candidates <- informative_candidates[, -c(constraints_to_remove)]
      informative_candidates <- informative_candidates[-c(pairs_to_remove), ]
    
      # Store the current stratum in the list of final rankings
      final_ranking[[iterations]] <- current_stratum
    
      constraint_names <- constraint_names[-c(which(constraint_names %in% current_stratum))] # Remove names of constraints in the current stratum from the list of constraint names.
      number_of_constraints <- length(constraint_names)
    }
    # If all of the Winner-Loser pairs have been explained, put any remaining constraints in the lowest stratum and break the loop
    if(nrow(informative_candidates) == 0){
      final_ranking[[iterations+1]] <- constraint_names
      constraint_names <- c()
      break
    }
    # Logically, RCD requires a number of iterations at most equal to the number of constraints. 
    # If an iteration has just been carried out which exceeds the number of constraints, break the while loop.
    if(iterations > original_number_of_constraints){
      break
    }
    iterations <- iterations + 1
    if(length(constraint_names) == 0){
      break
    }
  }
  
  if(length(constraint_names) == 0){
    return(final_ranking)
  }
  else{
    crash_list <- list("MIRCD Crashed!", complete_comparative_tableau, constraint_names, final_ranking, informative_candidates)
    names(crash_list) <- c("Error Message", "Comparative Tableau", "Unranked Constraints", "Ranked Constraints", "Remaining Informative Candidates")
    return(list(crash_list))
  }
}

# USAGE: accepts a list of the form output by RCD() and MIRCD(), and a data frame object represented a tableau. 
  # Use the first line inside either the RCD or MICRD functions to read in a tableau file.

make_ranked_tableau <- function(ranking = list(), tableau = data.frame()){
  # Assumes that the constraint names in the ranking provided and the constraint names in the data frame provided are identical
  tableau_for_ranking <- tableau[, 1:3]
  for(i in 1:length(ranking)){
    stratum <- ranking[[i]]
    constraints_in_stratum <- tableau[, which(colnames(tableau) %in% stratum)]
    tableau_for_ranking <- cbind(tableau_for_ranking, constraints_in_stratum)
  }
  colnames(tableau_for_ranking) <- c("Input", "Candidate", "Frequency", unlist(ranking))
  return(tableau_for_ranking)
}


# MIRCD has the benefit of being more "efficient" in a sense: more of the outputs are decided through fewer high-ranking constraints.
# Comparison of RCD and MIRCD shows cases in which candidates put in the top-ranked stratum under RCD (because they have no losers)
  # end up the bottom stratum in MIRCD, because the relevant losers can be otherwise explained.
