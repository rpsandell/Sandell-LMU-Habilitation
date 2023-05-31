
### Generating winners for specific stress patterns for calculating SSEs in simulation results
for_my_overts <- simple_learning_loop_no_lexical(number_iterations = 1000, learning_rate = 0.1, number_generations = 1)


overt_shapes <- for_my_overts[[3]]$Output_Overt
input_overt_shapes_frame <- as.data.frame(cbind(for_my_overts[[3]]$Input_Shape, for_my_overts[[3]]$Output_Overt))



# Quantity-insensitive
# initial stress
default_vector <- rep(0, 503)
initial <-  default_vector 
initial[grep("^[LH]1 ", overt_shapes)] <- 1

#final stress
final <-  default_vector
final[grep(" V:?1C?$", overt_shapes)] <- 1

# peninitial stress
peninitial <- default_vector
peninitial[grep("^[LH] [LHV]:?1C?", overt_shapes)] <- 1

#penultimate stress
penult <- default_vector
penult[grep("[LH]1 V:?C?$", overt_shapes)] <- 1

#antepenultimate stress
antepenult <- default_vector
antepenult[grep("^[LH]1 V:?C?$", overt_shapes)] <- 1
antepenult[grep("[LH]1 [LH] V:?C?$", overt_shapes)] <- 1

# Unbounded quantity-sensitive
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H)|(V:)|(VC)", temp_split)
  if(length(grep("(H1)|(V:1)|(V1C)", temp_split)) > 1){
    print(overt_shapes[i])
  }
}


DTS_L <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes[i])) == 1){
      DTS_L[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      DTS_L[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == min(temp_match)){
        DTS_L[i] <- 1
      }
    }
  }
}

DTS_R <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("V1$", overt_shapes[i])) == 1){
      DTS_R[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      DTS_R[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == max(temp_match)){
        DTS_R[i] <- 1
      }
    }
  }
}



DTO_L <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("V1$", overt_shapes[i])) == 1){
      DTO_L[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      DTO_L[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == min(temp_match)){
        DTO_L[i] <- 1
      }
    }
  }
}

DTO_R <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes[i])) == 1){
      DTO_R[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      DTO_R[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == max(temp_match)){
        DTO_R[i] <- 1
      }
    }
  }
}


DTS_R_FSM <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("L1 V:?C?", overt_shapes[i])) == 1){
      DTS_R_FSM[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)", overt_shapes[i])) == 1){
      DTS_R_FSM[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)", overt_shapes[i])) == 1){
      if(grep("(H1)", temp_split) == max(temp_match)){
        DTS_R_FSM[i] <- 1
      }
    }
  }
}



DTO_R_FSM <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes[i])) == 1){
      DTO_R_FSM[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)", overt_shapes[i])) == 1){
      DTO_R_FSM[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)", overt_shapes[i])) == 1){
      if(grep("(H1)", temp_split) == max(temp_match)){
        DTO_R_FSM[i] <- 1
      }
    }
  }
}



## Other quantity-sensitive systems
C_Skt_Rule <- default_vector # A vector for the C Skt. rule can be added manually based on the vector for C. Lat. rule
# for(i in 1:length(overt_shapes)){
#   temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
#   if(length(temp_split == 2)){
#     if(length(grep("H1", overt_shapes[i])) == 1){
#       C_Skt_Rule[i] <- 1
#       next
#     }
#     else if(length(grep("H", overt_shapes[i])) == 0){
#       if(length(grep("V:1", overt_shapes[i])) == 1){
#         C_Skt_Rule[i] <- 1
#         next
#       }
#       else{
#         C_Skt_Rule[i] <- 1
#         next
#       }
#     }
#   }
#   if(length(temp_split) > 2){
#     if(temp_split[length(temp_split) - 1] == "H1"){
#       C_Skt_Rule[i] <- 1
#       next
#     }
#     else if(temp_split[length(temp_split) - 1] == "L"){
#       if(temp_split[length(temp_split) - 2] == "H1"){
#         C_Skt_Rule[i] <- 1
#         next
#       }
#       else if(length(temp_split) > 3){
#         if(temp_split[length(temp_split) - 3] == "H1"){
#           C_Skt_Rule[i] <- 1
#           next
#         }
#         else if(length(temp_split) == 4){
#           if(temp_split[length(temp_split) - 3] == "L1"){
#             C_Skt_Rule[i] <- 1
#             next
#           }
#         }
#         else if(length(temp_split) == 5){
#           if(temp_split[length(temp_split) - 2] == "L1"){
#             C_Skt_Rule[i] <- 1
#             next
#           }
#         }
#       }
#     }
#   }
# }



C_Lat_Rule <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  if(length(temp_split) == 2){
    if(length(grep("[LH]1", temp_split[1])) == 1){
      C_Lat_Rule[i] <- 1
    }
  }
  else{
    if(length(grep("H1", temp_split[length(temp_split) - 1])) == 1){
      C_Lat_Rule[i] <- 1
    }
    else{
      if(length(grep("L", temp_split[length(temp_split) - 1])) == 1){
        if(length(grep("H1", temp_split[length(temp_split) - 2])) == 1){
          C_Lat_Rule[i] <- 1
        }
        else{
          if(length(grep("L1", temp_split[length(temp_split) - 2])) == 1){
            C_Lat_Rule[i] <- 1
          }
        }
      }
    }
  }
}



initial_iambs <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  if(is.na(temp_split)){
    next
  }
  if(length(temp_split) == 2){
    if(temp_split[1] == "L"){
      initial_iambs[i] <- 1
    }
    else if(temp_split[1] == "H1"){
      initial_iambs[i] <- 1
    }
  }
  else{
    if(length(grep("H1", temp_split[1])) == 1){
      initial_iambs[i] <- 1
    }
    else if(temp_split[1] == "L"){
      if(length(grep("[LH]1", temp_split[2])) == 1){
        initial_iambs[i] <- 1
      }
    }
  }
}

right_trochees <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  if(is.na(temp_split)){
    next
  }
  if(length(grep("(V:1C?)|(V1C)$", temp_split[length(temp_split)])) == 1){
    right_trochees[i] <- 1
    next
  }
  else{
    if(length(grep("V$", temp_split[length(temp_split)])) == 1){
      if(length(grep("(L1)|(H1)", temp_split[length(temp_split) - 1])) == 1){
        right_trochees[i] <- 1
      }
    }
  }
}

DTS_L_extrametrical_above_2 <- default_vector
for(i in 1:length(overt_shapes)){
  temp_split <- unlist(strsplit(overt_shapes[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(is.na(temp_split)){
    next
  }
  if(length(temp_split) == 2){
    if(length(grep("H1", temp_split[1])) == 1){
      DTS_L_extrametrical_above_2[i] <- 1
    }
    if(length(grep("L$", temp_split[1])) == 1){
      if(length(grep("(V:1C?)|(V1C)", temp_split[2])) == 1){
        DTS_L_extrametrical_above_2[i] <- 1
      }
    }
    else if(length(grep("L1$", temp_split[1])) == 1){
      if(length(grep("V$", temp_split[2])) == 1){
        DTS_L_extrametrical_above_2[i] <- 1
      }
    }
  }
  else if(length(temp_split) > 2){
    if(length(temp_match) == 0){
      if(length(grep("^L1", temp_split[[1]])) == 1){
        DTS_L_extrametrical_above_2[i] <- 1
      }
    }
    else if(length(temp_match) == 1){
      if(length(grep("H1", temp_split)) == 1){
        DTS_L_extrametrical_above_2[i] <- 1
      }
    }
    else if(length(temp_match) >1){
      if(length(grep("H1", temp_split)) == 1){
        if(grep("H1", temp_split) == min(temp_match)){
          DTS_L_extrametrical_above_2[i] <- 1
        }
      }
    }
  }
}


stress_pattern_matches <- cbind(input_overt_shapes_frame, initial, peninitial, final, penult, antepenult, DTS_L, DTS_R, DTO_L, DTO_R, DTS_R_FSM, DTO_R_FSM, C_Lat_Rule, initial_iambs)
#stress_patterns_for_SSE <- cbind(stress_patterns_for_SSE, right_trochees)
stress_patterns_for_SSE <- cbind(stress_patterns_for_SSE, DTS_L_extrametrical_above_2)

write.table(stress_patterns_for_SSE, "Stress_Pattern_Matches_corrected.txt", sep="\t", quote=FALSE, row.names=FALSE)


write.table(right_trochees, "right_trochees.txt", sep="\t", quote=FALSE, row.names=FALSE)







new_overt_shapes <- c()
duplicates <- c()
overt_index <- c()
for(i in 1:length(overt_shapes)){
  if(overt_shapes[i] %in% new_overt_shapes){
    duplicates <- append(duplicates, overt_shapes[i])
    overt_index <- append(overt_index, i)
  }
  else{
    new_overt_shapes <- append(new_overt_shapes, overt_shapes[i])
  }
}

get_SSE <- function(vector_1, vector_2){
  return(sum((vector_1 - vector_2 )^2 ))
}

stress_patterns_for_SSE <- read.table("Stress_Pattern_Matches_corrected.txt", header=T, sep="\t")

### Making SSE tables for lexical stress
vedic_lexical_tableaux <- prepare_data("C_Skt_RIP_Lexical_Stress_corrected.txt")
learner_weights <- c(rep(5, ncol(vedic_lexical_tableaux[[2]][[1]])-1), 0) # set all weights of all (markedness) constraints to 0
learner_violations <- vedic_lexical_tableaux[[2]] # extract the violation profiles of all tableaux
learner_candidate_probs <- list()
learner_cand_probs <- initial_learner_candidate_probs(lexical_learner_violations, lexical_learner_weights) # set initial probabilities of candidates in initial state. In this simulation, every candidate is initially equally probable.

for_my_overts_lexical <- simple_learning_loop_lexical_1(tableaux = vedic_lexical_tableaux, number_iterations = 1000, learning_rate = 0.1, learning_rate_max_prom = 0.01, number_generations = 1, initial_weights = lexical_learner_weights)


overt_shapes_lexical <- for_my_overts_lexical[[3]]$Output_Overt
input_overt_shapes_frame_lexical <- as.data.frame(cbind(for_my_overts_lexical[[3]]$Input_Shape, for_my_overts_lexical[[3]]$Output_Overt))


# initial stress
# Quantity-insensitive
default_vector_lexical <- rep(0, 2238)
initial_lexical <-  default_vector_lexical
initial_lexical[grep("^[LH]1 ", overt_shapes_lexical)] <- 1

final_lexical <-  default_vector_lexical
final_lexical[grep(" V:?1C?$", overt_shapes_lexical)] <- 1

peninitial_lexical <- default_vector_lexical
peninitial_lexical[grep("^[LH] [LHV]:?1C?", overt_shapes_lexical)] <- 1

penult_lexical <- default_vector_lexical
penult_lexical[grep("[LH]1 V:?C?$", overt_shapes_lexical)] <- 1

antepenult_lexical <- default_vector_lexical
antepenult_lexical[grep("^[LH]1 V:?C?$", overt_shapes_lexical)] <- 1
antepenult_lexical[grep("[LH]1 [LH] V:?C?$", overt_shapes_lexical)] <- 1

# Unbounded quantity-sensitive
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H)|(V:)|(VC)", temp_split)
  if(length(grep("(H1)|(V:1)|(V1C)", temp_split)) > 1){
    print(overt_shapes_lexical[i])
  }
}


DTS_L_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes_lexical[i])) == 1){
      DTS_L_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      DTS_L_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == min(temp_match)){
        DTS_L_lexical[i] <- 1
      }
    }
  }
}

DTS_R_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("V1$", overt_shapes_lexical[i])) == 1){
      DTS_R_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      DTS_R_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == max(temp_match)){
        DTS_R_lexical[i] <- 1
      }
    }
  }
}



DTO_L_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("V1$", overt_shapes_lexical[i])) == 1){
      DTO_L_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      DTO_L_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == min(temp_match)){
        DTO_L_lexical[i] <- 1
      }
    }
  }
}

DTO_R_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)|(V:1?)|(V1?C)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes_lexical[i])) == 1){
      DTO_R_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      DTO_R_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)|(V:1)|(V1C)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)|(V:1)|(V1C)", temp_split) == max(temp_match)){
        DTO_R_lexical[i] <- 1
      }
    }
  }
}


DTS_R_FSM_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("L1 V:?C?", overt_shapes_lexical[i])) == 1){
      DTS_R_FSM_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)", overt_shapes_lexical[i])) == 1){
      DTS_R_FSM_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)", temp_split) == max(temp_match)){
        DTS_R_FSM_lexical[i] <- 1
      }
    }
  }
}



DTO_R_FSM_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(length(temp_match) == 0){
    if(length(grep("^L1", overt_shapes_lexical[i])) == 1){
      DTO_R_FSM_lexical[i] <- 1
    }
  }
  else if(length(temp_match) == 1){
    if(length(grep("(H1)", overt_shapes_lexical[i])) == 1){
      DTO_R_FSM_lexical[i] <- 1
    }
  }
  else if(length(temp_match) > 1){
    if(length(grep("(H1)", overt_shapes_lexical[i])) == 1){
      if(grep("(H1)", temp_split) == max(temp_match)){
        DTO_R_FSM_lexical[i] <- 1
      }
    }
  }
}



## Other quantity-sensitive systems
C_Skt_Rule_lexical <- default_vector_lexical # If length 2, match the exact shapes and plug in the 1; if length is above 2, plug in a 1 at a heavy penult or a 1 at a heavy antepenult
# If length == 3, plug in a 1 on the antepenult
# If length == 4, plug in a 1 on the preantepenult if the penult and antepenult are light
# If length == 5, plug in a 1 on the preantepenult if the penult and antepenult are light; else, put a 1 on a light penult
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  if(length(temp_split) == 2){
    if(length(grep("H1", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
      next
    }
    if(length(grep("^L1 VC?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
    if(length(grep("^L V:1C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    } 
  }
  if(length(temp_split) > 2){
    if(length(grep("H1 V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
    else if(length(grep("H1 L V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
  }
  if(length(temp_split) == 3){
    if(length(grep("^L1 L V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
  }
  if(length(temp_split) == 4){
    if(length(grep("^[HL]1 L L V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
  }
  if(length(temp_split) == 5){
    if(length(grep("H1 L L V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
    else if(length(grep("L1 L V:?C?$", overt_shapes_lexical[i])) == 1){
      C_Skt_Rule_lexical[i] <- 1
    }
  }
}

C_Lat_Rule_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  if(length(temp_split) == 2){
    if(length(grep("[LH]1", temp_split[1])) == 1){
      C_Lat_Rule_lexical[i] <- 1
    }
  }
  else{
    if(length(grep("H1", temp_split[length(temp_split) - 1])) == 1){
      C_Lat_Rule_lexical[i] <- 1
    }
    else{
      if(length(grep("L", temp_split[length(temp_split) - 1])) == 1){
        if(length(grep("H1", temp_split[length(temp_split) - 2])) == 1){
          C_Lat_Rule_lexical[i] <- 1
        }
        else{
          if(length(grep("L1", temp_split[length(temp_split) - 2])) == 1){
            C_Lat_Rule_lexical[i] <- 1
          }
        }
      }
    }
  }
}



initial_iambs_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  if(is.na(temp_split)){
    next
  }
  if(length(temp_split) == 2){
    if(temp_split[1] == "L"){
      initial_iambs_lexical[i] <- 1
    }
    else if(temp_split[1] == "H1"){
      initial_iambs_lexical[i] <- 1
    }
  }
  else{
    if(length(grep("H1", temp_split[1])) == 1){
      initial_iambs_lexical[i] <- 1
    }
    else if(temp_split[1] == "L"){
      if(length(grep("[LH]1", temp_split[2])) == 1){
        initial_iambs_lexical[i] <- 1
      }
    }
  }
}

right_trochees_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  if(is.na(temp_split)){
    next
  }
  if(length(grep("(V:1C?)|(V1C)$", temp_split[length(temp_split)])) == 1){
    right_trochees_lexical[i] <- 1
    next
  }
  else{
    if(length(grep("V$", temp_split[length(temp_split)])) == 1){
      if(length(grep("(L1)|(H1)", temp_split[length(temp_split) - 1])) == 1){
        right_trochees_lexical[i] <- 1
      }
    }
  }
}

DTS_L_extrametrical_above_2_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  temp_split <- unlist(strsplit(overt_shapes_lexical[i], split = " "))
  temp_match <- grep("(H1?)", temp_split)
  if(is.na(temp_split)){
    next
  }
  if(length(temp_split) == 2){
    if(length(grep("H1", temp_split[1])) == 1){
      DTS_L_extrametrical_above_2_lexical[i] <- 1
    }
    if(length(grep("L$", temp_split[1])) == 1){
      if(length(grep("(V:1C?)|(V1C)", temp_split[2])) == 1){
        DTS_L_extrametrical_above_2_lexical[i] <- 1
      }
    }
    else if(length(grep("L1$", temp_split[1])) == 1){
      if(length(grep("V$", temp_split[2])) == 1){
        DTS_L_extrametrical_above_2_lexical[i] <- 1
      }
    }
  }
  else if(length(temp_split) > 2){
    if(length(temp_match) == 0){
      if(length(grep("^L1", temp_split[[1]])) == 1){
        DTS_L_extrametrical_above_2_lexical[i] <- 1
      }
    }
    else if(length(temp_match) == 1){
      if(length(grep("H1", temp_split)) == 1){
        DTS_L_extrametrical_above_2_lexical[i] <- 1
      }
    }
    else if(length(temp_match) >1){
      if(length(grep("H1", temp_split)) == 1){
        if(grep("H1", temp_split) == min(temp_match)){
          DTS_L_extrametrical_above_2_lexical[i] <- 1
        }
      }
    }
  }
}

perfect_lexical_lexical <- default_vector_lexical
for(i in 1:length(overt_shapes_lexical)){
  UR_to_surface_temp <- gsub("/", "", input_overt_shapes_frame_lexical$V1[i])
  if(overt_shapes_lexical[i] == UR_to_surface_temp){
    perfect_lexical_lexical[i] <- 1
  }
}

stress_pattern_matches_for_SSE_lexical <- cbind(input_overt_shapes_frame_lexical, initial_lexical, peninitial_lexical, final_lexical, penult_lexical, antepenult_lexical, DTS_L_lexical, DTS_R_lexical, DTO_L_lexical, DTO_R_lexical, DTS_R_FSM_lexical, DTO_R_FSM_lexical, C_Lat_Rule_lexical, C_Skt_Rule_lexical, initial_iambs_lexical, right_trochees_lexical, DTS_L_extrametrical_above_2_lexical, perfect_lexical_lexical)

### Write out the table of stress patterns for SSE
write.table(stress_pattern_matches_for_SSE_lexical, "Stress_Pattern_Matches_corrected_lexical.txt", sep="\t", quote=FALSE, row.names=FALSE)

# Read them in
stress_patterns_for_SSE_lexical <- read.table("Stress_Pattern_Matches_corrected_lexical.txt", header=T, sep="\t")