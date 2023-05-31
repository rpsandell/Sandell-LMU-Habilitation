

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
