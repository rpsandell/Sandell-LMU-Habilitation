

# maybe it would be better to start from overt tokens with a primary stress marked
test_form <- ("L L L L V")

make_primary_stress <- function(prosodic_shape){
  current_input_split <- unlist(strsplit(prosodic_shape, split=" "))
  primary_stress_positions <- c()
  for(syllable in 1:length(current_input_split)){
    current_syllable <- current_input_split[syllable]
    stressed_syllable <- gsub("([LHV]:?)(C?/?)$", "\\11\\2", current_syllable) # add the 1 to mark that syllable as stressed
    current_input_stressed <- current_input_split
    current_input_stressed[syllable] <- stressed_syllable # insert the stressed syllable at the right position in the vector
    current_input_stressed <- paste(current_input_stressed, collapse = " ")
    primary_stress_positions <- append(primary_stress_positions, current_input_stressed)
  }
  return(primary_stress_positions)
}
my_primary_stresses <- make_primary_stress(test_form)

generate_all_parses <- function(primary_stresses){
  parses_list <- list()
  for(i in 1:length(primary_stresses)){
    parses_list[[i]] <- complete_footing_options(primary_stresses[i])
  }
  names(parses_list) <- primary_stresses
  return(parses_list)
}
my_parses_list <- generate_all_parses(my_primary_stresses)

# First generate the possible positions of primary stress

# The default operation in gsub is to parse from left to right: any matching sequences found on the left are replaced first, scanning the whole string
### Composing Prosodic Tableaux
  # First forms with just a single foot around the primary stress
primary_heavy <- function(overt_form){
  footed <- gsub("(H1)", "\\(\\1\\)", overt_form)
  footed <- gsub("(V:1)$", "\\(\\1\\)", footed)
  footed <- gsub("(V1C)$", "\\(\\1\\)", footed)
  footed <- gsub("(V:1\\C)$", "\\(\\1\\)", footed)
  return(footed)
}
primary_trochee <- function(overt_form){
  trochaics <- c()
  trochaics <- append(trochaics, gsub("([HL]1) ([LV]C?)", "\\(\\1 \\2\\)", overt_form)) ## L L or LV or H L or H V
  trochaics <- append(trochaics, gsub("(L1) (H|V:C?)", "\\(\\1 \\2\\)", overt_form)) # L H or LV:(C)
  #trochaics <- append(trochaics, gsub("(L1) (VC)$", "\\(\\1 \\2\\)\\3", overt_form))
  return(trochaics)
}
primary_iamb <- function(overt_form){
  iambics <- c()
  iambics <- append(iambics, gsub("(L) ([LV]1C?)", "\\(\\1 \\2\\)", overt_form)) #LL or LV(C)
  iambics <- append(iambics, gsub("(L) (H|V:)(1C?)", "\\(\\1 \\2\\3\\)", overt_form)) #L H or LV:
  #footed <- gsub("(L) ([HLV]:?1C?)", "\\(\\1 \\2\\)", overt_form) # LVC
  return(iambics)
}
primary_degenerates <- function(overt_form){
  footed <- gsub("([LV]1)", "\\(\\1\\)", overt_form)
  return(footed)
}

  # Then forms with maximal foot parses
  ## Foot form functions
    # Single heavy feet


# VERSIONS WITHOUT 2RY STRESS
  make_single_heavy <- function(overt_form){
    footed <- gsub("(H1?)", "\\(\\1\\)", overt_form)
    footed <- gsub("(V:1?)$", "\\(\\1\\)", footed)
    footed <- gsub("(V1?C)$", "\\(\\1\\)", footed)
    footed <- gsub("(V:1?C)$", "\\(\\1\\)", footed)
    return(footed)
  }
  # return at the end find all options with V:?C feet and create variants with final consonant extrametricality
    #Other trochees
  make_trochees <- function(overt_form){
    trochaic_feet <- c()
    footed <- gsub("([HL]1?) ([LV])(C?)", "\\(\\1 \\2\\)\\3", overt_form) # parse maximally
    trochaic_feet <- c(trochaic_feet, footed)
    footed <- make_single_heavy(footed) # wipe up any extra heavies
    trochaic_feet <- c(trochaic_feet, footed)
    # restart from the overt form
    footed <- make_single_heavy(overt_form) # parse heavies
    footed <- gsub("([HL]1?) ([LV])(C?)", "\\(\\1 \\2\\)\\3", footed) # parse maximally
    trochaic_feet <- c(trochaic_feet, footed)
    # clean up extra foot edges
    trochaic_feet <- gsub("\\(([^\\)]*)\\(", "\\(\\1", trochaic_feet)
    trochaic_feet <- gsub("\\)([^\\(]*)\\)", "\\1\\)", trochaic_feet)
    return(unique(trochaic_feet))
  }
  
    #Iambs
  make_iambs <- function(overt_form){
    iambic_feet <- c()
    footed <- gsub("(L) ([HLV]:?1?C?)", "\\(\\1 \\2\\)", overt_form) # parse maximally
    iambic_feet <- c(iambic_feet, footed)
    footed <- make_single_heavy(footed) # wipe up any extra heavies
    iambic_feet <- c(iambic_feet, footed)
    # restart from the overt form
    footed <- make_single_heavy(overt_form) # parse heavies
    footed <- gsub("(L) ([HLV]:?1?C?)", "\\(\\1 \\2\\)", footed) # parse maximally
    iambic_feet <- c(iambic_feet, footed)
    # clean up extra foot edges
    iambic_feet <- gsub("\\(([^\\)]*)\\(", "\\(\\1", iambic_feet)
    iambic_feet <- gsub("\\)([^\\(]*)\\)", "\\1\\)", iambic_feet)    
    return(unique(iambic_feet))
  }
  
  make_degenerates <- function(overt_form){
    footed <- gsub("([LV]1?)", " \\(\\1\\)", overt_form)
    # clean up extra foot edges
    footed <- gsub("\\(([^\\)]*)\\(", "\\(\\1", footed)
    footed <- gsub("\\)([^\\(]*)\\)", "\\1\\)", footed) 
    return(footed)
  }
# VERSIONS WITH 2RY STRESS
  make_single_heavy_2ry <- function(overt_form){
    footed <- gsub("H ", "\\(H2\\) ", overt_form)
    footed <- gsub("(V:)$", "\\(\\12\\)", footed)
    footed <- gsub("(V)(C)$", "\\(\\12\\2\\)", footed)
    footed <- gsub("(V:)(C)$", "\\(\\12\\2\\)", footed)
    return(footed)
  }
  # return at the end find all options with V:?C feet and create variants with final consonant extrametricality
  #Other trochees
  make_trochees_2ry <- function(overt_form){
    trochaic_feet <- c()
    footed <- gsub("([HL]) ([LV])([^12])", "\\(\\12 \\2\\)\\3", overt_form) # parse maximally for LL or HL
    trochaic_feet <- c(trochaic_feet, footed)
    footed <- gsub("(L) (H|V:)([^12])", "\\(\\12 \\2\\)\\3", overt_form) # parse maximally for LH
    footed <- make_single_heavy_2ry(footed) # wipe up any extra heavies
    trochaic_feet <- c(trochaic_feet, footed)
    # restart from the overt form
    footed <- make_single_heavy_2ry(overt_form) # parse heavies
    footed <- gsub("(L) (H|V:)([^12])", "\\(\\12 \\2\\)\\3", footed) # parse maximally for LH
    footed <- gsub("([HL]) ([LV])([^12])", "\\(\\12 \\2\\)\\3", footed) # parse maximally for LL or HL
    trochaic_feet <- c(trochaic_feet, footed)
    # clean up extra foot edges
    trochaic_feet <- gsub("\\(([^\\)]*)\\(", "\\(\\1", trochaic_feet)
    trochaic_feet <- gsub("\\)([^\\(]*)\\)", "\\1\\)", trochaic_feet)
    return(unique(trochaic_feet))
  }
  
  #Iambs
  make_iambs_2ry <- function(overt_form){
    iambic_feet <- c()
    footed1 <- gsub("(L) ([LV])(C|\\s|$)", "\\(\\1 \\22\\)\\3", overt_form) # parse maximally for LL
    iambic_feet <- c(iambic_feet, footed1)
    footed <- make_single_heavy_2ry(footed1) # wipe up any extra heavies
    iambic_feet <- c(iambic_feet, footed)
    footed <- gsub("(L) (H|V:)(C?)(\\s|$)", "\\(\\1 \\22\\3\\)\\4", overt_form) # parse maximally for LH
    iambic_feet <- c(iambic_feet, footed)
    footed <- make_single_heavy_2ry(footed) # wipe up any extra heavies
    iambic_feet <- c(iambic_feet, footed)
    footed <- gsub("(L) (V)(C)$", "\\(\\1 \\22\\3\\)", overt_form) # parse for L VC
    iambic_feet <- c(iambic_feet, footed)
    footed <- make_single_heavy_2ry(footed) # wipe up any extra heavies
    iambic_feet <- c(iambic_feet, footed)
    # restart from the overt form
    footed <- make_single_heavy_2ry(overt_form) # parse heavies
    footed <- gsub("(L) (V)(C)$", "\\(\\1 \\22\\3\\)", footed) # parse for L VC
    footed <- gsub("(L) (H|V:)(C?)(\\s|$)", "\\(\\1 \\22\\3\\)\\4", footed) # parse maximally for LH
    footed <- gsub("(L) ([LV])(C|\\s|$)", "\\(\\1 \\22\\)\\3", footed) # parse maximally for LL
    iambic_feet <- c(iambic_feet, footed)
    # clean up extra foot edges
    iambic_feet <- gsub("\\(([^\\)]*)\\(", "\\(\\1", iambic_feet)
    iambic_feet <- gsub("\\)([^\\(]*)\\)", "\\1\\)", iambic_feet)    
    return(unique(iambic_feet))
  }
  
  make_degenerates_2ry <- function(overt_form){
    footed_L <- gsub(" L ", " \\(L2\\) ", overt_form)
    footed_V <- gsub(" V(C?)$", " \\(V2\\)\\1", overt_form)
    footed_L_V <- gsub(" V(C?)$", " \\(V2\\)\\1", footed_L)
    footed <- c(footed_L, footed_V, footed_L_V)
    # clean up extra foot edges
    footed <- gsub("\\(([^\\)]*)\\(", "\\(\\1", footed)
    footed <- gsub("\\)([^\\(]*)\\)", "\\1\\)", footed) 
    return(footed)
  }
  
  # Finally, generate variants with final consonant extrametricality
  make_extrametrical_variants_2ry <- function(footed_form){
    extrametrical_variant <- gsub("(V:?[12]?)C\\)$", "\\1\\)C", footed_form)
    return(extrametrical_variant)
  }
  # and variants that avoid stress clash -- still trying to fix this!
  remove_clash_right_2ry <- function(footed_form){
    clashless_variant <- gsub("([HL]1\\)) \\(([LHV]:?2 ?[LHV]?:?C?)\\)", "\\1 \\2", footed_form) # note that in the case of (L X) to the right, it only makes clash it that foot is trochaic
    clashless_variant <- gsub("([HL]1\\)) ([LHV]:?)2", "\\1 \\2", clashless_variant)
    #clashless_variant <- gsub("\\(([LHV]:?C? [LHV]:?C?)\\) (\\([HLV]:?1)", "\\1 \\2", clashless_variant)
    #clashless_variant <- gsub("([HLV]1\\)) \\((.+)\\)", "\\1 \\2", footed_form)
    #clashless_variant <- gsub("\\((.+)\\) (\\([HLV]:?1)", "\\1 \\2", clashless_variant)
    return(clashless_variant)
  }
  remove_clash_left_2ry <- function(footed_form){
    #clashless_variant <- gsub("([HLV])2\\) \\(([LHV]:?1C? [LHV]:?C?)\\)", "\\1 \\2", footed_form)
    clashless_variant <- gsub("\\(([LH]? ?[LH])2\\) (\\([HLV]:?1)", "\\1 \\2", footed_form) # note that in the case of (L X) to the left, it only makes clash if that foot is iambic
    clashless_variant <- gsub("2 \\(", " \\(", clashless_variant)
    #clashless_variant <- gsub("([HLV]1\\)) \\((.+)\\)", "\\1 \\2", footed_form)
    #clashless_variant <- gsub("\\((.+)\\) (\\([HLV]:?1)", "\\1 \\2", clashless_variant)
    return(clashless_variant)
  }
  
# I assume that degenerate feet of the form (L) are always less harmonic than some alternative, so additional degenerate feet need only be created at the end
complete_footing_options <- function(overt_form){
  footing_options <- c()
  # Include all options with just a single foot
  footing_options <- c(footing_options, primary_heavy(overt_form))
  footing_options <- c(footing_options, primary_trochee(overt_form))
  footing_options <- c(footing_options, primary_iamb(overt_form))
  footing_options <- c(footing_options, primary_degenerates(overt_form))
  footing_options <- unique(footing_options)
  footing_options <- grep("\\(", footing_options, value=TRUE)
  
  # Include all options with multiple feet, doing just a single L-R parse of each sort
    # All heavy syllables
  footing_options_heavy <- c(footing_options, make_single_heavy_2ry(footing_options))
    # All trochaic feet
  footing_options_trochee <- c(footing_options, make_trochees_2ry(footing_options))
    # All iambic feet
  footing_options_iamb <- c(footing_options, make_iambs_2ry(footing_options))
    # With degenerate feet, too
  footing_options_degenerate <- c(footing_options, make_degenerates_2ry(footing_options))
  footing_options <- unique(c(footing_options, footing_options_heavy, footing_options_trochee, footing_options_iamb, footing_options_degenerate))
  
  # Include variants with final consonant extrametricality or without CLASH around the primary stress
  extrametrical_variants_with_clash <- make_extrametrical_variants_2ry(footing_options)
  
  no_clash_left_no_extrametrical <- remove_clash_left_2ry(footing_options)
  no_clash_right_no_extrametrical <- remove_clash_right_2ry(footing_options)
  no_clash_all_no_extrametrical <- remove_clash_right_2ry(no_clash_left_no_extrametrical)
  
  no_clash_left_yes_extrametrical <- remove_clash_left_2ry(extrametrical_variants_with_clash)
  no_clash_right_yes_extrametrical <- remove_clash_right_2ry(extrametrical_variants_with_clash)
  no_clash_all_yes_extrametrical <- remove_clash_right_2ry(no_clash_left_yes_extrametrical)
  
  footing_options <- append(footing_options, c(extrametrical_variants_with_clash, no_clash_left_no_extrametrical, no_clash_right_no_extrametrical, no_clash_all_no_extrametrical, no_clash_left_yes_extrametrical, no_clash_right_yes_extrametrical, no_clash_all_yes_extrametrical))
  
  #footing_options <- append(footing_options, no_clash_no_extrametrical)
  #footing_options <- append(footing_options, no_clash_yes_extrametrical)
  
  #clean up spaces between characters and foot edges, multiple white spaces, white spaces at edges
  footing_options <- gsub("\\( ", "\\(", footing_options)
  footing_options <- gsub(" \\)", "\\(", footing_options)
  footing_options <- gsub("^ ", "", footing_options)
  footing_options <- gsub(" $", "", footing_options)
  footing_options <- gsub("  ", " ", footing_options)
  
  footing_options <- unique(footing_options)
  
  #remove any options that do not contain any feet. more precisely, save only the options that contain a left foot edge
  final_footing_options <- grep("\\(", footing_options, value=TRUE)
  
  return(final_footing_options)
}



  
  
## Two versions: input is a string without primary stress, where the primary stresses must be marked, or input is 
#Input is 