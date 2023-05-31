### Prosodic Shape Parser for RV

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


C <- "[bBcCdDfFgGhjJkKlmMnNpPrsStTvyYzZwWxX78]" # update the consonants
v <- "[aiuRQ]" # update the short vowels
V <- "[AIUeoEO3]" # update the long vowels

# There are forms with an independent svarita! These contain \\ in their entries
## don't use this
#tokens_rv <- gsub("y([aiuRQAIUeoEO])\\\\", "y\\1;", tokens_rv) # you converted the forms with independent svarita to stressed on the vowel with the svarita!



stressed_tokens_rv <- tokens_rv[grep(";", tokens_rv)] # all tokens with an udātta
single_stressed_tokens_rv <- stressed_tokens_rv[-c(grep(";.*;", stressed_tokens_rv))] # all forms with just a single udātta
multiple_stressed_tokens_rv <- tokens_rv[grep(";.*;", tokens_rv)] # all forms with multiple udāttas
no_stress_tokens_rv <- tokens_rv[-c(grep(";", tokens_rv))] # remove any forms without an udātta
no_stress_tokens_rv <- no_stress_tokens_rv[-c(grep("\\\\", no_stress_tokens_rv))] # 
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

all_prosodic_shapes_table <- sort(table(all_prosodic_shapes_stressed_rv), decreasing = T) # sort into a table by frequency
setwd("~/Documents/Dropbox/RyanTex/Sandell Habilitation/Prominence Frequencies/Vedic")
all_prosodic_shapes_frame <- as.data.frame(all_prosodic_shapes_table)
write.table(all_prosodic_shapes_frame, "RV_All_prosodic_shapes2.txt", quote = FALSE, sep="\t", row.names = FALSE)

# Matrix of stress positions by syllable and word length
matrix_token_udatta_positions <- matrix(0, nrow=8, ncol=8)
for(i in 1:length(all_prosodic_shapes_table)){
  temp_split <- unlist(strsplit(names(all_prosodic_shapes_table[i]), split=" "))
  temp_length <- length(temp_split)
  temp_stress_position <- grep("1", temp_split)
  matrix_token_udatta_positions[temp_length, temp_stress_position] <- (matrix_token_udatta_positions[temp_length, temp_stress_position] + all_prosodic_shapes_table[i])
}


# all prosodic shapes for tokens with no udātta
all_prosodic_shapes_no_stress <- c()
for(i in 1:length(no_stress_tokens_rv)){
  temp_result <- parse_prosodic_shape(no_stress_tokens_rv[i])
  all_prosodic_shapes_no_stress <- append(all_prosodic_shapes_no_stress, temp_result)
} 
# Version with sapply
all_prosodic_shapes_no_stress <- sapply(no_stress_tokens_rv, parse_prosodic_shape)
all_prosodic_shapes_no_stress_table <- (sort(table(all_prosodic_shapes_no_stress), decreasing = T ))

# Vector of frequencies by 

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
