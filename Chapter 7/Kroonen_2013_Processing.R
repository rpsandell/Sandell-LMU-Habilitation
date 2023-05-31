# It might have been more worthwhile to type up the lemmata (yep!)

# Kroonen EDPG searches
kroonen_edpg <- scan("EDPG_new2.txt", sep="\n", fileEncoding = "utf-8", what = "raw", quote = NULL)

# Masculine ja-stems
kroonen_edpg_m_ja <- grep("\\*.*ja- m\\. '", kroonen_edpg, value =TRUE)
#ength(kroonen_edpg_m_ja
       
# Neuter ja-stems
kroonen_edpg_n_ja <- grep("\\*.*ja- n\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_n_ja)

# Adjective ja-stems
kroonen_edpg_adj_a <- grep("^\\*.*ja- adj\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_adj_ja)

# Masculine a-stems
kroonen_edpg_m_a <- grep("^\\*.*a- m\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_m_a)

# Neuter a-stems
kroonen_edpg_n_a <- grep("^\\*.*a- n\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_n_a)

# Adjective a-stems
kroonen_edpg_adj_a <- grep("^\\*.*[^j]a- adj\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_adj_a)

# Fem. jo-stems
kroonen_edpg_f_jo <- grep("^\\*.*jo- f\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_f_jo)

# Fem. o-stems
kroonen_edpg_f_o <- grep("^\\*.*[^j]o- f\\. '", kroonen_edpg, value =TRUE)
length(kroonen_edpg_f_o)


# All lemmata
kroonen_edpg_lemmas <- grep("^\\*", kroonen_edpg, value=TRUE)
  # Extract just the lemma
  #kroonen_lemmata_only <-  grep("^\\*.*- ", kroonen_edpg_lemmas, value = TRUE)
laryngeal_initial <- grep("^\\*h[z123]", kroonen_edpg_lemmas)
laryngeal_medial <- grep("^\\*[^\\ ]+h[z123]", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- kroonen_edpg_lemmas[-c(laryngeal_initial, laryngeal_medial)]
# replace other numbers
kroonen_edpg_lemmas <- gsub("6", "o", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("O", "o", kroonen_edpg_lemmas)

aspirate_initial <- grep("^\\*[^aeiou\\ ]h", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- kroonen_edpg_lemmas[-c(aspirate_initial)]

kroonen_space_in_lemma1 <- grep("^\\*[^aeiou]*[aeiou]\\ \\w*[-\\ ]", kroonen_edpg_lemmas, value=TRUE)
kroonen_space_in_lemma1 <- gsub("(^\\*[^aeiou]*[aeiou])\\ (\\w*[-\\ ])", "\\1C\\2", kroonen_space_in_lemma1)
kroonen_space_in_lemma2 <- grep("^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ][aeiou]\\ \\w*[-\\ ]", kroonen_edpg_lemmas, value=TRUE)
kroonen_space_in_lemma2 <- gsub("(^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ][aeiou])\\ (\\w*[-\\ ])", "\\1C\\2", kroonen_space_in_lemma2)

# Eliminate slashes in lemmata
kroonen_edpg_lemmas <- gsub("/", "C", kroonen_edpg_lemmas)

kroonen_edpg_lemmas <- gsub("(^\\*[^aeiou]*[aeiou]+)\\ (\\w*[-\\ ])", "\\1C\\2", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("(^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ]+)\\ (\\w*[-\\ ])", "\\1C\\2", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("(^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ][aeiou]+)\\ (\\w*[-\\ ])", "\\1C\\2", kroonen_edpg_lemmas)

saved_lemma_initial_space <- grep("^\\*[^aeiou]\\ ", kroonen_edpg_lemmas)
  # 55 lemmata removed because they were in state in which they could only be reliably repaired by hand
kroonen_edpg_lemmas <- kroonen_edpg_lemmas[-c(saved_lemma_initial_space)]

# Other manual repairs
kroonen_edpg_lemmas <- gsub("^\\*uz-e", "uzeta", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("^\\*wlda", "wIda", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("^\\*wfhan", "wihan", kroonen_edpg_lemmas)
kroonen_edpg_lemmas <- gsub("^\\*wrlhan", "wrihan", kroonen_edpg_lemmas)

kroonen_diph_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][iu]", kroonen_edpg_lemmas, value=TRUE)
kroonen_VCC_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][^aeiou][^aeiou]", kroonen_edpg_lemmas, value=TRUE)

# Counted by hand words with a long vowel in the 1st syll not already captured
  #ēbanþ- ~ ēbund-, ēla, ēlō-, ēmōn-, ēmōn-, ēsa-,
  #īda-, īdala-, īsa-, īsarna- ~ īzarna-, īwa-, 
 # ōfera-, ōsa(n)-, ōþala-
  #  ūra-, ūru-, ūt-, 
  #= 17
length(kroonen_diph_1st)+length(kroonen_VCC_1st)+17
# Some number with a long vowel in an open first syllable that are not counted
# Estimate 2800-2850 lemmata
# At least 1854 with a heavy first syllable, surely somewhat more, perhaps 1900-2000
  # Sum: probably ~ 65-70% of lemmata have a heavy first syllable
# How many of the shape /L H/?
kroonen_LH <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][^aeiou][aeiou]([iu]|[^aeiou-][^aeiou-])", kroonen_edpg_lemmas, value=TRUE)
    
# In Kroonen's lexicon, heavy first syllables are very common (65-70% of total)
  # Lemmata that inevitably 
kroonen_trisyllabic <- grep("^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ]+[aeiou]+[^aeiou/-\\ ]+[aeiou]+", kroonen_edpg_lemmas, value=TRUE)
kroonen_monosyllabic <- grep("^\\*[^aeiou]*[aeiou][iu]?[^aeiou/-\\ ]?-", kroonen_edpg_lemmas, value=TRUE)
kroonen_disyllabic <- grep("^\\*[^aeiou]*[aeiou]+[^aeiou/-\\ ]+[aeiou]+[^aeiou/-\\ ]*", kroonen_edpg_lemmas, value = TRUE)




weird_cluster <- grep("^\\*[^saeiou][^aeiou][^aeiou]", kroonen_edpg_lemmas, value = TRUE)


# Getting what's left over

kroonen_trisyllabic <- grep("^\\*[^aeiou]*[aeiou]+[^aeiou]+[aeiou]+[^aeiou]+[aeiou]+[^aeiou]*[-\\ ]", kroonen_edpg_lemmas)
kroonen_monosyllabic <- grep("^\\*[^aeiou]*[aeiou][iu]?[^aeiou]*[-\\ ]", kroonen_edpg_lemmas)
kroonen_disyllabic <- grep("^\\*[^aeiou]*[aeiou]+[^aeiou]+[aeiou]+[^aeiou]*[-\\ ]", kroonen_edpg_lemmas)

leftover <- kroonen_edpg_lemmas[-c(kroonen_trisyllabic, kroonen_disyllabic, kroonen_monosyllabic)]

# Distribution of syllable length of lemmata in Kroonen 2013
  # 1: 
  # 2:
  # 3:
  # 4:

# In disyllables
kroonen_diph_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][iu]", kroonen_disyllabic, value=TRUE)
kroonen_VCC_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][^aeiou][^aeiou]", kroonen_disyllabic, value=TRUE)
kroonen_o_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?o", kroonen_disyllabic, value=TRUE)

kroonen_diph_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][iu]", kroonen_trisyllabic, value=TRUE)
kroonen_VCC_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou][^aeiou][^aeiou]", kroonen_trisyllabic, value=TRUE)
kroonen_o_1st <- grep("^\\*s?[^aeiou]?[^aeiou]?o", kroonen_trisyllabic, value=TRUE)

kroonen_trisyllabic_LL <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiu][^aeiou][aeiu][^aeiou][aeiou]", kroonen_trisyllabic, value=TRUE)
kroonen_trisyllabic_LH <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiu][^aeiou][aeiuo]([iu]|[^aeiu]+)[^aeiou][aeiou]", kroonen_trisyllabic, value=TRUE)
kroonen_trisyllabic_HL <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou]([iu]|[^aeiou]+)[^aeiou][aeiu][^aeiou][aeiou]", kroonen_trisyllabic, value=TRUE)
kroonen_trisyllabic_HH <- grep("^\\*s?[^aeiou]?[^aeiou]?[aeiou]([iu]|[^aeiou]+)[^aeiou][aeiou]([iu]|[^aeiou]+)[^aeiou][aeiou]", kroonen_trisyllabic, value=TRUE)

# Gothic databases from Project Wulfila: not used here
#library(Hmisc)
#gotica <- mdb.get("Gotica.mdb")
  # Is a list of 23 dataframes, each corresponding to a table in the database

  

