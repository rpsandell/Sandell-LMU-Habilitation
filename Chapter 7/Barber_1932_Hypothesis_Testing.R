


# Hypothesis Testing: Barber's data

  # Consonant and s-stems excluded, since they have special features

# i-stem masculines: above average proportions of Endbetonung and schwankende Betonung
i_stem_masc <- matrix(c(13, 35, 7, 696-13, 678-35, 114-7), nrow=2, byrow=T)
chisq.test(i_stem_masc) 
# X-squared = 12.543, df = 2, p-value = 0.001889
# fisher: p-value = 0.000932

# i-stem feminines:above average proportions of Endbetonung and schwankende Betonung
i_stem_fem <- matrix(c(21, 38, 9, 696-21, 678-38, 114-9), nrow=2, byrow=T)
chisq.test(i_stem_fem) 
#X-squared = 8.402, df = 2, p-value = 0.01498
# fisher: p-value = 0.01231

# u-stem masculines: above average proportions of Endbetonung and schwankende Betonung
u_stem_masc <- matrix(c(15, 21, 5, 696-15, 678-21, 114-5), nrow=2, byrow=T)
chisq.test(u_stem_masc) 
# X-squared = 2.3629, df = 2, p-value = 0.3068
# Fisher: p-value = 0.2635

# u_stem_neut: little data, above avg. Anfangsbetonung
u_stem_neut <-matrix(c(3, 1, 0, 696-3, 678-1, 114-0), nrow=2, byrow=T)
#chisq.test(u_stem_neut) 
fisher.test(u_stem_neut)
# Fisher: p-value = 0.7272

# u-stem feminines: also little data, but mostly Endbetont
u_stem_fem <-matrix(c(1, 6, 0, 696-1, 678-6, 114-0), nrow=2, byrow=T)
#chisq.test(u_stem_neut) 
fisher.test(u_stem_fem)
# Fisher: p-value = 0.143

# u-stem adjectives: little data, evenly distributed
u_stem_adj <-matrix(c(4, 3, 1, 696-4, 678-3, 114-1), nrow=2, byrow=T)
#chisq.test(u_stem_neut) 
fisher.test(u_stem_adj)
#Fisher: p-value = 0.6243

# ā-stem (feminines):quite evenly distributed, almost in line with the averages
aa_stem_fem <-matrix(c(91, 95, 15, 696-91, 678-95, 114-15), nrow=2, byrow=T)
chisq.test(aa_stem_fem) 
# X-squared = 0.27109, df = 2, p-value = 0.8732
fisher.test(aa_stem_fem)
# p-value = 0.8856

# o-stem masculines: also typical of the average distribution
o_stem_masc <-matrix(c(131, 124, 25, 696-131, 678-124, 114-25), nrow=2, byrow=T)
chisq.test(o_stem_masc) 
# X-squared = 0.8468, df = 2, p-value = 0.65482
fisher.test(o_stem_masc)
# p-value = 0.6318

# o-stem neuters: again pretty typical distribution
o_stem_neut <-matrix(c(103, 93, 14, 696-103, 678-93, 114-14), nrow=2, byrow=T)
chisq.test(o_stem_neut) 
# X-squared = 0.67365, df = 2, p-value = 0.714
fisher.test(o_stem_neut)
# p-value = 0.7538

#o_stem_adj: somewhat higher schwankende Betonuung
o_stem_adj <-matrix(c(68, 52, 15, 696-68, 678-52, 114-15), nrow=2, byrow=T)
chisq.test(o_stem_adj) 
# X-squared = 4.3346, df = 2, p-value = 0.1145
fisher.test(o_stem_adj)
# p-value = 0.1102

# jaa-stem (fem): fairly average
jaa_stem_fem <-matrix(c(30, 38, 4, 696-30, 678-38, 114-4), nrow=2, byrow=T)
chisq.test(jaa_stem_fem) 
# X-squared = 4.3346, df = 2, p-value = 0.1145
fisher.test(jaa_stem_fem)
#p-value = 0.4757

#jo-stem masculine: 
jo_stem_masc <-matrix(c(13, 12, 0, 696-13, 678-12, 114-0), nrow=2, byrow=T)
chisq.test(jo_stem_masc) 
# X-squared = 2.1296, df = 2, p-value = 0.3448, too small for x-squared
fisher.test(jo_stem_masc)
# p-value = 0.4452

# jo-stem neuter
jo_stem_neut <-matrix(c(22, 21, 2, 696-22, 678-21, 114-2), nrow=2, byrow=T)
chisq.test(jo_stem_neut) 
# X-squared = 0.68351, df = 2, p-value = 0.7105
fisher.test(jo_stem_neut)
# p-value = 0.8213

# jo-stem adjective
jo_stem_adj <-matrix(c(26, 19, 1, 696-26, 678-19, 114-1), nrow=2, byrow=T)
chisq.test(jo_stem_adj) 
#X-squared = 3.0189, df = 2, p-value = 0.221, too little data
fisher.test(jo_stem_adj)
# p-value = 0.2358

# n-stem masculines
n_stem_masc <-matrix(c(78, 71, 13, 696-78, 678-71, 114-13), nrow=2, byrow=T)
chisq.test(n_stem_masc) 
#X-squared = 3.0189, df = 2, p-value = 0.221, too little data
fisher.test(n_stem_masc)
# p-value = 0.2358

# n-stem neuters: too little data
n_stem_neut <-matrix(c(0, 3, 1, 696-0, 678-3, 114-1), nrow=2, byrow=T)
chisq.test(n_stem_neut) 
#X-squared = 4.2125, df = 2, p-value = 0.1217
fisher.test(n_stem_neut)
# p-value = 0.0605
  # perhaps marginally significant

# fem. on-stem: above average Anfangsbetonung
on_stem_fem <- matrix(c(69, 46, 1, 696-69, 678-46, 114-1), nrow=2, byrow=T)
chisq.test(on_stem_fem) 
#X-squared = 12.9, df = 2, p-value = 0.001581
fisher.test(on_stem_fem)
# p-value = 0.0005073

# in-stem fem:
in_stem_fem <- matrix(c(8, 0, 1, 696-8, 678-0, 114-1), nrow=2, byrow=T)
chisq.test(in_stem_fem) 
#X-squared = 7.6999, df = 2, p-value = 0.02128
fisher.test(in_stem_fem)
# p-value = 0.01031

# Summary: consonant- and s-stems significantly and almost exceptionlessly show Anfangsbetonung
  # i-stems show significantly higher proportions of Endbetonung and schwankende Betonung
  # neuter n-stems are rare, but perhaps marginally point to an overrepresentation of Endbetonung and Anfangsbetonung
  # secondary feminine suffixes on- and in- have significantly more Anfangsbetonung
  # All other categories are averagein their behavior

# Correlations without consonant stems, s-stems, u-stems (other than masc.), n-stem neuters, and in-stems
prop.endbetont_noCons_no_low_freq <- c(0.636363636, 0.558823529, 0.512195122, 0.472636816, 0.442857143, 0.442857143, 0.385185185, 0.527777778, 0.48, 0.466666667, 0.413043478,
                           0.438271605, 0.396551724)
prop.schwankend_noCons_no_low_freq <- c(0.127272727, 0.132352941, 0.12195122, 0.074626866, 0.089285714, 0.066666667, 0.111111111, 0.055555556, 0, 0.044444444, 0.02173913, 0.080246914, 
                                        0.00862069)
# moderately positive correlation between proportion of Endbetonung and proportion of schwankende Betonung
  # r= 0.4994582

endbetonung_schwankend_lm <- lm(prop.schwankend_noCons_no_low_freq ~ prop.endbetont_noCons_no_low_freq + I(prop.endbetont_noCons_no_low_freq^2))
  # proportion of Endbetonung is a weakly significant predictor of Endbetonung


barber_labels <-c("i-stem_Masc", "i-stem_Fem", "u-Stem_Masc", "ā-Stem","o-Stem_Masc", "o-Stem_Neut", "o-Stem_Adj", "jā-Stem", "jo-Stem_Masc", "jo-Stem_Neut", "jo-Stem_Adj", "n-Stem_Masc", "ōn-Stem")
plot(prop.endbetont_noCons_no_low_freq, prop.schwankend_noCons_no_low_freq, type = "n", main = "Correlation of Endbetonung and schwankende Betonung (Barber 1932)", xlab = "Proportion endbetont", ylab = "Proportion with schwankender Betonung")
text(prop.endbetont_noCons_no_low_freq, prop.schwankend_noCons_no_low_freq, barber_labels)
abline(endbetonung_schwankend_lm)



