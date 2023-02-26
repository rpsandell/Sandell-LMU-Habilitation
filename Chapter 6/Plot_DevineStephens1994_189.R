
# Plot of Figure 4.5 in Devine & Stephens 1994:189
Height <- c(3, 4, 5, 2, 1)
Tone <- c("M2", "M1", "H", "L1", "L2")
Syllable <- c(1, 2, 3, 4, 5)

library(ggplot2)

excursion <- data.frame(Height, Tone, Syllable)
excursion_plot <- ggplot(data=excursion, aes(x=Syllable, y=Height, label=Tone)) + geom_line() + geom_point() + 
  geom_text(hjust=0.75, vjust=1.7) + xlab("Time") + ylab("Pitch Excursion")
