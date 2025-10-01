#R script to perform steps 3-6 of example 2 of the SPRINT HIA: 

# Define cut-off categories 

category <- cut(joined_sf_subset$FinalDose, 

                   breaks = c(0,0.001, 0.057, 0.62, 9.49, Inf), 

                   include.lowest = FALSE, 

                   right = TRUE, 

                   labels = c("unexposed"," (0–0.057]", "(0.057–0.62]", "(0.62–9.49]", ">9.49")) 

 

# Count and percentage 

summary_table <- as.data.frame(table(category)) 

summary_table$percent <- round(100 * summary_table$Freq / sum(summary_table$Freq), 1) 

# Rename columns for clarity 

names(summary_table) <- c("Range", "N", "Percent") 

# View result 

print(summary_table) 

 

# Sum of 'value' by 'group' 

aggregate(joined_sf_subset$aantal_geboorten ~ joined_sf_subset$gly_cat2, sum, data=joined_sf_subset) 

# Input data: odds ratios and population proportions for each exposure category 

odds_ratios <- c(1.12, 1.14, 1.24, 1.72)  # OR for <10th , 10-50th, 50th-9th and >90th percentiles of gly exposure from Rappazzo et al 2018  

proportions <- c(0.030949256, 0.172900262, 0.593285214, 0.198818898)  # % of NL population in each category according to levels reported by Rappazzo et al 2018  

# Function to calculate Attributable Fraction (AF) for each exposure category 

calculate_AF <- function(OR, P) { 

  AF <- (OR - 1) * P / (1 + (OR - 1) * P) 

  return(AF) 

} 

 

# Calculate AF for each category 

AFs <- mapply(calculate_AF, odds_ratios, proportions) 

AFs*100 

# Calculate the overall PAF 

PAF <- sum(AFs * proportions) 

# Print the result 

print(paste("Population Attributable Fraction (PAF):", round(PAF, 4))) 

 

#expected incidence of ASD according to VZinfo.nl 6 Per 10.000 levend geborenen  

48380* (6/10000) 

#PAF*expected incidence of ASD 

0.103 * 29 
