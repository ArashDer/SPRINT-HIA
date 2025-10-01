#R Script for an example of meta-analysis used in WP5-HIA of SPRINT: 

# Install the metafor package if not already installed 

install.packages("metafor") 

# Load metafor 

library(metafor) 

# Sample data 

data <- data.frame( 

  study = c("Glover et al. (2023)", "Eskenazi et al. (2023)"), 

  logOR = c(0.799, 0.262),         # ln(OR) 

  SE = c(0.306, 0.385),             # standard error of ln(OR) 

  N = c(338, 72)                # sample size 

) 

# Run the meta-analysis using the log(OR) and SE 

res <- rma(yi = logOR, sei = SE, data = data, method = "REML") 

summary(res) 

# Forest plot with sample sizes as an additional column 

forest(res, 

       slab = data$study,                    # Study names 

       ilab = data$N,                        # Sample sizes 

       ilab.xpos = 2.5,                      # X position for N column 

       xlab = "Odds Ratio", 

       atransf = exp,                        # Transform log(OR) to OR 

       cex = 0.9) 

# Add a header for the sample size column 

text(2.5, length(data$logOR) + 2, "N", font = 2) 

# Compute OR and 95% CI manually for display 

OR <- exp(data$logOR) 

lower_CI <- exp(data$logOR - 1.96 * data$SE) 

upper_CI <- exp(data$logOR + 1.96 * data$SE) 

formatted_CI <- sprintf("%.2f [%.2f, %.2f]", OR, lower_CI, upper_CI) 

# Add OR and CI column to plot 

text(4.5, seq_along(data$study), formatted_CI, pos = 4, cex = 0.8) 

text(4.5, length(data$study) + 2, "OR [95% CI]", font = 2, pos = 4) 

 
