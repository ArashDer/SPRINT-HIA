# R Script example 1 of the SPRINT WP5 HIA
#Calculation of population attributable fraction of metabolic syndrome
#the RR comes form the meta-analysis

# Inputs
baseline_cases <- 4234000   # baseline metabolic syndrome cases
exposure_level <- 0.17875       # average urinary glyphosate level (µg/L)
beta <- log(1.35)            # log RR per 1 µg/L increase (20% increase)

# Calculate RR
RR <- exp(beta * exposure_level)

# Calculate attributable fraction
AF <- (RR - 1) / RR

# Calculate attributable cases
attr_cases <- AF * baseline_cases

cat("Attributable fraction (AF):", round(AF, 4), "\n")
cat("Attributable cases of MetS:", round(attr_cases), "\n")

