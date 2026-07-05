setwd("/Users/henrikvonwehrden/Desktop/R/temp/")

#this script was partially created using Claude AI. The script was verified and complemented by the Henrik and the tutors

library(nlme)   # mixed-effect models
library(vegan)  # ordination (DCA)
library(MASS)   # stepAIC for model reduction

df <- read.csv("survey_data_stats.csv")


# =============================================================================
# 1. DATA INSPECTION
# =============================================================================

#Inspect the data, check for missing values, check the data formats, check that the data is clean

#Your code here

# =============================================================================
# 2. DATA EXPLORATION – GRAPHS
# =============================================================================

# Explore the data by running histograms, boxplots, barplots and scatterplots
# For the advanced students, create a scatter plot matrices
# 
# #Your code here

# =============================================================================
# 3. DATA ANALYSIS
# =============================================================================

# ── 3.1 T-TEST ──────────────────────────────────────────────────────────────
# Is there a difference in happiness between major groups?
#Your code here


# Is there a difference in happiness before and after completing the survey?
#Your code here


# ── 3.2 CHI-SQUARE ──────────────────────────────────────────────────────────
# Are the the different awareness status related to the morning drink? 
# Select only the coffee, tea and water beverages
#Your code here


# Are season preferences related to morning drink preferences?
#Your code here


# Are season preferences related to pineapple on pizza preferences?
#Your code here


# Are morning drink preferences related to pineapple on pizza preferences?
#Your code here


# ── 3.3 CORRELATION ─────────────────────────────────────────────────────────
# Which of the following variable pairs are significantly correlated?
# 
# happy1 vs sleep; happy1 vs heigth, sleep vs energy, happy1 vs bother
# recommended: run a sunflowerplot()
# Don't forget to check the pre-conditions for the different correlation tests


#Your code here



# For the advanced students run a full correlation matrix with p-values

#Your code here


# ---------3.4 ANALYSIS OF VARIANCE------------
# # Don't forget to check the pre-conditions
# 
# How much % of the variation in the average of hours of sleep per night is explained by the morning drink?
# Your code here

# Do season and breakfast have a significant effect on the happiness before completing the survey?
# Your code here



# Do the major, breakfast and having siblings have and effect on the happiness before completing the survey?
# Your code here




# ── 3.5 LINEAR (MULTIPLE) REGRESSION ────────────────────────────────────────
# Run a linear regression model to predict happy2 from numeric predictors only
# Don't forget to check the pre-conditions and evaluate ther residuals

# Your code here


# ── 3.6 MIXED-EFFECT MODELS ─────────────────────────────────────────────────
# Taking as reference the model you produced in aov3, now generate a Mixed Effect Model
# Take the response variable as happy2
# Keep the variables major, breakfast, siblings
# Add as fixed effects sleep, and os
# Keep tutorial_batch as the random intercept
# Use the function lme(), use method="ML"
# Run the summary and anova of the model()
# Find the minimum adequate model
# Random intercept: tutorial_batch as grouping factor

#Your code here




# ── 3.7 DATA FISHING ─────────────────────────────────────────────────────
#
# WARNING: This section is called "data fishing" (or p-hacking) on purpose.
#
# The idea: throw every variable you have into a model, then let an algorithm
# automatically remove the weakest ones until AIC stops improving. It sounds
# efficient, but it is statistically dangerous because:
#
#   1. With enough variables, some will appear significant purely by chance
#      (with 30 predictors you expect ~1-2 false positives at p < 0.05).
#   2. The final model was chosen AFTER looking at the data, so its p-values
#      and AIC are optimistic – they would not replicate on a new dataset.
#   3. There is no scientific story: the model does not reflect any prior
#      hypothesis about WHY variables belong together.
#
# This is why section 3.8 (candidate models) is the right approach in practice:
# you define competing hypotheses BEFORE running the models, then compare them.
# Data fishing is shown here so you can recognise it – not as a method to copy.
#
# HOW TO READ THE OUTPUT:
#   - Each step prints the AIC if that variable were removed ("<none>" = current).
#   - The algorithm drops the variable whose removal gives the lowest AIC.
#   - When no removal improves AIC, it stops.
#   - Lower AIC = better fit/complexity trade-off (not lower p-value!).

model_full <- lm(happy2 ~ happy1 + sleep + height + food_money +
                   apartment_size + sports + energy + travel +
                   siblings_number + pets + beer + coffee + rooms +
                   apps + countries + bother + pinky + hand + phone +
                   os + season + breakfast + siblings + direction +
                   aware + morning_drink_cat + pineapple_pizza + cb,
                 data = df, na.action = na.omit)

cat("Full model AIC:", AIC(model_full), "\n")

# step() removes/adds predictors and prints AIC at each step
model_reduced <- step(model_full, direction = "both", trace = 1)

cat("\nReduced model AIC:", AIC(model_reduced), "\n")
summary(model_reduced)

# ── 3.8 CANDIDATE MODELS ─────────────────────────────────────────────────────
# Start with clear hypotheses
# A lower AIC = better model. We compare the different models that test the different assumptions.
# This is not relevant for the exam, but this is how we would 
#install.packages(bbmle)
#install.packages(AICcmodavg)
library(bbmle)
library(AICcmodavg)
cand.models<-list()
cand.models[[1]] <- lm(happy2 ~  sleep + apartment_size + sports +
                     coffee + os  + breakfast + morning_drink_cat ,
                   data = df, na.action = na.omit)
cand.models[[2]] <- lm(happy2 ~  sleep +  sports + morning_drink_cat ,
                   data = df, na.action = na.omit)
cand.models[[3]] <- lm(happy2 ~  coffee  + breakfast + morning_drink_cat ,
                   data = df, na.action = na.omit)
cand.models[[4]] <- lm(happy2 ~  sleep + morning_drink_cat ,
                   data = df, na.action = na.omit)
cand.models[[5]] <- lm(happy2 ~  sleep + sports ,
                   data = df, na.action = na.omit)
cand.models[[6]] <- lm(happy2 ~  1 ,
                   data = df, na.action = na.omit)

#Vector of names
modnames<-paste("mod", 1:length(cand.models),sep="")

#AICc table
aic.table<-aictab(cand.set=cand.models, modnames=modnames, sort=TRUE)
print(aic.table,digits=4,LL=TRUE)


# =============================================================================
# 4 - ORDINATIONS
# =============================================================================


# ── 4.1 PCA ─────────────────────────────────────────────────────────────────
# Principal Component Analysis 
# 
# Subselect the dataframe considering only the numeric variables. 
# Which of the variables are correlated and redudant?
# Which of the variables are negative correlated?
# What are the two principal compoments showing?


#Your code here







# ── 4.2 (DETRENDED) CORRESPONDENCE ANALYSIS ──────────────────────────────────────────────
# Take heigth as factor
# Create a contingency table with that variable and morning_drink_cat
# Run both a correspondence analysis and detretended correspondence 
# Explain the different between the ordinations


#Your code here

