setwd("/Users/henrikvonwehrden/Desktop/R/temp/")

#this script was partially created using Claude AI. The script was verified and complemented by the Henrik and the tutors

library(nlme)   # mixed-effect models
library(vegan)  # ordination (DCA)
library(MASS)   # stepAIC for model reduction

df <- read.csv("survey_data_stats.csv")


# =============================================================================
# 1. DATA INSPECTION
# =============================================================================

dim(df)
str(df)
summary(df)
head(df)
tail(df)

# Missing values per column
colSums(is.na(df))

# Factor levels for key categorical variables. 
# This is a shortcut for converting categorical variables into factor


# =============================================================================
# 2. DATA EXPLORATION – GRAPHS
# =============================================================================

par(mfrow = c(2, 3))

hist(df$happy1,          main = "Happiness (before)", xlab = "happy1",         col = "steelblue")
hist(df$happy2,          main = "Happiness (after)",  xlab = "happy2",         col = "tomato")
hist(df$sleep,           main = "Hours of Sleep",     xlab = "sleep",          col = "seagreen")
hist(df$height,          main = "Height",             xlab = "height",         col = "goldenrod")
hist(df$food_money,      main = "Food Money",         xlab = "food_money",     col = "mediumpurple")
hist(df$apartment_size,  main = "Apartment Size",     xlab = "apartment_size", col = "coral")
which(df$apartment_size>1000)
df$apartment_size[384]


par(mfrow = c(1, 1))

# Boxplots: happiness by categorical variables
boxplot(happy1 ~ os,        data = df, main = "Happiness by OS",       col = c("steelblue", "tomato"))
boxplot(happy1 ~ season,    data = df, main = "Happiness by Season",   col = rainbow(4))
boxplot(happy1 ~ breakfast, data = df, main = "Happiness by Breakfast",col = c("goldenrod", "seagreen"))
boxplot(happy1 ~ siblings,  data = df, main = "Happiness by Siblings", col = c("orchid", "skyblue"))

# Scatterplot matrix
numeric_vars <- c("happy1", "happy2", "sleep", "height", "food_money",
                  "apartment_size", "sports", "energy", "travel")
pairs(df[, numeric_vars], main = "Scatterplot Matrix", pch = 16, col = "steelblue")

# Bar chart: morning drink frequency
barplot(table(df$morning_drink_cat), main = "Morning Drink",
        col = rainbow(length(levels(df$morning_drink_cat))), las = 2)

# Correlation heatmap
cor_matrix <- cor(df[, numeric_vars], use = "complete.obs")
image(1:ncol(cor_matrix), 1:nrow(cor_matrix), cor_matrix,
      axes = FALSE, main = "Correlation Matrix",
      col = colorRampPalette(c("tomato", "white", "steelblue"))(50))
axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2, cex.axis = 0.7)
axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix), las = 2, cex.axis = 0.7)


# =============================================================================
# 3. DATA ANALYSIS
# =============================================================================

# ── 3.1 T-TEST ──────────────────────────────────────────────────────────────
# Two-sample: happy1 by major
# Is there a difference in happiness between major groups?
t.test(happy1 ~ major, data = df)

# Was there a difference in happiness before and after completing the survey?
t.test(df$happy1, df$happy2, paired = TRUE)


# ── 3.2 CHI-SQUARE ──────────────────────────────────────────────────────────
# Are the the different awareness status related to the morning drink?
# select only the coffee, tea and water beverages
df2 <- subset(df, df$morning_drink_cat %in% c("coffee", "tea", "water"))
table(df2$aware, df2$morning_drink_cat)
chisq.test(table(df2$aware, df2$morning_drink_cat))


# Are season prefernces related to morning drink preferences?
table_season_drink <- table(df2$season, df2$morning_drink_cat)
chisq.test(table_season_drink)


# Are season preferences related to pineapple on pizza preferences?
table(df2$season, df2$pineapple_pizza)
chisq.test(df2$season, df2$pineapple_pizza)

# Are morning drink preferences related to pineapple on pizza preferences?
table(df2$morning_drink_cat, df2$pineapple_pizza)
chisq.test(df2$morning_drink_cat, df2$pineapple_pizza)


# ── 3.3 CORRELATION ─────────────────────────────────────────────────────────
# Which of the following variable pairs are significantly correlated?
# 
# happy1 vs sleep; happy1 vs heigth, sleep vs energy, happy1 vs bother
# recommended: run a sunflowerplot()
cor.test(df$happy1, df$sleep)
sunflowerplot(df$happy1, df$sleep)
abline(lm( df$sleep~df$happy1))
cor.test(df$happy1, df$height)
sunflowerplot(df$happy1, df$height)
abline(lm(df$height~df$sleep))
cor.test(df$sleep,  df$energy)
sunflowerplot(df$happy1, df$energy)
abline(lm(df$energy~df$sleep))

# Spearman (ordinal / non-normal)
cor.test(df$happy1, df$bother, method = "spearman")

# Full correlation matrix with p-values
cor_p <- function(data) {
  vars  <- colnames(data)
  n     <- ncol(data)
  r_mat <- p_mat <- matrix(NA, n, n, dimnames = list(vars, vars))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      test         <- cor.test(data[[i]], data[[j]], use = "complete.obs")
      r_mat[i, j]  <- test$estimate
      p_mat[i, j]  <- test$p.value
    }
  }
  list(r = r_mat, p = p_mat)
}
cor_results <- cor_p(df[, numeric_vars])
print(round(cor_results$r, 2))
print(round(cor_results$p, 3))


# ---------3.4 ANALYSIS OF VARIANCE------------
# 
# How much % of the variation in the average of hours of sleep per night is explained by the morning drink?
aov1 <- aov(sleep ~ morning_drink_cat, data = df2)
summary(aov1)
TukeyHSD(aov1)


# Do season and breakfast have a significant effect on the happiness before completing the survey?
aov2 <- aov(happy1 ~ season*breakfast, data = df)
summary(aov2)
TukeyHSD(aov2)

# Do the major, breakfast and having siblings have and effect on the happiness before completing the survey?
aov3 <- aov(happy1 ~ major* breakfast * siblings, data = df)
summary(aov3)

# ── 3.5 LINEAR (MULTIPLE) REGRESSION ────────────────────────────────────────
# Predict happy2 from numeric predictors only
model_lm <- lm(happy2 ~ happy1 + sleep + height + food_money +
                 apartment_size + sports + energy,
               data = df)
summary(model_lm)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_lm)
par(mfrow = c(1, 1))


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
model_mixed <- lme(happy2 ~ major + breakfast + siblings + sleep + os,
                    random = ~1 | tutorial_batch,
                    data = df, method="ML")
summary(model_mixed)

anova(model_mixed)

model_mixed2 <- lme(happy2 ~ happy1 + breakfast + siblings + sleep + os,
                   random = ~1 | tutorial_batch,
                   data = df, method="ML")
summary(model_mixed2)
anova(model_mixed2)

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

pca_vars <- c("happy1", "happy2", "sleep", "height", "food_money",
              "apartment_size", "sports", "energy", "travel",
              "siblings_number", "pets", "beer", "coffee", "rooms",
              "apps", "countries", "bother", "pinky", "hand", "phone")

pca_data <- df[, pca_vars]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
summary(pca_result)

# Loadings – first two PCs
print(round(pca_result$rotation[, 1:2], 3))

# Scree plot
plot(pca_result, type = "l", main = "Scree Plot")

# Biplot
biplot(pca_result, main = "PCA Biplot")

plot(
  pca_result$rotation[, 1:2],
  type = "n",
  xlab = "PC1",
  ylab = "PC2",
  asp = 1
)

arrows(
  0, 0,
  pca_result$rotation[,1],
  pca_result$rotation[,2],
  length = 0.1,
  col="blue"
)

text(
  pca_result$rotation[,1],
  pca_result$rotation[,2],
  labels = rownames(pca_result$rotation),
  pos = 4,
  col="green"
)


# ── 4.2 (DETRENDED) CORRESPONDENCE ANALYSIS ──────────────────────────────────────────────
# Take heigth as factor
# Create a contingency table with that variable and morning_drink_cat
# Run both a correspondence analysis and detretended correspondence 
# Explain the different between the ordinations

ca_matrix <- table(as.factor(df$height), df$morning_drink_cat)
print(ca_matrix)

# CA using vegan (already loaded)
ca_result <- cca(ca_matrix)
dca_result <- decorana(ca_matrix)


plot(ca_result)
plot(dca_result)

