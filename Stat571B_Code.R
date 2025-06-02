#-------------------------------------------
# Load data & Convert factors as categorical
#-------------------------------------------
# Data Import (may need to change based on where you saved the csv)
study <- read.csv("study.csv")

# Setting factors
study$Person <- as.factor(study$Person)
study$Day <- as.factor(study$Day)
study$Time <- as.factor(study$Time)
study$Replicate <- as.factor(study$Replicate)
study$Location <- as.factor(study$Location)

head(study)

#-----------------------------------------------------
# Fitting into ANOVA full factorial model
#-----------------------------------------------------
library(lmerTest)

# Score as response
score_model <- lmer(Score ~ Day * Time * Location + (1 | Person), data = study)
anova(score_model)

#-----------------------------------------------------
# Fitting into a second order ANOVA model
#-----------------------------------------------------
library(lmerTest)

# Score as response
score_model_2 <- lmer(Score ~ (Day + Time + Location)^2 + (1 | Person), data = study)
anova(score_model_2)

#---------------------------------------------------
# Fitting into a second order reduced ANOVA model
#---------------------------------------------------
score_model_red <- lmer(Score ~ Time*Location + (1 | Person), data = study)
anova(score_model_red)

#---------------------------------------------------
# Interaction Plot and Formal Test with Tukey's test for additivity
#---------------------------------------------------
library(ggplot2)
library(emmeans)

# Estimated marginal means
emm <- emmeans(score_model_red, ~ Time * Location)
emm_df <- as.data.frame(emm)

# Plot
ggplot(emm_df, aes(x = Location, y = emmean, color = Time, group = Time)) +
  
# Raw data jittered (from full dataset)
geom_jitter(data = study, aes(x = Location, y = Score, color = Time),
            width = 0.2, alpha = 0.3, inherit.aes = FALSE) +
  
# Estimated means with error bars
geom_point(size = 3, position = position_dodge(0.2)) +
geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, position = position_dodge(0.2)) +
geom_line(position = position_dodge(0.2)) +
  
# Annotate the best location
annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
         alpha = 0.05, fill = "green") +
annotate("text", x = 1, y = max(emm_df$emmean) + 0.5,
         label = "Best: Common Room", color = "darkgreen", fontface = "bold") +
  
labs(title = "Fig A1: Time x Location Interaction Plot",
     y = "Estimated Score", x = "Location") +
theme_minimal()

#----------------------------------------------------
# Tukey's Test for NonAdditivity
#----------------------------------------------------
score_add <- aov(Score ~ Time + Location, data = study)
study$q3 <- fitted(score_add)^2
score_tukey_add <- aov(Score ~ Time + Location + q3, data = study)
anova(score_tukey_add)

#-----------------------------------------------
# Normal plot (Appendix Figure 2) and Residuals vs. Fitted (Appendix Figure 3)
#-----------------------------------------------

resid_values <- resid(score_model_red)
fitted_values <- fitted(score_model_red)

par(mfrow = c(1, 2))

# Q-Q Plot for Normality
qqnorm(resid_values, main = "Fig A2: Q-Q Plot of Residuals")
qqline(resid_values, col = "red", lwd = 2)

# Residuals vs. Fitted 
plot(fitted_values, resid_values,
     main = "Fig A3: Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)

#---------------------------------------------------
# Residual boxplots (Appendix Figures 4, 5, 6)
#--------------------------------------------------
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)
resid_values <- resid(score_model_red)

boxplot(resid_values ~ study$Time,
        main = "Fig A4: Residuals by Time",
        ylab = "Residuals",
        xlab = "Time")

boxplot(resid_values ~ study$Location,
        main = "Fig A5: Residuals by Location",
        ylab = "Residuals",
        xlab = "Location")

interaction_group <- interaction(study$Time, study$Location)

boxplot(resid_values ~ interaction_group,
        main = "Fig A6: Residuals by Time x Location",
        ylab = "Residuals",
        xlab = "Time x Location")

#--------------------------------------------------
# Tukey-adjusted confidence intervals
#--------------------------------------------------
library(emmeans)

emm <- emmeans(score_model_red, ~ Location)
summary(pairs(emm, adjust = "tukey"))


emm <- emmeans(score_model_red, ~ Time)
summary(pairs(emm, adjust = "tukey"))


emm <- emmeans(score_model_red, ~ Time:Location)
summary(pairs(emm, adjust = "tukey"))

