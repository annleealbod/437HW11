library(readr)
library(survival)
data <- read_csv("covidChina.txt")

# Convert dates to Date Format
data$datehosp <- as.Date(data$datehosp, "%Y-%m-%d")
data$datesympt <- as.Date(data$datesympt, "%Y-%m-%d")
data$dateconf <- as.Date(data$dateconf, "%Y-%m-%d")

# Calculate time from first symptoms to hospitalization
data$time_to_hosp <- as.numeric(data$datehosp - data$datesympt)

# Create censoring indicator (1 if hospitalized, 0 if not)
data$censoring_indicator <- ifelse(!is.na(data$time_to_hosp), 1, 0)

# Fit Kaplan-Meier survival curves for male and female groups
surv_male <- survfit(Surv(time_to_hosp, censoring_indicator) ~ male, data = data)

plot(surv_male, col = c("blue", "red"), lty = c(1, 2), lwd = 2, 
     main = "Kaplan-Meier Survival Curves by Gender",
     xlab = "Time from Symptoms Onset to Hospitalization",
     ylab = "Survival Probability",
     legend = c("Female", "Male"))


# Fit the Cox proportional hazards model
cox_model <- coxph(Surv(time_to_hosp, censoring_indicator) ~ age + male + fever + cough + sorethroat + wuhantravel, data = data)

# Assess proportional hazards assumption
cox_zph <- cox.zph(cox_model)
summary(cox_zph)
plot(cox_zph)