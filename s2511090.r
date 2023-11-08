library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(reshape2)
library(caret)
## Introductory Probability and Statistics
## Project - Biomarkers and Pain level
## Student number- s2511090

## Hypothesis testing question - Do the levels at inclusion vary between males and females?

# Pre-Processed Biomarker file
biomarkers_data <- read_excel("biomarkers.xlsx") 
# Pre-Processed Covariate file
covariates_data <- read_excel("covariates.xlsx")

# Extract covariates data for females
female_data <- covariates_data %>%
  filter(covariates_data[,3] == "2")

# Extract covariates data for males
male_data <- covariates_data %>%
  filter(covariates_data[,3] == "1")

# Combine biomarker data for female patients
female_inclusion<- biomarkers_data %>%
  filter(PatientID %in% female_data$PatientID, Timepoint == "0weeks")

# Combine biomarker data for male patients
male_inclusion<- biomarkers_data %>%
  filter(PatientID %in% male_data$PatientID, Timepoint == "0weeks")

### Hypothesis testing - Do the levels at inclusion vary between males and females?
  ## H0 (NULL Hypothesis): The levels of biomarker "X" at inclusion do not differ between females and males  
  ## H1 (Alternate Hypothesis): The levels of biomarker "X" at inclusion differ between females and males

### Hypothesis testing done by Welch Two Sample t-test for every biomarker
## Biomarker IL-6
IL6_0W <- t.test(male_inclusion$`IL-6`, female_inclusion$`IL-6`)
IL6_0W
## Biomarker VEGF-A
VEGFA_0W <- t.test(male_inclusion$`VEGF-A`, female_inclusion$`VEGF-A`)
VEGFA_0W
## Biomarker OPG
OPG_0W <- t.test(male_inclusion$OPG, female_inclusion$OPG)
OPG_0W
## Biomarker TGF-beta-1
TGFbeta1_0W <- t.test(male_inclusion$`TGF-beta-1`, female_inclusion$`TGF-beta-1`)
TGFbeta1_0W
## Biomarker IL-8
IL8_0W <- t.test(male_inclusion$`IL-8`, female_inclusion$`IL-8`)
IL8_0W
## Biomarker CXCL9
CXCL9_0W <- t.test(male_inclusion$CXCL9, female_inclusion$CXCL9)
CXCL9_0W
## Biomarker CXCL1
CXCL1_0W <- t.test(male_inclusion$CXCL1, female_inclusion$CXCL1)
CXCL1_0W
## Biomarker IL-18
IL18_0W <- t.test(male_inclusion$`IL-18`, female_inclusion$`IL-18`)
IL18_0W
## Biomarker CSF-1
CSF1_0W <- t.test(male_inclusion$`CSF-1`, female_inclusion$`CSF-1`)
CSF1_0W

# ### Bonferonni 
# p <- c(IL6_0W$p.value, VEGFA_0W$p.value, OPG_0W$p.value, TGFbeta1_0W$p.value, IL8_0W$p.value, CXCL9_0W$p.value, CXCL1_0W$p.value, IL18_0W$p.value, CSF1_0W$p.value)
# p.adjust(p, method = "bonferroni", n = length(p))

### Multiple Regression
set.seed(50)
#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(covariates_data), replace=TRUE, prob=c(0.8,0.2))
train  <- covariates_data[sample, ]
test   <- covariates_data[!sample, ]

VAS_12months <- train$"Vas-12months"
vector_ID <- train$PatientID
filtered_data <- biomarkers_data %>%
  filter(PatientID %in% vector_ID, Timepoint == "0weeks")

age <- train$"Age"
smoking_status <- train$"Smoker (1=yes, 2=no)"
sex <- train$"Sex (1=male, 2=female)"
VAS_inclusion <- train$"VAS-at-inclusion"

IL8 <- filtered_data$"IL-8"
VEGFA <- filtered_data$"VEGF-A"
OPG <- filtered_data$"OPG"
TGF <- filtered_data$"TGF-beta-1"
IL6 <- filtered_data$"IL-6"
CXCL9 <- filtered_data$"CXCL9"
CXCL1 <- filtered_data$"CXCL1"
IL18 <- filtered_data$"IL-18"
CSF1 <- filtered_data$"CSF-1"

# Fit the multiple regression model
model1 <- lm(VAS_12months ~ IL8 + VEGFA + OPG + TGF + IL6 + CXCL9 + CXCL1 + IL18 + CSF1 + age + sex + smoking_status + VAS_inclusion)

# Print the model summary
summary(model1)
# plot(model1)

new_train_comb <- data.frame(VAS_12months , IL8 , VEGFA , OPG , TGF , IL6 , CXCL9 , CXCL1 , IL18 , CSF1 , age , sex , smoking_status , VAS_inclusion)
new_train_comb = melt(new_train_comb, id.vars='VAS_12months')
ggplot(new_train_comb) +
  geom_jitter(aes(value,VAS_12months, colour=variable),) + geom_smooth(aes(value,VAS_12months, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(y = "Pain level at 12 months")

vector_ID_test <- test$PatientID
filtered_data_test <- biomarkers_data %>%
  filter(PatientID %in% vector_ID_test, Timepoint == "0weeks")

age <- test$"Age"
smoking_status <- test$"Smoker (1=yes, 2=no)"
sex <- test$"Sex (1=male, 2=female)"
VAS_inclusion <- test$"VAS-at-inclusion"

IL8 <- filtered_data_test$"IL-8"
VEGFA <- filtered_data_test$"VEGF-A"
OPG <- filtered_data_test$"OPG"
TGF <- filtered_data_test$"TGF-beta-1"
IL6 <- filtered_data_test$"IL-6"
CXCL9 <- filtered_data_test$"CXCL9"
CXCL1 <- filtered_data_test$"CXCL1"
IL18 <- filtered_data_test$"IL-18"
CSF1 <- filtered_data_test$"CSF-1"

new <- data.frame(IL8 , VEGFA , OPG , TGF , IL6 , CXCL9 , CXCL1 , IL18 , CSF1 , age , sex , smoking_status, VAS_inclusion)

# Make predictions for the test data
predictions <- predict(model1, newdata = new)

# Create a table of fitted parameter values
fitted_parameters <- data.frame(
  Parameter = names(coef(model1)),
  Coefficient = coef(model1)
)
print(fitted_parameters)

new_prediction_data <- data.frame(predictions , IL8 , VEGFA , OPG , TGF , IL6 , CXCL9 , CXCL1 , IL18 , CSF1 , age , sex , smoking_status , VAS_inclusion)
plot(model1)
