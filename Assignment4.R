## Set Up

options(scipen=10000000)

library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)
library(car)

palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

## Dependent Data

housing <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/Assignment4/housingSubsidy.csv")

housing %>%
  dplyr::select(y, age, previous, unemploy_rate, cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs) %>%
  gather(Variable, value, -y) %>%
  ggplot(aes(y, value, fill=y)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="y", y="Value", 
       title = "Feature associations with the likelihood of click",
       subtitle = "(continous outcomes)") +
  theme(legend.position = "none")

housing %>%
  dplyr::select(y, age, previous, unemploy_rate, cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs) %>%
  gather(Variable, value, -y) %>%
  ggplot() + 
  geom_density(aes(value, color=y), fill = "transparent") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(title = "Feature distributions take vs. no take",
       subtitle = "(continous outcomes)")

housing %>%
  dplyr::select(y, job, marital, education) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of taking the credit",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

housing %>%
  dplyr::select(y, taxLien, mortgage, taxbill_in_phl) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of taking the credit",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

housing %>%
  dplyr::select(y, contact, month, day_of_week, campaign, pdays, poutcome) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(., aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Click", y="Value",
       title = "Feature associations with the likelihood of taking the credit",
       subtitle = "Categorical features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

housing <- 
  housing %>%
  mutate(campaign.cat = case_when(
    campaign >= 0 & campaign < 2  ~ "Once",
    campaign >= 2 & campaign < 10  ~ "2 to 9 times",
    campaign > 9                    ~ "More than 9 times"))

housing <- 
  housing %>%
  mutate(education.cat = case_when(
    education == "basic.4y" | education == "basic.6y" | education == "basic.9y" ~ "Basic",
    education == "high.school" ~ "HighSchool",
    education == "professional.course" ~ "Professional",
    education == "university.degree" ~ "University",
    education == "illiterate" ~ "Illiterate",
    education == "unknown" ~ "Unknown"))

housing <-
  housing %>%
  mutate(education.bi = ifelse(education.cat == "Basic"|education.cat =="Illiterate", "Low", 
                                           "High"))

housing <- housing %>%
  mutate(job.cat = ifelse(job == "self-employed"|job =="unemloyed", "Self_or_unemployed", 
                               "Other"))

housing <- housing %>%
  mutate(season = case_when(
    month == "mar"| month == "apr"| month == "may" ~ "Spring",
    month == "jun" | month == "jul" | month == "aug"~ "Summer",
    month == "sep"| month == "oct" | month == "nov" ~ "Fall",
    month == "dec" ~ "Winter"))



## Split Data

set.seed(3456)
trainIndex <- createDataPartition(housing$y,
                                  y = paste(housing$taxLien,
                                            housing$education,
                                            housing$campaign),
                                  p = .65,
                                  list = FALSE,
                                  times = 1)
housingTrain <- housing[ trainIndex,]
housingTest  <- housing[-trainIndex,]

## Baseline Model

housingModel.base <- glm(y_numeric ~ .,
                        data=housingTrain %>% 
                          dplyr::select(-y, -campaign.cat),
                        family="binomial" (link="logit"))

summary(housingModel.base)
pR2(housingModel.base)

testProbs.base <- data.frame(Outcome = as.factor(housingTest$y_numeric),
                        Probs = predict(housingModel.base, housingTest, type= "response"))

testProbs.base <- 
  testProbs.base %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs.base$Probs > 0.5 , 1, 0)))

caret::confusionMatrix(testProbs.base$predOutcome, testProbs.base$Outcome, 
                       positive = "1")

## New Model

housingModel.new <- glm(y_numeric ~ .,
                         data=housingTrain %>% 
                           dplyr::select(y_numeric, job, marital, taxLien, mortgage,
                                         taxbill_in_phl, contact, pdays, poutcome,
                                         cons.price.idx, cons.conf.idx, inflation_rate,
                                         spent_on_repairs, campaign.cat, education,
                                         season),
                         family="binomial" (link="logit"))

summary(housingModel.new)
pR2(housingModel.new)

testProbs <- data.frame(Outcome = as.factor(housingTest$y_numeric),
                        Probs = predict(housingModel.new, housingTest, type= "response"))

testProbs <- 
  testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs$Probs > 0.5 , 1, 0)))

caret::confusionMatrix(testProbs$predOutcome, testProbs$Outcome, 
                       positive = "1")

## ROC Curve
auc(testProbs.base$Outcome, testProbs.base$Probs)

ggplot(testProbs.base, aes(d = as.numeric(testProbs.base$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - Baseline Model")


auc(testProbs$Outcome, testProbs$Probs)

ggplot(testProbs, aes(d = as.numeric(testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - New Model")

## Cross Validation
ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

cvFit <- train(y ~ .,
               data=housing %>% 
                 dplyr::select(y, job, marital, taxLien, mortgage,
                               taxbill_in_phl, contact, pdays, poutcome,
                               cons.price.idx, cons.conf.idx, inflation_rate,
                               spent_on_repairs, campaign.cat, education,
                               season), 
               method="glm", family="binomial",
               metric="ROC", trControl = ctrl)

cvFit

dplyr::select(cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines")


ggplot(testProbs, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Click", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")

