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

housing <- read.csv("/Users/inordia/Desktop/UPenn搞起来/592/Assignment4/MUSA508_Assignment4/housingSubsidy.csv")

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

## Feature Engineering

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

housing <-housing%>%
  mutate(
    age.cat=case_when(age < 30 ~ "age_30",
                      age >= 30 & age < 50 ~ "age30_50",
                      age >= 50 & age < 65 ~ "age50_65",
                      age >= 65 ~ "age65+"),
    previous.cat = case_when(
      previous == 0 ~ "previous0",
      previous > 0 ~ "previousNOT0"),
    conf.cat = case_when(
      cons.conf.idx < -50 ~ "conf_50-",
      cons.conf.idx >= -50 & cons.conf.idx < -45 ~ "conf-50_-45",
      cons.conf.idx >= -45 & cons.conf.idx < -40 ~ "conf-45_-40",
      cons.conf.idx >= -40 & cons.conf.idx < -35 ~ "conf-40_-35",
      cons.conf.idx >= -35 & cons.conf.idx < -30 ~ "conf-35_-30",
      cons.conf.idx >= -30 & cons.conf.idx < -25 ~ "conf-30_-25"),
    price.cat=case_when(
      cons.price.idx < 93 ~ "price93-",
      cons.price.idx >= 93 & cons.price.idx < 93.5 ~ "price93_93.5",
      cons.price.idx >= 93.5 & cons.price.idx < 94 ~ "price93.5_94",
      cons.price.idx >= 94 & cons.price.idx < 94.5 ~ "price94_94.5",
      cons.price.idx >= 94.5 ~ "price94.5+"),
    inflation.cat = case_when(
      inflation_rate >= 0.5 & inflation_rate < 1.5 ~ "inflation0.5_1.5",
      inflation_rate >= 1.5 & inflation_rate < 2.5 ~ "inflation1.5_2.5",
      inflation_rate >= 2.5 & inflation_rate < 3.5 ~ "inflation2.5_3.5",
      inflation_rate >= 3.5 & inflation_rate < 4.5 ~ "inflation3.5_4.5",
      inflation_rate >= 4.5 ~ "inflation4.5+"),
    repair.cat=case_when(
      spent_on_repairs < 5000 ~ "repair5000-",
      spent_on_repairs >= 5000 & spent_on_repairs < 5100 ~ "repair5000_5100",
      spent_on_repairs >= 5100 & spent_on_repairs < 5200 ~ "repair5100_5200",
      spent_on_repairs >= 5200 ~ "repair5200+")
  )


housing%>%
  dplyr::select(y, age.cat, previous.cat, price.cat, conf.cat, inflation.cat, repair.cat,season, education.cat, education.bi, 
                campaign.cat, job.cat) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Figure 6. New Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

## Split Data

set.seed(3456)
trainIndex <- createDataPartition(housing.eng$y,
                                  y=paste(
                                    housing.eng$education,
                                    housing.eng$taxLien
                                  ),
                                  p = .65,
                                  list = FALSE,
                                  times = 1)
housingTrain <- housing[ trainIndex,]
housingTest  <- housing[-trainIndex,]

## Baseline Model

housingModel.base <- glm(y_numeric ~ .,
                        data=housingTrain %>% 
                          dplyr::select(-y, -campaign.cat, -education.cat, -education.bi, -job.cat,
                                        -season, -age.cat, -previous.cat, -conf.cat,
                                        -price.cat, -inflation.cat, -repair.cat),
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
                           dplyr::select(y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, 
                                         job.cat, marital, education.cat,taxLien,
                                         mortgage, contact, 
                                         day_of_week, campaign.cat, pdays, season),
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


## Cross Validation
ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

cvFit.base <- train(y ~ .,
                    data=housing %>% 
                      dplyr::select(-y_numeric, -campaign.cat, -education.cat, -education.bi, -job.cat,
                                    -season, -age.cat, -previous.cat, -conf.cat,
                                    -price.cat, -inflation.cat, -repair.cat), 
                    method="glm", family="binomial",
                    metric="ROC", trControl = ctrl)

cvFit.base

cvFit <- train(y ~ .,
               data=housing %>% 
                 dplyr::select(y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, 
                               job.cat, marital, education.cat,taxLien,
                               mortgage, contact, 
                               day_of_week, campaign.cat, pdays, season), 
               method="glm", family="binomial",
               metric="ROC", trControl = ctrl)

cvFit

## Plots of the baseline CV

dplyr::select(cvFit.base$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines")


ggplot(testProbs.base, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) +
  labs(x = "Click", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  theme(strip.text.x = element_text(size = 18),
        legend.position = "none")



## Plots of the new model CV
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

## CBA

cost_benefit_table <-
  testProbs %>%
  count(predOutcome, Outcome) %>%
  summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
            True_Positive = sum(n[predOutcome==1 & Outcome==1]),
            False_Negative = sum(n[predOutcome==0 & Outcome==1]),
            False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
  gather(Variable, Count) %>%
  mutate(Revenue =
           ifelse(Variable == "True_Negative", Count * 0,
                  ifelse(Variable == "True_Positive",((5000+56000-2850) * (Count* 0.25)+ (-2850) * (Count* 0.75)),
                         ifelse(Variable == "False_Negative", Count*0,
                                ifelse(Variable == "False_Positive", (-2850) * Count, 0))))) %>%
  bind_cols(data.frame(Description = c(
    "We correctly predicted no credit taking",
    "We correctly predicted credit taking",
    "We predicted no credit taking and customer took the credit",
    "We predicted credit taking and customer did not take credit")))

kable(cost_benefit_table,
      caption = "Cost/Benefit Table") %>% kable_styling()

## Threshold
iterateThresholds <- function(data) {
  x = .01
  all_prediction <- data.frame()
  while (x <= 1) {
    
    this_prediction <-
      testProbs %>%
      mutate(predOutcome = ifelse(Probs > x, 1, 0)) %>%
      count(predOutcome, Outcome) %>%
      summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
                True_Positive = sum(n[predOutcome==1 & Outcome==1]),
                False_Negative = sum(n[predOutcome==0 & Outcome==1]),
                False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
      gather(Variable, Count) %>%
      mutate(Revenue =
               ifelse(Variable == "True_Negative", Count * 0,
                      ifelse(Variable == "True_Positive",((5000+56000-2850) * (Count* 0.25)+ (-2850) * (Count* 0.75)),
                             ifelse(Variable == "False_Negative", Count*0,
                                    ifelse(Variable == "False_Positive", (-2850) * Count, 0)))),
             Threshold = x)
    
    all_prediction <- rbind(all_prediction, this_prediction)
    x <- x + .01
  }
  return(all_prediction)
}

whichThreshold <- iterateThresholds(testProbs2)

whichThreshold_revenue <- 
  whichThreshold %>% 
  group_by(Threshold) %>% 
  summarize(Revenue = sum(Revenue))

ggplot(whichThreshold_revenue)+
  geom_line(aes(x = Threshold, y = Revenue))+
  geom_vline(xintercept =  pull(arrange(whichThreshold_revenue, -Revenue)[1,1]))+
  labs(title = "Model Revenues By Threshold For Test Sample",
       subtitle = "Vertical Line Denotes Optimal Threshold")

whichThreshold_revenue[1:20,]

#Create two small multiple plots that show Threshold as a function of Total_Revenue and Total_Count_of_Credits. Interpret this.

whichThreshold_revenue <- 
  whichThreshold %>% 
  mutate(allocations = ifelse(Variable == "True_Positive", (Count * 0.25),
                              ifelse(Variable == "False_Negative", Count, 0))) %>% 
  group_by(Threshold) %>% 
  summarize(Total_Revenue = sum(Revenue),
            Total_Count_of_Credits = sum(allocations))

#Total_Count_of_Credits
ggplot(whichThreshold_revenue)+
  geom_line(aes(x = Threshold, y = Total_Count_of_Credits))+
  geom_vline(xintercept =  pull(arrange(whichThreshold_revenue, -Total_Count_of_Credits)[1,1]))+
  labs(title = "Figure 10. Total Count of Credits By Threshold For Test Sample",
       subtitle = "Vertical Line Denotes Optimal Threshold")

#Total_Revenue
ggplot(whichThreshold_revenue)+
  geom_line(aes(x = Threshold, y = Total_Revenue))+
  geom_vline(xintercept =  pull(arrange(whichThreshold_revenue, -Total_Revenue)[1,1]))+
  labs(title = "Figure 11. Model Revenues By Threshold For Test Sample",
       subtitle = "Vertical Line Denotes Optimal Threshold")

#Create a table of the Total_Revenue and Total_Count_of_Credits allocated for 2 categories. 50%_Threshold and your Optimal_Threshold.(unsolved)

Threshold.compare<-
  whichThreshold_revenue %>%
  filter(Threshold == "0.49"|
           Threshold == "0.14") %>%
  mutate(Model = paste(Threshold*100,"% Threshold", sep=''),
         Total_Revenue = paste("$", Total_Revenue, spe='')) %>%
  dplyr::select(Model, Total_Revenue, Total_Count_of_Credits)


kable(Threshold.compare,
      caption = "Table 2. the Total_Revenue and Total_Count_of_Credits allocated for 2 Threshold",format='pipe') %>% 
  kable_styling()
