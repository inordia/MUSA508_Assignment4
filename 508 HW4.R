options(scipen=10000000)

library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)
palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

housing<-read.csv("C:/Users/zheng/Desktop/MUSA 508/Chapter6/housingSubsidy.csv")

#Develop and interpret data visualizations that describe feature importance/correlation.
housing%>%
  dplyr::select(y,age,previous,unemploy_rate,cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs)%>%
  gather(Variable, value, -y) %>%
  ggplot(aes(y, value, fill=y)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="y", y="Value", 
       title = "Figure 1. Feature associations with the likelihood of y",
       subtitle = "(continous outcomes)") +
  theme(legend.position = "none")

housing %>%
  dplyr::select(y, age, previous, unemploy_rate, cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs) %>%
  gather(Variable, value, -y) %>%
  ggplot() + 
  geom_density(aes(value, color=y), fill = "transparent") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(title = "Figure 2. Feature distributions click vs. no click",
       subtitle = "(continous outcomes)") +
  theme(legend.position = "none")

housing %>%
  dplyr::select(y, job, marital, education,taxLien) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Figure 3. Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

housing %>%
  dplyr::select(y, mortgage, taxbill_in_phl, contact, month) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Figure 4. Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

housing %>%
  dplyr::select(y, day_of_week, campaign, pdays, poutcome) %>%
  gather(Variable, value, -y) %>%
  count(Variable, value, y) %>%
  ggplot(aes(value, n, fill = y)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Figure 5. Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))


#Engineer new features
housing.eng<-housing%>%
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
      spent_on_repairs >= 5200 ~ "repair5200+"),
    season = case_when(
      month == "mar"| month == "apr"| month == "may" ~ "Spring",
      month == "jun" | month == "jul" | month == "aug"~ "Summer",
      month == "sep"| month == "oct" | month == "nov" ~ "Fall",
      month == "dec" ~ "Winter"),
    education.cat = case_when(
      education == "basic.4y" | education == "basic.6y" | education == "basic.9y" ~ "Basic",
      education == "high.school" ~ "HighSchool",
      education == "professional.course" ~ "Professional",
      education == "university.degree" ~ "University",
      education == "illiterate" ~ "Illiterate",
      education == "unknown" ~ "Unknown"),
    job.cat = ifelse(job == "self-employed"|job =="unemloyed", "Self_or_unemployed", 
                     "Other"),
    campaign.cat = case_when(
      campaign >= 0 & campaign < 2  ~ "Once",
      campaign >= 2 & campaign < 10  ~ "2 to 9 times",
      campaign > 9                    ~ "More than 9 times")
  )

#Interpret your new features
housing.eng%>%
  dplyr::select(y, age.cat, previous.cat, price.cat, conf.cat, inflation.cat, repair.cat,season) %>%
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

#Split your data into a 65/35 training/test set.
set.seed(3456)
trainIndex <- createDataPartition(housing.eng$y,
                                  y=paste(
                                    housing.eng$education,
                                    housing.eng$taxLien
                                          ),
                                  p = .65,
                                  list = FALSE,
                                  times = 1)
housingTrain <- housing.eng[ trainIndex,]
housingTest  <- housing.eng[-trainIndex,]

#Show a regression summary for both the kitchen sink and your engineered regression.
#split the dataset into train/test
#kitchen sink
kitchenTrain<-housingTrain%>%
  dplyr::select(y,y_numeric,age, previous, unemploy_rate, 
                cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs,
                job, marital, education,taxLien,
                mortgage, taxbill_in_phl, contact, month,
                day_of_week, campaign, pdays, poutcome)

kitchenTest<-housingTest%>%
  dplyr::select(y,y_numeric,age, previous, unemploy_rate, 
                cons.price.idx, cons.conf.idx, inflation_rate, spent_on_repairs,
                job, marital, education,taxLien,
                mortgage, taxbill_in_phl, contact, month,
                day_of_week, campaign, pdays, poutcome)

#engineering
engTrain<-housingTrain%>%
  dplyr::select(
    y,y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, 
    job.cat, marital, education.cat,taxLien,
    mortgage, taxbill_in_phl, contact, 
    day_of_week, campaign, pdays, season, poutcome,campaign.cat
  )

engTest<-housingTest%>%
  dplyr::select(
    y,y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, 
    job.cat, marital, education.cat,taxLien,
    mortgage, taxbill_in_phl, contact, 
    day_of_week, campaign, pdays, season, poutcome,campaign.cat
  )

#kitchen sink regression
kitchen.reg<-glm(y_numeric ~ .,
                 data=kitchenTrain%>% 
                   dplyr::select(-y),
                 family="binomial" (link="logit"))
summary(kitchen.reg)

#engineered regression
eng.reg<-glm(y_numeric ~ .,
             data=engTrain%>% 
               dplyr::select(-y),
             family="binomial" (link="logit"))
summary(eng.reg)

#Cross-validation
kitchen.ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

kitchen.cvFit <- train(y ~ ., data = kitchenTest %>% 
                 dplyr::select(
                   -y_numeric), 
               method="glm", family="binomial",
               metric="ROC", trControl = kitchen.ctrl)

kitchen.cvFit

eng.ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

eng.cvFit <- train(y ~ ., data = engTest %>% 
                         dplyr::select(
                           -y_numeric), 
                       method="glm", family="binomial",
                       metric="ROC", trControl = eng.ctrl)

eng.cvFit

#Goodness of fit
kitchen.testProbs <- data.frame(Outcome = as.factor(kitchenTest$y_numeric),
                        Probs = predict(kitchen.reg, kitchenTest, type= "response"))
kitchen.testProbs <- 
  kitchen.testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(kitchen.testProbs$Probs > 0.5 , 1, 0)))

caret::confusionMatrix(kitchen.testProbs$predOutcome, kitchen.testProbs$Outcome, 
                       positive = "1")

eng.testProbs <- data.frame(Outcome = as.factor(engTest$y_numeric),
                                Probs = predict(eng.reg, engTest, type= "response"))
eng.testProbs <- 
  eng.testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(eng.testProbs$Probs > 0.5 , 1, 0)))

caret::confusionMatrix(eng.testProbs$predOutcome, eng.testProbs$Outcome, 
                       positive = "1")

#ROC curve
ggplot(kitchen.testProbs, aes(d = as.numeric(kitchen.testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "Figure 7. ROC Curve - kitchen sink")

pROC::auc(kitchen.testProbs$Outcome, kitchen.testProbs$Probs)

ggplot(eng.testProbs, aes(d = as.numeric(eng.testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "Figure 8. ROC Curve - engineer")

pROC::auc(eng.testProbs$Outcome, eng.testProbs$Probs)

#CV Goodness of Fit Metrics: kitchen sink
dplyr::select(kitchen.cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(kitchen.cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="Figure 8. CV Goodness of Fit Metrics: kitchen sink",
       subtitle = "Across-fold mean reprented as dotted lines")

#CV Goodness of Fit Metrics: engineering
dplyr::select(eng.cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(eng.cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="Figure 9. CV Goodness of Fit Metrics: engineering",
       subtitle = "Across-fold mean reprented as dotted lines")

#cost benefit analysis
#cost_benefit_table
cost_benefit_table <-
  eng.testProbs %>%
  count(predOutcome, Outcome) %>%
  summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
            True_Positive = sum(n[predOutcome==1 & Outcome==1]),
            False_Negative = sum(n[predOutcome==0 & Outcome==1]),
            False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
  gather(Variable, Count) %>%
  mutate(Revenue =
           case_when(Variable == "True_Negative"  ~ 0,
                     Variable == "True_Positive"  ~ ((10000-2850-5000) * (Count * .25)) + 
                       (-2850 * (Count * .75)),
                     Variable == "False_Negative" ~ (10000-5000) * Count,
                     Variable == "False_Positive" ~ -2850 * Count)) %>%
  bind_cols(data.frame(Description = c(
    "We predicted no allocation and homeowner did not take credit",
    "We predicted allocation and homeowner took credit",
    "We predicted no allocation and homeowner took credit",
    "We predicted allocation and the homeowner did not take credit")))

kable(cost_benefit_table,
      caption = "Table 1. Cost/Benefit Table",format='pipe') %>% 
  kable_styling()

#Plot the confusion metric outcomes for each Threshold
iterateThresholds <- function(data) {
  x = .01
  all_prediction <- data.frame()
  while (x <= 1) {
    
    this_prediction <-
      eng.testProbs %>%
      mutate(predOutcome = ifelse(Probs > x, 1, 0)) %>%
      count(predOutcome, Outcome) %>%
      summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
                True_Positive = sum(n[predOutcome==1 & Outcome==1]),
                False_Negative = sum(n[predOutcome==0 & Outcome==1]),
                False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
      gather(Variable, Count) %>%
      mutate(Revenue =
               ifelse(Variable == "True_Negative", 0,
                      ifelse(Variable == "True_Positive",((10000-2850-5000) * (Count * .25)) + 
                               (-2850 * (Count * .75)),
                             ifelse(Variable == "False_Negative", (10000-5000) * Count,
                                    ifelse(Variable == "False_Positive", -2850 * Count, 0)))),
             Threshold = x)
    
    all_prediction <- rbind(all_prediction, this_prediction)
    x <- x + .01
  }
  return(all_prediction)
}

whichThreshold <- iterateThresholds(eng.testProbs)

whichThreshold %>%
  ggplot(.,aes(Threshold, Revenue, colour = Variable)) +
  geom_point() +
  scale_colour_manual(values = palette5[c(5, 1:3)]) +    
  labs(title = "Figure 9. Revenue by confusion matrix type and threshold",
       y = "Revenue") +
  guides(colour=guide_legend(title = "Confusion Matrix")) 

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
  filter(Threshold == 0.50|
  Threshold == 0.23) %>%
  mutate(Model = paste(Threshold*100,"% Threshold", sep=''),
         Total_Revenue = paste("$", Total_Revenue, spe='')) %>%
  dplyr::select(Model, Total_Revenue, Total_Count_of_Credits)

kable(Threshold.compare,
      caption = "Table 2. the Total_Revenue and Total_Count_of_Credits allocated for 2 Threshold",format='pipe') %>% 
  kable_styling()
