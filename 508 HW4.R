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
      spent_on_repairs >= 5200 ~ "repair5200+")
  )

#Interpret your new features
housing.eng%>%
  dplyr::select(y, age.cat, previous.cat, price.cat, conf.cat, inflation.cat, repair.cat) %>%
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
    y,y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, repair.cat,
    job, marital, education,taxLien,
    mortgage, taxbill_in_phl, contact, month,
    day_of_week, campaign, pdays, poutcome
  )

engTest<-housingTest%>%
  dplyr::select(
    y,y_numeric,age.cat, previous.cat, price.cat, conf.cat, inflation.cat, repair.cat,
    job, marital, education,taxLien,
    mortgage, taxbill_in_phl, contact, month,
    day_of_week, campaign, pdays, poutcome
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

#Goodness of fit(unsolved)
kitchen.testProbs <- data.frame(Outcome = as.factor(kitchenTest$y_numeric),
                        Probs = predict(kitchen.reg, kitchenTest, type= "response"))


eng.testProbs <- data.frame(Outcome = as.factor(engTest$y_numeric),
                                Probs = predict(eng.reg, engTest, type= "response"))

#ROC curve(unsolved)
ggplot(testProbs, aes(d = as.numeric(testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - kitchen sink")

#CV Goodness of Fit Metrics: kitchen sink
dplyr::select(kitchen.cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(kitchen.cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="Figure 7. CV Goodness of Fit Metrics: kitchen sink",
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
  labs(x="Goodness of Fit", y="Count", title="Figure 8. CV Goodness of Fit Metrics: engineering",
       subtitle = "Across-fold mean reprented as dotted lines")
