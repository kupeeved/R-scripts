
##Загрузка пакетов-----------------------
#install.packages('writexl')
#install.packages('tidyr')
#install.packages('stringr')
#install.packages('randomizr')
#install.packages('rstatix')
#install.packages('digest')
#install.packages('data.table')
#install.packages('tidyverse')
#install.packages('mltools')
#install.packages('modelsummary')
#install.packages('haven')

library(haven)
library(dplyr)
library(modelsummary)
library(tidyverse)
library(mltools)
library(data.table)
library (writexl)
library(randomizr) 
library(digest)
library(rstatix)
library (stringr)
library(tidyr)


###Таблица 1--------------------------------------------------------------------------
data <- read_dta('GEF_dataset_final.dta')

data <- transform( 
  data, treatment = as.character(treatment))

data['treatment'][data['treatment'] == 0] <- 'Control'
data['treatment'][data['treatment'] == 1] <- 'Alt-Fact'
data['treatment'][data['treatment'] == 2] <- 'Facts'
data['treatment'][data['treatment'] == 3] <- 'Fact-Check'

data <- replace(data,is.na(data),0)


data$Single <- as.integer(data$marital_status == 1)
data$Married <- as.integer(data$marital_status == 2)

data$Student <- as.integer(data$labor_stat == 5)
data$Unemployed <- as.integer(data$labor_stat == 6)
data$Retired <- as.integer(data$labor_stat == 7)
data <- transform( 
  data, labor_stat = as.character(labor_stat))
data['labor_stat'][data['labor_stat'] == 1] <- 'C'
data['labor_stat'][data['labor_stat'] == 2] <- 'C'
data$Full_or_part_time_worker <- as.integer(data$labor_stat == 'C')


data$Source_of_income_wages <- as.integer(data$source_inc == 1)
data$Source_of_income_social_benefits<- as.integer(data$source_inc == 2)
data$Source_of_income_pension <- as.integer(data$source_inc == 3)

data$Source_of_news_TV <- as.integer(data$source_media == 1)
data$Source_of_news_radio<- as.integer(data$source_media == 2)
data$Source_of_news_internet <- as.integer(data$source_media == 3)

data$Religion_Catholic <- as.integer(data$religion == 1)
data$Religion_Muslim <- as.integer(data$religion == 4)
data$Religion_none <- as.integer(data$religion == 7)

data$Voted_in_2012_Hollande <- as.integer(data$whomvoted_past == 1)
data$Voted_in_2012_Sarkozy <- as.integer(data$whomvoted_past == 2)
data$Voted_in_2012_Melenchon <- as.integer(data$whomvoted_past == 3)
data$Voted_in_2012_Le_Pen <- as.integer(data$whomvoted_past == 4)
data <- transform( 
  data, whomvoted_past = as.character(whomvoted_past))
data['whomvoted_past'][data['whomvoted_past'] == 5] <- 'C'
data['whomvoted_past'][data['whomvoted_past'] == 7] <- 'C'

data$Voted_in_2012_Other_candidate <- as.integer(data$whomvoted_past == 'C')

new_data <- data %>% select(children, num_children,Married,Single,income,
                            Student,Unemployed,Full_or_part_time_worker,Retired,Source_of_income_wages,Source_of_income_social_benefits,
                            Source_of_income_pension,Source_of_news_TV,Source_of_news_radio,Source_of_news_internet,
                            Religion_Catholic,Religion_Muslim,Religion_none,Voted_in_2012_Hollande,
                            Voted_in_2012_Sarkozy,Voted_in_2012_Melenchon,Voted_in_2012_Le_Pen,
                            Voted_in_2012_Other_candidate, treatment)

df1 <- dcast(melt(new_data), variable ~ treatment, mean)


formula <- lapply(names(new_data)[-24], function(var) formula(paste0(var, '~treatment')))
tests <- data.frame() 
d1 <- for(i in 1:length(formula)){   
  test <- pairwise_t_test(formula[[i]], data = new_data)  %>% select(1, 2, 3, 6) 
  tests <- rbind(tests, test)   
  print(test) }

tests <- unite(tests, col='group', c('group1', 'group2'), sep='-')

D <- tests %>% pivot_wider(names_from = group, values_from = p)

names(D)[names(D) == '.y.'] <- 'variable'

D1 <-merge(df1, D, by="variable") #Таблица 1

#write_xlsx(D1, 'table1.xlsx')

#11 СТОЛБЕЦ (Проверка Множественных Гипотез)

Rtests <- data.frame() 
Rd1 <- for(i in 1:length(formula)){   
  Rtest <- pairwise_t_test(formula[[i]], p.adjust.method = "holm", data = new_data)  %>% select(1, 2, 3, 6, 9) 
  Rtests <- rbind(Rtests, Rtest)   
  print(Rtest) }

Rtests <- unite(Rtests, col='group', c('group1', 'group2'), sep='-')
R <- Rtests %>% pivot_wider(names_from = group, values_from = p)

names(R)[names(R) == '.y.'] <- 'variable'

R <- replace(R,is.na(R),0)

R <- transform( 
  R, p.adj.signif = as.character(p.adj.signif))

R['p.adj.signif'][R['p.adj.signif'] == 'ns'] <- 0
R['p.adj.signif'][R['p.adj.signif'] == '*'] <- 1
R['p.adj.signif'][R['p.adj.signif'] == '**'] <- 2


R$p.adj.signif <- as.numeric(R$p.adj.signif)
R$variable <- as.numeric(R$variable)
R[9, ] <- R[9, ] + R[10, ]
R[11, ] <- R[11, ] + R[12, ]
R[14, ] <- R[14, ] + R[15, ]
R <- R[-c(10,12,15), ]
R[1, 1] <- 'children'
R[2, 1] <- 'num_children'
R[3, 1] <- 'Married'
R[4, 1] <- 'Single'
R[5, 1] <- 'income'
R[6, 1] <- 'Student'
R[7, 1] <- 'Unemployed'
R[8, 1] <- 'Full_or_part_time_worker'
R[9, 1] <- 'Retired'
R[10, 1] <- 'Source_of_income_wages'
R[11, 1] <- 'Source_of_income_social_benefits'
R[12, 1] <- 'Source_of_income_pension'
R[13, 1] <- 'Source_of_news_TV'
R[14, 1] <- 'Source_of_news_radio'
R[15, 1] <- 'Source_of_news_internet'
R[16, 1] <- 'Religion_Catholic'
R[17, 1] <- 'Religion_Muslim'
R[18, 1] <- 'Religion_none'
R[19, 1] <- 'Voted_in_2012_Hollande'
R[20, 1] <- 'Voted_in_2012_Sarkozy'
R[21, 1] <- 'Voted_in_2012_Melenchon'
R[22, 1] <- 'Voted_in_2012_Le_Pen'
R[23, 1] <- 'Voted_in_2012_Other_candidate'

R['p.adj.signif'][R['p.adj.signif'] == 0] <- 'NO'
R['p.adj.signif'][R['p.adj.signif'] == 1] <- 'YES'
R['p.adj.signif'][R['p.adj.signif'] == 2] <- 'YES'

R <-merge(df1, R, by="variable")
#write_xlsx(R, 'TABLE3.xlsx')

###2.2 Рандомизация------------------------------------------------------------------

# Задание 1------
set.seed(123)
N <- nrow(data)
T1 <- complete_ra(N=N, conditions = c("Control", "Alt-Fact", "Fact-Check", "Facts"))
data$T1 <- T1 # добавляем переменную в датасет
head(data, 10)
table(data$T1) # проверим, что рандомизация равномерная по 25%

#Проверим качество рандомизации 
new_data2 <- data %>% select(children, num_children, Married,Single,income,
                            Student,Unemployed,Full_or_part_time_worker,Retired,Source_of_income_wages,Source_of_income_social_benefits,
                            Source_of_income_pension,Source_of_news_TV,Source_of_news_radio,Source_of_news_internet,
                            Religion_Catholic,Religion_Muslim,Religion_none,Voted_in_2012_Hollande,
                            Voted_in_2012_Sarkozy,Voted_in_2012_Melenchon,Voted_in_2012_Le_Pen,
                            Voted_in_2012_Other_candidate, T1)
new_data2<-as.data.table(new_data2)
df2 <- dcast( melt(new_data2), variable ~ T1, mean)

formula2 <- lapply(names(new_data2)[-24], function(var) formula(paste0(var, '~T1')))
Rtests2 <- data.frame() 
Rd2 <- for(i in 1:length(formula)){   
  Rtest2 <- pairwise_t_test(formula2[[i]], p.adjust.method = "holm", data = new_data2)  %>% select(1, 2, 3, 6, 9) 
  Rtests2 <- rbind(Rtests2, Rtest2)   
  print(Rtest2)}

Rtests2 <- unite(Rtests2, col='group', c('group1', 'group2'), sep='-')
RD2 <- Rtests2 %>% pivot_wider(names_from = group, values_from = p)

names(RD2)[names(RD2) == '.y.'] <- 'variable'

R2 <-merge(df2, RD2, by="variable")

R2 <- transform( 
  R2, p.adj.signif = as.character(p.adj.signif))

R2['p.adj.signif'][R2['p.adj.signif'] == 'ns'] <- 0
R2['p.adj.signif'][R2['p.adj.signif'] == '*'] <- 1
R2['p.adj.signif'][R2['p.adj.signif'] == '**'] <- 2


R2['p.adj.signif'][R2['p.adj.signif'] == 0] <- 'NO'
R2['p.adj.signif'][R2['p.adj.signif'] == 1] <- 'YES'
R2['p.adj.signif'][R2['p.adj.signif'] == 2] <- 'YES'

#write_xlsx(R2, 'R2.xlsx')

# Хэш-функции------------------------------------------

hashes <- sapply(data$responseid,function(x){digest(x, algo = 'murmur32')})
hashes

result <- strtoi(substring(hashes, 2), base = 16)
result

data$T2 <- as.numeric(result %% 100 < 25 )
data$T3 <- as.numeric(result %% 100 >= 25 & result %% 100 < 50 )
data$T4 <- as.numeric(result %% 100 >= 50 & result %% 100 < 75 )
data$T5 <- as.numeric(result %% 100 >= 75 & result %% 100 <= 99 )

data['T2'][data['T2'] == 1] <- 2
data['T3'][data['T3'] == 1] <- 3
data['T4'][data['T4'] == 1] <- 4
data['T5'][data['T5'] == 1] <- 5

data['T2'][data['T2'] == 0] <- 1
data['T3'][data['T3'] == 0] <- 1
data['T4'][data['T4'] == 0] <- 1
data['T5'][data['T5'] == 0] <- 1


data$T6 <- data$T2*data$T3*data$T4*data$T5

data['T6'][data['T6'] == 2] <- 'Control'
data['T6'][data['T6'] == 3] <- 'Alt-Fact'
data['T6'][data['T6'] == 4] <- 'Facts'
data['T6'][data['T6'] == 5] <- 'Fact-Check'

table(data$T6)#проверяем, что количество наблюдение распределено +- равномерно

#Проверим качество рандомизации 
new_data3 <- data %>% select(children, num_children, Married,Single,income,
                             Student,Unemployed,Full_or_part_time_worker,Retired,Source_of_income_wages,Source_of_income_social_benefits,
                             Source_of_income_pension,Source_of_news_TV,Source_of_news_radio,Source_of_news_internet,
                             Religion_Catholic,Religion_Muslim,Religion_none,Voted_in_2012_Hollande,
                             Voted_in_2012_Sarkozy,Voted_in_2012_Melenchon,Voted_in_2012_Le_Pen,
                             Voted_in_2012_Other_candidate, T6)

df3 <- dcast(melt(new_data3), variable ~ T6, mean)

formula3 <- lapply(names(new_data3)[-24], function(var) formula(paste0(var, '~T6')))
Rtests3 <- data.frame() 
Rd3 <- for(i in 1:length(formula3)){   
  Rtest3 <- pairwise_t_test(formula3[[i]], p.adjust.method = "holm", data = new_data3)  %>% select(1, 2, 3, 6, 9) 
  Rtests3 <- rbind(Rtests3, Rtest3)   
  print(Rtest3) }

Rtests3 <- unite(Rtests3, col='group', c('group1', 'group2'), sep='-')
RD3 <- Rtests3 %>% pivot_wider(names_from = group, values_from = p)

names(RD3)[names(RD3) == '.y.'] <- 'variable'

R3 <-merge(df3, RD3, by="variable")

R3 <- transform( 
  R3, p.adj.signif = as.character(p.adj.signif))

R3['p.adj.signif'][R3['p.adj.signif'] == 'ns'] <- 0
R3['p.adj.signif'][R3['p.adj.signif'] == '*'] <- 1
R3['p.adj.signif'][R3['p.adj.signif'] == '**'] <- 2


R3$p.adj.signif <- as.numeric(R3$p.adj.signif)

R3['p.adj.signif'][R3['p.adj.signif'] == 0] <- 'NO'
R3['p.adj.signif'][R3['p.adj.signif'] == 1] <- 'YES'
R3['p.adj.signif'][R3['p.adj.signif'] == 2] <- 'YES'


#write_xlsx(R3, 'R3.xlsx')

#Задание3-----------
Rtests4 <- data.frame() 
Rd4 <- for(i in 1:length(formula)){   
  Rtest4 <- pairwise_t_test(formula[[i]], p.adjust.method = "BY", data = new_data)  %>% select(1, 2, 3, 6, 9) 
  Rtests4 <- rbind(Rtests4, Rtest4)   
  print(Rtest4) }

Rtests4 <- unite(Rtests4, col='group', c('group1', 'group2'), sep='-')
R4 <- Rtests4 %>% pivot_wider(names_from = group, values_from = p)

names(R4)[names(R4) == '.y.'] <- 'variable'

R4 <- replace(R4,is.na(R4),0)

R4 <- transform( 
  R4, p.adj.signif = as.character(p.adj.signif))

R4['p.adj.signif'][R4['p.adj.signif'] == 'ns'] <- 0
R4['p.adj.signif'][R4['p.adj.signif'] == '*'] <- 1
R4['p.adj.signif'][R4['p.adj.signif'] == '**'] <- 2


R4$p.adj.signif <- as.numeric(R4$p.adj.signif)
R4$variable <- as.numeric(R4$variable)
R4[10, ] <- R4[10, ] + R4[11, ]
R4 <- R4[-c(11), ]

R4[1, 1] <- 'children'
R4[2, 1] <- 'num_children'
R4[3, 1] <- 'Married'
R4[4, 1] <- 'Single'
R4[5, 1] <- 'income'
R4[6, 1] <- 'Student'
R4[7, 1] <- 'Unemployed'
R4[8, 1] <- 'Full_or_part_time_worker'
R4[9, 1] <- 'Retired'
R4[10, 1] <- 'Source_of_income_wages'
R4[11, 1] <- 'Source_of_income_social_benefits'
R4[12, 1] <- 'Source_of_income_pension'
R4[13, 1] <- 'Source_of_news_TV'
R4[14, 1] <- 'Source_of_news_radio'
R4[15, 1] <- 'Source_of_news_internet'
R4[16, 1] <- 'Religion_Catholic'
R4[17, 1] <- 'Religion_Muslim'
R4[18, 1] <- 'Religion_none'
R4[19, 1] <- 'Voted_in_2012_Hollande'
R4[20, 1] <- 'Voted_in_2012_Sarkozy'
R4[21, 1] <- 'Voted_in_2012_Melenchon'
R4[22, 1] <- 'Voted_in_2012_Le_Pen'
R4[23, 1] <- 'Voted_in_2012_Other_candidate'


R4['p.adj.signif'][R4['p.adj.signif'] == 0] <- 'NO'
R4['p.adj.signif'][R4['p.adj.signif'] == 1] <- 'YES'
R4['p.adj.signif'][R4['p.adj.signif'] == 2] <- 'YES'

R4 <-merge(df1, R4, by="variable")
#write_xlsx(R4, 'R4.xlsx')

###2.3 Оценка Эффекта---------------------------------------------------------------

data$Alt_facts <- as.integer(data$treatment == 'Alt-Fact')
data$Fact_Check <- as.integer(data$treatment == 'Fact-Check')
data$Facts <- as.integer(data$treatment == 'Facts')

data <- transform( 
  data, region_residence = as.character(region_residence))
data <- transform( 
  data, quot_age = as.character(quot_age))
data <- transform( 
  data, quota_educ = as.character(quota_educ))
data <- transform( 
  data, income = as.character(income))
data <- transform( 
  data, education = as.character(education))
data <- transform( 
  data, religion = as.character(religion))

as.factor(data$region_residence)
as.factor(data$quot_age)
as.factor(data$quota_educ)
as.factor(data$income)
as.factor(data$education)
as.factor(data$religion)

data$voted_2012_Hollande <- as.integer(data$whomvoted_past == 1)
data$voted_2012_Sarkozy <- as.integer(data$whomvoted_past == 2)
data$voted_2012_Melenchon <- as.integer(data$whomvoted_past == 3)
data$voted_2012_MLP <- as.integer(data$whomvoted_past == 4)
data$voted_2012_other <- as.integer(data$whomvoted_past == 7)
data$voted_2012_nv <- as.integer(data$whomvoted_past == 6)


data$voted_2012_Hollande_Alt <- data$voted_2012_Hollande*data$Alt_facts
data$voted_2012_Sarkozy_Alt <- data$voted_2012_Sarkozy*data$Alt_facts
data$voted_2012_Melenchon_Alt <- data$voted_2012_Melenchon*data$Alt_facts
data$voted_2012_MLP_Alt <- data$voted_2012_MLP*data$Alt_facts
data$voted_2012_other_Alt <- data$voted_2012_other*data$Alt_facts
data$voted_2012_nv_Alt <- data$voted_2012_nv*data$Alt_facts


data$voted_2012_Hollande_Facts <- data$voted_2012_Hollande*data$Facts
data$voted_2012_Sarkozy_Facts <- data$voted_2012_Sarkozy*data$Facts
data$voted_2012_Melenchon_Facts <- data$voted_2012_Melenchon*data$Facts
data$voted_2012_MLP_Facts <- data$voted_2012_Melenchon*data$Facts
data$voted_2012_other_Facts <- data$voted_2012_other*data$Facts
data$voted_2012_nv_Facts <- data$voted_2012_nv*data$Facts


data$voted_2012_Hollande_FactsCH <- data$voted_2012_Hollande*data$Fact_Check
data$voted_2012_Sarkozy_FactsCH <- data$voted_2012_Sarkozy*data$Fact_Check
data$voted_2012_Melenchon_FactsCH <- data$voted_2012_Melenchon*data$Fact_Check
data$voted_2012_MLP_FactsCH <- data$voted_2012_MLP*data$Fact_Check
data$voted_2012_other_FactsCH <- data$voted_2012_other*data$Fact_Check
data$voted_2012_nv_FactsCH <- data$voted_2012_nv*data$Fact_Check

data$dwage <- as.integer(data$source_inc== 1)
data$dmarried <- as.integer(data$marital_status == 2)

#Зависимая переменная - willvote_FN
mo1 <- lm(data = data, willvote_FN ~Alt_facts+Fact_Check+Facts+ sexe + quot_age+ region_residence)
mo2 <- lm(data = data, willvote_FN ~ Alt_facts+Fact_Check+Facts+sexe + dwage+income+religion+dmarried+ quot_age+education+ region_residence)
mo3 <- lm(data = data, willvote_FN ~ Alt_facts+Fact_Check+Facts+sexe + dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+
region_residence)
mo4 <- lm(data = data, willvote_FN ~ Alt_facts+Fact_Check+Facts+sexe +voted_2012_Hollande_FactsCH+voted_2012_Sarkozy_FactsCH+voted_2012_Melenchon_FactsCH+voted_2012_MLP_FactsCH+voted_2012_other_FactsCH+voted_2012_nv_FactsCH+
            voted_2012_Hollande_Facts+voted_2012_Sarkozy_Facts+voted_2012_Melenchon_Facts+voted_2012_MLP_Facts+voted_2012_other_Facts+voted_2012_nv_Facts+
            voted_2012_Hollande_Alt+voted_2012_Sarkozy_Alt+voted_2012_Melenchon_Alt+voted_2012_MLP_Alt+voted_2012_other_Alt+voted_2012_nv_Alt+ 
            dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+region_residence)

summary(mo2)

#Зависимая переменная- economic_reason_for_migration
data$economic_reason_for_migration <- as.integer(data$reason_mig == 1)

mo11 <- lm(data = data, economic_reason_for_migration ~Alt_facts+Fact_Check+Facts+ sexe + quot_age+ region_residence+ quota_educ)
mo22 <- lm(data = data, economic_reason_for_migration ~ Alt_facts+Fact_Check+Facts+sexe + dwage+income+religion+dmarried+ quot_age+education+ region_residence+ quota_educ)
mo33 <- lm(data = data, economic_reason_for_migration ~ Alt_facts+Fact_Check+Facts+sexe + dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+
            region_residence+ quota_educ)
mo44 <- lm(data = data, economic_reason_for_migration ~ Alt_facts+Fact_Check+Facts+sexe +voted_2012_Hollande_FactsCH+voted_2012_Sarkozy_FactsCH+voted_2012_Melenchon_FactsCH+voted_2012_MLP_FactsCH+voted_2012_other_FactsCH+voted_2012_nv_FactsCH+
            voted_2012_Hollande_Facts+voted_2012_Sarkozy_Facts+voted_2012_Melenchon_Facts+voted_2012_MLP_Facts+voted_2012_other_Facts+voted_2012_nv_Facts+
            voted_2012_Hollande_Alt+voted_2012_Sarkozy_Alt+voted_2012_Melenchon_Alt+voted_2012_MLP_Alt+voted_2012_other_Alt+voted_2012_nv_Alt+ 
            dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+region_residence+ quota_educ)

summary(mo44)

#Зависимая переменная- agree_with_MLP
data$agree_with_MLP <- as.integer(data$disagree_FN == 1&2)
mo111 <- lm(data = data, agree_with_MLP ~Alt_facts+Fact_Check+Facts+ sexe + quot_age+ region_residence+ quota_educ)
mo222 <- lm(data = data, agree_with_MLP ~Alt_facts+Fact_Check+Facts+ sexe + dwage+income+religion+dmarried+ quot_age+education+ region_residence+ quota_educ)
mo333 <- lm(data = data, agree_with_MLP ~Alt_facts+Fact_Check+Facts+ sexe + dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+
             region_residence+ quota_educ)
mo444 <- lm(data = data, agree_with_MLP ~Alt_facts+Fact_Check+Facts+ sexe +voted_2012_Hollande_FactsCH+voted_2012_Sarkozy_FactsCH+voted_2012_Melenchon_FactsCH+voted_2012_MLP_FactsCH+voted_2012_other_FactsCH+voted_2012_nv_FactsCH+
             voted_2012_Hollande_Facts+voted_2012_Sarkozy_Facts+voted_2012_Melenchon_Facts+voted_2012_MLP_Facts+voted_2012_other_Facts+voted_2012_nv_Facts+
             voted_2012_Hollande_Alt+voted_2012_Sarkozy_Alt+voted_2012_Melenchon_Alt+voted_2012_MLP_Alt+voted_2012_other_Alt+voted_2012_nv_Alt+ 
             dwage+income+religion+dmarried+ quot_age+education+ voted_2012_Hollande+voted_2012_Sarkozy+voted_2012_Melenchon+voted_2012_MLP+voted_2012_other+voted_2012_nv+region_residence+ quota_educ)

s <- rbind(summary(mo1)$coefficients[2:4],summary(mo2)$coefficients[2:4],summary(mo3)$coefficients[2:4],summary(mo4)$coefficients[2:4],
            summary(mo11)$coefficients[2:4],summary(mo22)$coefficients[2:4],summary(mo33)$coefficients[2:4],summary(mo44)$coefficients[2:4],
            summary(mo111)$coefficients[2:4],summary(mo222)$coefficients[2:4],summary(mo333)$coefficients[2:4],summary(mo444)$coefficients[2:4])
S <- t(s)

s1 <- rbind(summary(mo1)$coefficients[2:4,2],summary(mo2)$coefficients[2:4,2],summary(mo3)$coefficients[2:4,2],summary(mo4)$coefficients[2:4,2],
           summary(mo11)$coefficients[2:4,2],summary(mo22)$coefficients[2:4,2],summary(mo33)$coefficients[2:4,2],summary(mo44)$coefficients[2:4,2],
           summary(mo111)$coefficients[2:4,2],summary(mo222)$coefficients[2:4,2],summary(mo333)$coefficients[2:4,2],summary(mo444)$coefficients[2:4,2])
S1 <- t(s1)

variable <- c('Alt-Facts', 'Fact-Check', 'Facts')
S <- cbind(variable, S)
S1 <- cbind(variable, S1)
colnames(S)[2] <- 'willvote_FN1'
colnames(S)[3] <- 'willvote_FN2'
colnames(S)[4] <- 'willvote_FN3'
colnames(S)[5] <- 'willvote_FN4'
colnames(S)[6] <- 'economic_reason1'
colnames(S)[7] <- 'economic_reason2'
colnames(S)[8] <- 'economic_reason3'
colnames(S)[9] <- 'economic_reason4'
colnames(S)[10] <- 'agree_with_MLP1'
colnames(S)[11] <- 'agree_with_MLP2'
colnames(S)[12] <- 'agree_with_MLP3'
colnames(S)[13] <- 'agree_with_MLP4'

colnames(S1)[2] <- 'willvote_FN1'
colnames(S1)[3] <- 'willvote_FN2'
colnames(S1)[4] <- 'willvote_FN3'
colnames(S1)[5] <- 'willvote_FN4'
colnames(S1)[6] <- 'economic_reason1'
colnames(S1)[7] <- 'economic_reason2'
colnames(S1)[8] <- 'economic_reason3'
colnames(S1)[9] <- 'economic_reason4'
colnames(S1)[10] <- 'agree_with_MLP1'
colnames(S1)[11] <- 'agree_with_MLP2'
colnames(S1)[12] <- 'agree_with_MLP3'
colnames(S1)[13] <- 'agree_with_MLP4'

S <- as.data.frame(S)
S1 <- as.data.frame(S1[ , 2:13])

#write_xlsx(S, 'S.xlsx')
#write_xlsx(S1, 'S.xlsx')


