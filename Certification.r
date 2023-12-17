############РЕПЛИКАЦИЯ РЕЗУЛЬТАТОВ СТАТЬИ ПРО СЕРТИФИКАЦИЮ КОФЕ В ЭФИОПИИ#######
{
  library(MatchIt)
  library(dplyr)
  library(WeightIt)
  library(tableone)
  library(cobalt)
  library(ggplot2)
  library(survey) 
  library(haven)
  library(optmatch)
}
#подгружаем датасет
data <- read_dta('data-Agecon.dta')
 
#1) оценка эффекта от сертификации с помощью МНК по аналогии с автором
#зависимая переменная - доход в расчете на одного члена домохозяйства, ее надо создать
data$income_per_capita <- ((data$totalincome_hh)/data$hh_size)/30

#еще логарифмируем совокупный доход д/х
data$log_total_income<-log(data$totalincome_hh)

#третья зависимая - потребление на 1 члена д/х
data$percapita_consumption<-data$percapita_consumption/30
model1<-lm(income_per_capita~certified+age_hh+agesq+nonfarmincome_access+logtotal_land+depratio+
             badweat+edu+gender+years_cofeproduction+logmyyield+hh_size, data = data)
#model1<-lm(income_per_capita~certified, data = data)
summary(model1)

model2<-lm(percapita_consumption~certified+age_hh+agesq+nonfarmincome_access+logtotal_land+depratio+
        edu+gender+logmyyield+hh_size, data = data)
summary(model2)

model3<-lm(log_total_income~certified+age_hh+agesq+nonfarmincome_access+logtotal_land+depratio+
             badweat+edu+gender+years_cofeproduction+logmyyield+hh_size, data = data)
summary(model3)

#############ОЦЕНКА ЭФФЕКТА С ПОМОЩЬЮ РАЗНОСТИ СРЕДНИХ############
treated<-data[data$certified==1,]
nontreated<-data[data$certified==0,]
ATE1 <-mean(treated$income_per_capita)-mean(nontreated$income_per_capita)
ATE2 <-mean(treated$log_total_income)-mean(nontreated$log_total_income)
ATE3 <-mean(treated$percapita_consumption)-mean(nontreated$percapita_consumption)
t.test(treated$income_per_capita, nontreated$income_per_capita)
t.test(treated$log_total_income, nontreated$log_total_income)
t.test(treated$percapita_consumption, nontreated$percapita_consumption)
print(ATE3)
##################БАЛАНС КОВАРИАТОВ#############################################
library(tableone)
balance_of_covariates<-CreateTableOne(vars = c("age_hh", "agesq", "gender",
                                               "hh_size", "depratio", "edu", 
                                               "edusq", "years_cofeproduction", "logtotal_land",
                                               "nonfarmincome_access", "totalincome_hh", "percapita_consumption",
                                               "access_credit", "myyield", "badweat", "livestock")
                                      ,strata = 'certified',data = data, test = TRUE)

print(balance_of_covariates)
#все оч плохо


#######3) PSM---------------------------------------------- 
data <- replace(data,is.na(data),0) #удаляем пропуски

# оцениваем меру склонности логит моделью
ps <- glm(certified ~ age_hh + agesq + nonfarmincome_access+ logtotal_land
          + depratio+badweat+edu+gender + years_cofeproduction+ access_credit,
          data=data, 
          family=binomial(link="logit"))
summary(ps)
predict(ps, type = "response")
ps$fitted.values

data$ps <- ps$fitted.values # мера склонности

# выкидываем наблюдения с расчётным вероятностями попадания в тритмент <10% и >90%
data <- data[data$ps > 0.1,]
data <- data[data$ps < 0.9,]

data$weight_ate <- data$certified / data$ps + (1 - data$certified) / (1 - data$ps) # веса

data$Per_capita_income_w<- data$income_per_capita*data$weight_ate
# просто сравним средние перевзвешенные игреки
t.test(data$Per_capita_income_w[data$certified==1],data$Per_capita_income_w[data$certified==0])

### Оценка ATE

model_1 <- lm(income_per_capita ~ certified, data = data, weights = data$weight_ate)
model_2 <- lm(percapita_consumption ~ certified, data = data, weights = data$weight_ate)
model_3 <- lm(log_total_income ~ certified, data = data, weights = data$weight_ate)

summary(model_1)
summary(model_2)
summary(model_3)

#################ОЦЕНКИ#########################################################
stargazer(model_1, model_2, model_3, type = "text")

#Новый баланс ковариатов
data$age_hh_w <- data$age_hh * data$weight_ate
data$agesq_w <- data$agesq * data$weight_ate
data$hh_size_w <- data$hh_size * data$weight_ate
data$depratio_w <-  data$depratio * data$weight_ate
data$edu_w <- data$edu * data$weight_ate
data$edusq_w <- data$edusq * data$weight_ate
data$years_cofeproduction_w <- data$years_cofeproduction * data$weight_ate
data$logtotal_land_w <-  data$logtotal_land * data$weight_ate
data$totalincome_hh_w <- data$totalincome_hh * data$weight_ate
data$myyield_w <- data$myyield * data$weight_ate

balance_of_covariates1<-CreateTableOne(vars=c("age_hh_w", "agesq_w",
                                             "hh_size_w", "depratio_w", "edu_w", 
                                             "edusq_w", "years_cofeproduction_w", "logtotal_land_w",
                                             "totalincome_hh_w",
                                             "myyield_w") ,strata = 'certified',
                                      data = data, test = TRUE)

print(balance_of_covariates1)




##########МЭТЧИНГ методом минимизации суммы попарных расстояний#####################################
#Выбор соседей осуществляется путем решения оптимизационной задачи по минимизации 
#суммы абсолютных попарных расстояний
match_2 <- matchit(certified ~ age_hh + agesq + nonfarmincome_access+ logtotal_land
                   + depratio+badweat+edu+gender + years_cofeproduction+ access_credit,
                   data=data,
                   method = "optimal", estimand = "ATT") # здесь метод ATE не работает
# тут решается оптимизационная задача <- минимизация абсолютной суммы расстояний
summary(match_2) # после мэтчинга N_c = N_t
match_2$weights

plot(match_2, type = "jitter", interactive = FALSE)

data$match_2_weights <- match_2$weights

model_21 <- lm(income_per_capita ~ certified, data = data, weights = match_2$weight)
model_22 <- lm(percapita_consumption ~ certified, data = data, weights = match_2$weight)
model_23 <- lm(log_total_income ~ certified, data = data, weights = match_2$weight)


summary(model_21)
summary(model_22)
summary(model_23)

######################ОЦЕНКИ ДУБЛЬ ДВА##########################################
stargazer(model_21, model_22, model_23, type = "text")

#Новый баланс ковариатов
data$age_hh_w1 <- data$age_hh * data$match_2_weights
data$agesq_w1 <- data$agesq * data$match_2_weights
data$hh_size_w1 <- data$hh_size * data$match_2_weights
data$depratio_w1 <-  data$depratio * data$match_2_weights
data$edu_w1 <- data$edu * data$match_2_weights
data$edusq_w1 <- data$edusq * data$match_2_weights
data$years_cofeproduction_w1 <- data$years_cofeproduction * data$match_2_weights
data$logtotal_land_w1 <-  data$logtotal_land * data$match_2_weights
data$totalincome_hh_w1 <- data$totalincome_hh * data$match_2_weights
data$myyield_w1 <- data$myyield * data$match_2_weights


balance_of_covariates2 <-CreateTableOne(vars=c("age_hh_w1", "agesq_w1",
                                             "hh_size_w1", "depratio_w1", "edu_w1", 
                                             "edusq_w1", "years_cofeproduction_w1", "logtotal_land_w1",
                                             "totalincome_hh_w1",
                                             "myyield_w1") ,strata = 'certified',
                                      data = data, test = TRUE)


print(balance_of_covariates2)




