install.packages('fixest')
library('fixest')
library(readxl)
library(lmtest)
library("knitr") # используем Rmarkdown
library("texreg") # вывод результатов регрессии в тех и html

library("tidyverse") # подключаем ggplot2 (графики), dplyr, tidyr, etc (манипуляции с данными)
library("plm") # работа с панельками
D<-read_excel("prices.xlsx")
D2<-read_excel("prices(6).xlsx")
D3<-read_excel("prices(3).xlsx")

attach(D)

m1 = plm(price ~ target+after+target*after, data = D)
summary(m1)

m2 = plm(price ~ target+after+target*after, data = D2)
summary(m2)

m3 = plm(price ~ target+after+target*after, data = D3)
summary(m3)

#для D2 значимо на 1%, для D3 незначимо
reg_fixest <- feols(price ~ target + after + target*after | region, D3, se = "iid")
summary(reg_fixest)
etable(reg_fixest)

#Незначимо
reg_fixest2 <- feols(price ~ target + after + target*after, panel.id = ~ time + wink, data = D2, se = "iid")
summary(reg_fixest)
etable(reg_fixest2)

#Target и after коллинеарность 
reg_fixest3 <- feols(price ~ target + after + target*after | region + time + wink, data = D3, se = "iid")
summary(reg_fixest3)

#Незначимо 
reg_fixest4 <- feols(price ~ target + after + target*after | region, D3, se = "iid")
summary(reg_fixest4)


reg <- plm(price ~ target + after + target*after, model= "within", effect = "individual", data=D2)
summary(reg)
        

pooltest(reg, m1)

library('coefplot')
coefplot(reg_fixest, drop = 'region')


# Группируем на уровень неделя-винк
library('dplyr')
D_agr <- D3 %>% group_by(wink, time) %>% summarise(after = sum(after)>0, target = sum(target)>0)

# Рисуем график
library('panelView')
## Для переменной after
D_agr$after <- as.numeric(D_agr$after)
panel <- panelview(1 ~ after, data = D_agr, index = c('wink', 'time'))
panel + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4))

## Для переменной target
D_agr$target <- as.numeric(D_agr$target)
panel <- panelview(1 ~ target, data = D_agr, index = c('wink', 'time'))
panel + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4))

D2$wink <- as.factor(D2$wink)
summary(D2)
