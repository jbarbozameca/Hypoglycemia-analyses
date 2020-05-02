library(tidyverse)
data_hypoglycemia=base_hypoglycemia
str(data_hypoglycemia)
data_hypoglycemia$`Sexo del RN`=factor(data_hypoglycemia$`Sexo del RN`,levels = c(2,1),labels = c("Mujer","Varón"))
data_hypoglycemia$`Tipo de Parto`=factor(data_hypoglycemia$`Tipo de Parto`,levels = c(1,2),labels = c("Vaginal","Cesárea"))
data_hypoglycemia$`Uso de Drogas`=factor(data_hypoglycemia$`Uso de Drogas`,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$`Inducción del parto`=factor(data_hypoglycemia$`Inducción del parto`,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$`Hijo de Madre Diabética`=factor(data_hypoglycemia$`Hijo de Madre Diabética`,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$RCIU=factor(data_hypoglycemia$RCIU,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$`Madre THE`=factor(data_hypoglycemia$`Madre THE`,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$SALAM=factor(data_hypoglycemia$SALAM,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$`Estrés perinatal`=factor(data_hypoglycemia$`Estrés perinatal`,levels = c(2,1),labels = c("No","Si"))
data_hypoglycemia$`Hipoglucemia neonatal`=factor(data_hypoglycemia$`Hipoglucemia neonatal`,levels = c(1,2), labels = c("si","no"))
str(data_hypoglycemia)
summary(data_hypoglycemia)
View(data_hypoglycemia)
attach(data_hypoglycemia)

library(tableone)
tabla1_hypoglycemia=CreateTableOne(data=data_hypoglycemia, strata = "Hipoglucemia neonatal")
tabla2_hypoglycemia=print(tabla1_hypoglycemia,showAllLevels=T);tabla2_hypoglycemia
write.csv(tabla2_hypoglycemia,file = "Tabla1_hypo.csv")

library(survey)
library(stats)
modelo3=glm(data_hypoglycemia$`Hipoglucemia neonatal`~
              data_hypoglycemia$`Hijo de Madre Diabética`+ 
              data_hypoglycemia$SALAM+
              data_hypoglycemia$RCIU+
              data_hypoglycemia$`Estrés perinatal`,
            data=data_hypoglycemia,
            family = binomial(link = "logit"))
            
library(sjPlot)
tab_model(modelo3,show.p=T)



