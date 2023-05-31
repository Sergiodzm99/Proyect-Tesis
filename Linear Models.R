#-----------------------------------Linear Models---------------------------------------------------

#Library -----

library(readxl)
library(tidyverse)
library(ggeffects)
library(ggplot2)

#Data Cortisol-----

Datos_Cort_Pre <-  read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Analisis_Cortisol.xlsx", sheet = 2)
Datos_Cort_Pos <-  read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Analisis_Cortisol.xlsx", sheet = 3)

Datos_CAR <- read_excel ("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Datos_CAR.xlsx")
Datos_CAR_Pre <- select(Datos_CAR, -4,-5,-6)
Datos_CAR_Pos <- select(Datos_CAR, -1,-2,-3)

#Data Test-----

Datos_Test_Pre <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Test_PrePost_AQ_DASS.xlsx",sheet = "PRE")
Datos_Test_Pos <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Test_PrePost_AQ_DASS.xlsx",sheet = 3)

#Data bouth -----

D_Cort_Test_Pre <- data.frame(Datos_Cort_Pre, Datos_Test_Pre) 
D_Cort_Test_Pre <- select(D_Cort_Test_Pre, -8)
D_Cort_Test_Pos <- data.frame(Datos_Test_Pos, Datos_Cort_Pos)

D_CAR_Test_Pre <- data.frame(Datos_Test_Pre, Datos_CAR_Pre) 
D_CAR_Test_Pos <- data.frame(Datos_Test_Pos, Datos_CAR_Pos) 

#Clean environment -----

rm(Datos_Cort_Pre,Datos_Cort_Pos,Datos_CAR,Datos_CAR_Pre,Datos_CAR_Pos,Datos_Test_Pre,Datos_Test_Pos)
  
#Models with all -----

  #Data: D_Cort_Test_Pre (ModA_)

ModA_AQ_TOTAL <- lm(AQ_TOTAL ~ Dia1_Desp_Pre + Dia1_30min_Pre + Dia2_30min_Pre + Dia3_30min_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_Cort_Test_Pre)

ModA_AQ_IRA <- lm(AQ_IRA ~ Dia1_30min_Pre + Dia2_30min_Pre + Dia3_Desp_Pre + Dia3_30min_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_Cort_Test_Pre)

ModA_AQ_HOS <- lm(AQ_HOS ~ Dia2_30min_Pre + Dia3_30min_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_Cort_Test_Pre)

ModA_AQ_VERBAL <- lm(AQ_VERBAL ~ Dia2_30min_Pre + DASS_ESTRES, data = D_Cort_Test_Pre)

ModA_AQ_FIS <- lm(AQ_FIS ~ Dia2_30min_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_Cort_Test_Pre)


  #Data: D_CAR_Test_Pre (ModB_)

ModB_AQ_TOTAL <- lm(AQ_TOTAL ~ CAR_1_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_CAR_Test_Pre)

ModB_AQ_IRA <- lm(AQ_IRA ~ CAR_1_Pre + CAR_2_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_CAR_Test_Pre)

ModB_AQ_HOS <- lm(AQ_HOS ~ CAR_3_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_CAR_Test_Pre)

ModB_AQ_VERBAL <- lm(AQ_VERBAL ~ CAR_1_Pre + CAR_2_Pre + CAR_3_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_CAR_Test_Pre)

ModB_AQ_FIS <- lm(AQ_FIS ~ CAR_1_Pre + CAR_2_Pre + CAR_3_Pre + DASS_TOTAL + DASS_ESTRES + DASS_ANS, data = D_CAR_Test_Pre)


  #Data: D_Cort_Test_Pos (ModC_)

ModC_AQ_TOTAL <- lm(AQ_TOTAL_POST ~ Dia2_30min_Post + Dia3_30min_Post + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST, data = D_Cort_Test_Pos)

ModC_AQ_IRA <- lm(AQ_IRA_POST ~ Dia1_Desp_Post + Dia2_30min_Post + Dia3_Desp_Post + Dia3_30min_Post + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST + DASS_DEP_POST, data = D_Cort_Test_Pos)

ModC_AQ_HOS <- lm(AQ_HOS_POST ~ Dia1_Desp_Post + Dia2_Desp_Post + Dia2_30min_Post + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_DEP_POST, data = D_Cort_Test_Pos)

ModC_AQ_VERBAL <- lm(AQ_VERBAL_POST ~ Dia1_Desp_Post + Dia1_30min_Post + Dia2_Desp_Post + Dia2_30min_Post + Dia3_30min_Post + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST + DASS_DEP_POST, data = D_Cort_Test_Pos)

ModC_AQ_FIS <- lm(AQ_FIS_POST ~ Dia1_Desp_Post + Dia1_30min_Post + Dia2_30min_Post + Dia3_30min_Post + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST, data = D_Cort_Test_Pos)


  #Data: D_CAR_Test_Pos (ModD_)

ModD_AQ_TOTAL <- lm(AQ_TOTAL_POST ~ CAR_2_Pos + CAR_3_Pos + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST, data = D_CAR_Test_Pos)

ModD_AQ_IRA <- lm(AQ_IRA_POST ~ CAR_2_Pos + CAR_3_Pos + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST + DASS_DEP_POST, data = D_CAR_Test_Pos)

ModD_AQ_HOS <- lm(AQ_HOS_POST ~ CAR_1_Pos + CAR_2_Pos + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_DEP_POST, data = D_CAR_Test_Pos)

ModD_AQ_VERBAL <- lm(AQ_VERBAL_POST ~ CAR_2_Pos + CAR_3_Pos + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST + DASS_DEP_POST, data = D_CAR_Test_Pos)

ModD_AQ_FIS <- lm(AQ_FIS_POST ~ CAR_1_Pos + CAR_2_Pos + CAR_3_Pos + DASS_TOTAL_POST + DASS_ESTRES_POST + DASS_ANS_POST, data = D_CAR_Test_Pos)


# Improving Models and Summary -----

  #AQ_TOTAL

ModA_AQ_TOTAL <- lm(AQ_TOTAL ~ Dia1_Desp_Pre + Dia2_30min_Pre + DASS_ANS, data = D_Cort_Test_Pre)
summary(ModA_AQ_TOTAL) 



ModD_AQ_TOTAL <- lm(AQ_TOTAL_POST ~ CAR_2_Pos + DASS_ESTRES_POST, data = D_CAR_Test_Pos)
summary(ModD_AQ_TOTAL)


ModC_AQ_TOTAL <- lm(AQ_TOTAL_POST ~ DASS_ESTRES_POST, data = D_Cort_Test_Pos)
summary(ModC_AQ_TOTAL)

  #AQ_IRA 

ModC_AQ_IRA <- lm(AQ_IRA_POST ~ DASS_ESTRES_POST, data = D_Cort_Test_Pos)
summary(ModC_AQ_IRA)

  #AQ_HOS

ModA_AQ_HOS <- lm(AQ_HOS ~ DASS_ESTRES, data = D_Cort_Test_Pre)
summary(ModA_AQ_HOS)

  #AQ_FIS

ModD_AQ_FIS <- lm(AQ_FIS_POST ~ DASS_ESTRES_POST, data = D_CAR_Test_Pos)
summary(ModD_AQ_FIS)

ModC_AQ_FIS <- lm(AQ_FIS_POST ~ Dia3_30min_Post + DASS_ESTRES_POST + DASS_ANS_POST, data = D_Cort_Test_Pos)
summary(ModC_AQ_FIS)

#Trash Models ----- 

rm(Mod_AQtotal, ModB_AQ_TOTAL, ModA_AQ_IRA, ModB_AQ_IRA, ModD_AQ_IRA, ModB_AQ_HOS, ModC_AQ_HOS, ModD_AQ_HOS, ModA_AQ_VERBAL, ModB_AQ_VERBAL, ModC_AQ_VERBAL, ModD_AQ_VERBAL, ModB_AQ_FIS, ModA_AQ_FIS)

  #Not good enough

rm(ModA_AQ_TOTAL, ModC_AQ_TOTAL, ModD_AQ_FIS)

#Model Assumptions ---- 

library(gvlma) #gvlma() --- (linearity, homoscedasticity, normality and independence)
library(car) #vif () --- variance inflation factor (VIF) ---- multicollinearity

  #Model 1

ModC_AQ_FIS %>% # R^2 = 0.98, Stress + Ans + Cor(+30min)
  gvlma() %>% 
  summary()
anova(ModC_AQ_FIS) #how much variance explain each variable 
vif(ModC_AQ_FIS) #if a Variable is >= 5 there is multicollinearity 

  #Model 2

ModD_AQ_TOTAL %>% # R^2 = 0.97, CAR + Stress
  gvlma() %>% 
  summary()
anova(ModD_AQ_TOTAL)
vif(ModD_AQ_TOTAL)

  #Model 3

ModC_AQ_IRA %>% # R^2 = 0.83, Stress 
  gvlma() %>% 
  summary()
anova(ModC_AQ_IRA)

  #Model 4

ModA_AQ_HOS %>% # R^2 = 0.69, Stress 
  gvlma() %>% 
  summary()
anova(ModA_AQ_HOS)

#Models in Tables APA------------------- 

library(apaTables)

# apa.reg.table(ModC_AQ_FIS, filename = "Modelo_AQ_Fis.doc", table.number = NA,
#              prop.var.conf.level = 0.95)


# apa.reg.table(ModD_AQ_TOTAL, filename = "Modelo_AQ_Total.doc", table.number = NA,
#              prop.var.conf.level = 0.95)


# apa.reg.table(ModC_AQ_IRA, filename = "Modelo_AQ_Ira.doc", table.number = NA,
#              prop.var.conf.level = 0.95)


# apa.reg.table(ModA_AQ_HOS, filename = "Modelo_AQ_Hos.doc", table.number = NA,
#              prop.var.conf.level = 0.95)

##Plot Models -----

## 2D models

  #ModC_AQ_IRA

ggplot(D_Cort_Test_Pos, aes(x=AQ_IRA_POST , y=DASS_ESTRES_POST, alpha=0.9))+
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  labs(title = "Modelo de Ira",
       subtitle = "Regresion Lineal Simple, R^2 = 0.83",
       x = "DASS_Estrés",
       y = "AQ_Ira") +
  guides(alpha=FALSE) + 
  theme_classic()  

  #ModA_AQ_HOS

ggplot(D_Cort_Test_Pre, aes(x=AQ_HOS, y=DASS_ESTRES, alpha=0.9))+
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  labs(title = "Modelo de Hostilidad",
       subtitle = "Regresion Lineal Simple, R^2 = 0.69",
       x = "DASS_Estrés",
       y = "AQ_Hostilidad") +
  guides(alpha=FALSE) + 
  theme_classic()

## Three Dimensional Models Plot -----

library(plot3D)
library(plot3Drgl)

z<-D_CAR_Test_Pos$AQ_FIS_POST  #es importante que los valores z sean de la dependiente. 
y<-D_CAR_Test_Pos$CAR_2_Pos  
x<-D_CAR_Test_Pos$DASS_ESTRES_POST 

scatter3D(x, y, z, theta = 15, phi = 20)
scatter3D(x, y, z, phi = 0, bty ="g")
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "Prediccion de Agresividad", xlab = "Estrés",
          ylab ="CAR", zlab = "Agresividad")

scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "Estrés",
          ylab ="CAR", zlab = "Agresividad")

  #Create other object lm()

objr<-lm(z ~ x+y)
objr

  #Prepare the 3D model

grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

  # Create the interaction liners that search the linear regression 

fitpoints <- predict(objr)

  #Plot in 3D with the linear regression

scatter3D(x, y, z, pch = 18, cex = 2, bty ="g",
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "x = Stress", 
          ylab = "y = CAR", 
          zlab = "                z = Aggressivity",  
          main = "              Multiple Linear Regression Model",
          surf = list(x = x.pred, 
                      y = y.pred, 
                      z = z.pred,  
                      facets = NA, 
                      fit = fitpoints))
  
 
