#Library -----

library(readxl)
library(tidyverse)
library(ggeffects)
library(ggplot2)

#Data Cortisol-----
 
Datos_Cort_Pre <-  read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Analisis_Cortisol.xlsx", sheet = 2)
Datos_Cort_Pre <-  select(Datos_Cort_Pre, -1)
Datos_Cort_Pos <-  read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Analisis_Cortisol.xlsx", sheet = 3)
 
Datos_CAR <- read_excel ("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Datos_CAR.xlsx")
Datos_CAR_Pre <- select(Datos_CAR, -4,-5,-6)
Datos_CAR_Pos <- select(Datos_CAR, -1,-2,-3)

rm(Datos_CAR)
 
#Data Test-----
 
Datos_Test_Pre <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Test_PrePost_AQ_DASS.xlsx",sheet = "PRE")
Datos_Test_Pos <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Test_PrePost_AQ_DASS.xlsx",sheet = 3)

#Data Pre & Post ---- 

Datos_Pre <- data.frame(Datos_Test_Pre, Datos_Cort_Pre, Datos_CAR_Pre)

   rm(Datos_Cort_Pre, Datos_CAR_Pre, Datos_Test_Pre)

Datos_Pos <- data.frame(Datos_Test_Pos, Datos_Cort_Pos, Datos_CAR_Pos)

    rm(Datos_Cort_Pos, Datos_CAR_Pos, Datos_Test_Pos)


#Clean Cortisol Data for Matrix
    
    Datos_Pre <-
      Datos_Pre %>% 
      select(- Dia1_Desp_Pre,
             - Dia1_30min_Pre,
             - Dia2_Desp_Pre,
             - Dia3_Desp_Pre,
             - Dia3_30min_Pre,
             - CAR_1_Pre,
             - CAR_2_Pre)
    
    Datos_Pos <- 
      Datos_Pos %>% 
      select(- Dia1_Desp_Post,
             - Dia1_30min_Post,
             - Dia2_Desp_Post,
             - Dia3_Desp_Post, 
             - Dia3_30min_Post, 
             - CAR_1_Pos,
             - CAR_3_Pos)
    
#Change names columns 
  
library(reshape)
  
Datos_Pre = rename(Datos_Pre, c(Dia2_30min_Pre = "Cort_30min", CAR_3_Pre = "CAR"))

#writexl::write_xlsx(Datos_Pos, path= "Datos_Pos.xlsx")

Datos_Pos <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Datos_Pos.xlsx")
  
  
#Correlations ----

library(metan)

Cor_Pre <- corr_coef(Datos_Pre[,-1])
  plot(Cor_Pre,  col.low = "blue", col.mid = "white", col.high = "red", reorder = T)
  
  
Cor_Pos <- corr_coef(Datos_Pos[,-1])
  plot(Cor_Pos,  col.low = "blue", col.mid = "white", col.high = "red", reorder = T)

#Just significant Correlations 
  
  Datos_Pre <-
    Datos_Pre %>% 
    select(- DASS_TOTAL,
           - DASS_ANS,
           - AQ_VERBAL)
  
  Cor_Pre <- corr_coef(Datos_Pre[,-1])
  plot(Cor_Pre,  col.low = "white", col.mid = "skyblue", col.high = "blue", reorder = F)
  
  
  Datos_Pos <- 
    Datos_Pos %>% 
    select(- DASS_DEP,
           - AQ_HOS)
  
  Cor_Pos <- corr_coef(Datos_Pos[,-1])
  plot(Cor_Pos,  col.low = "white", col.mid = "skyblue", col.high = "blue", reorder = T)
  
  
  