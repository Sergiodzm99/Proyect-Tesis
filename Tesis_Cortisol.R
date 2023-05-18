#-----------------------------------Thesis_Cortisol---------------------------------------------------

#Library -----

library(readxl)
library(tidyverse)
library(ggeffects)

#Data -----

Datos <-  read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Analisis_Cortisol.xlsx", sheet = 1)

Datos_CAR <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Cortisol/Datos_CAR.xlsx")

#Explore Data -----

names(Datos)
str(Datos)

glimpse(Datos)
head(Datos)
tail(Datos)


#Mini Data.frame ----

DatosPre <-
  Datos %>% 
  select(1:7) %>% 
  pivot_longer(
    cols = 2:7, 
    names_to = "Medicion", 
    values_to = "DatosPre")

DatosPos <- 
  Datos %>% 
  select(Px, contains ("POST")) %>% 
  pivot_longer(
    cols = 2:Dia3_30min_Post, 
    names_to = "Medicion", 
    values_to = "DatosPos")

DatosPRE_POS <- 
  data.frame(DatosPre,DatosPos)%>% 
  select(- Px.1, -Medicion.1) 

DatosPRE_POS <- dplyr::rename(DatosPRE_POS, c(
                                              Pre = DatosPre,
                                              Pos = DatosPos))

Datos_Piv <-
  DatosPRE_POS %>%  
  pivot_longer(
    cols = Pre:Pos, 
    names_to = "Condición", 
    values_to = "Puntaje"
  )

Datos_CAR_Pre <- 
  Datos %>% 
    mutate(CAR_1_Pre = Dia1_Desp_Pre - Dia1_30min_Pre,
           CAR_2_Pre = Dia2_Desp_Pre - Dia2_30min_Pre,
           CAR_3_Pre = Dia3_Desp_Pre - Dia3_30min_Pre,
           CAR_1_Pos = Dia1_Desp_Post - Dia1_30min_Post,
           CAR_2_Pos = Dia2_Desp_Post - Dia2_30min_Post,
           CAR_3_Pos = Dia3_Desp_Post - Dia3_30min_Post) %>% 
    select (Px, CAR_1_Pre,CAR_2_Pre,CAR_3_Pre) %>% 
  pivot_longer(
    cols = 2:CAR_3_Pre, 
    names_to = "Medicion", 
    values_to = "Pre")

Datos_CAR_Pos <- 
  Datos %>% 
  mutate(CAR_1_Pre = Dia1_Desp_Pre - Dia1_30min_Pre,
         CAR_2_Pre = Dia2_Desp_Pre - Dia2_30min_Pre,
         CAR_3_Pre = Dia3_Desp_Pre - Dia3_30min_Pre,
         CAR_1_Pos = Dia1_Desp_Post - Dia1_30min_Post,
         CAR_2_Pos = Dia2_Desp_Post - Dia2_30min_Post,
         CAR_3_Pos = Dia3_Desp_Post - Dia3_30min_Post) %>% 
  select (CAR_1_Pos,CAR_2_Pos,CAR_3_Pos)%>% 
  pivot_longer(
    cols = CAR_1_Pos:CAR_3_Pos, 
    names_to = "Medicion", 
    values_to = "Pos")

Datos_CAR_PRE_POS <- 
  data.frame(Datos_CAR_Pre,Datos_CAR_Pos)%>% 
  select(-Medicion.1) 

Datos_CAR_Piv <-
  Datos_CAR_PRE_POS %>%  
  pivot_longer(
    cols = Pre:Pos, 
    names_to = "Condición", 
    values_to = "Puntaje"
  )

#writexl::write_xlsx(Datos_CAR, path="Datos_CAR.xlsx")

#Distribution -----

#Graficos de Caja

Datos_Piv$Condición <- factor(Datos_Piv$Condición, levels = c('Pre', 'Pos'))

ggplot(Datos_Piv, aes(x=Condición, y= Puntaje))+
  geom_boxplot(aes(fill=Condición)) +
  facet_wrap(~Medicion, scales = "free_y") + 
  theme_bw() + 
  theme(panel.grid = element_blank())


Datos_CAR_Piv$Condición <- factor(Datos_CAR_Piv$Condición, levels = c('Pre', 'Pos'))

ggplot(Datos_CAR_Piv, aes(x=Condición, y= Puntaje))+
  geom_boxplot(aes(fill=Condición)) +
  facet_wrap(~Medicion, scales = "free_y") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

#Descriptive Cortisol-----

DatosDescriptivos <- 
  DatosPRE_POS %>% 
  group_by(Medicion) %>% 
  summarise(
    Max_Pre = max(Pre),
    Max_Pos = max(Pos),
    Min_Pre = min(Pre),
    Min_Pos = min(Pos),
    Prom_Pre = mean(Pre),
    Prom_Pos = mean(Pos),
    Desv_Pre = sd(Pre),
    Desv_Pos = sd(Pos)) %>% 
  tibble()

      #writexl::write_xlsx(DatosDescriptivos, path="DatosDescriptivos.xlsx")

#Descriptive CAR -----

DatosDescriptivosCAR <- 
  Datos_CAR_PRE_POS %>% 
  group_by(Medicion) %>% 
  summarise(
    Max_Pre = max(Pre),
    Max_Pos = max(Pos),
    Min_Pre = min(Pre),
    Min_Pos = min(Pos),
    Prom_Pre = mean(Pre),
    Prom_Pos = mean(Pos),
    Desv_Pre = sd(Pre),
    Desv_Pos = sd(Pos)) %>% 
  tibble()

    #writexl::write_xlsx(DatosDescriptivosCAR, path="DatosDescriptivosCAR.xlsx")


#Shapiro Wilck - Normality Test -----

shapiro.test(Datos$Dia1_Desp_Pre)
shapiro.test(Datos$Dia1_30min_Pre)
shapiro.test(Datos$Dia2_Desp_Pre)
shapiro.test(Datos$Dia2_30min_Pre)
shapiro.test(Datos$Dia3_Desp_Pre)
shapiro.test(Datos$Dia3_30min_Pre)
shapiro.test(Datos$Dia1_Desp_Post)
shapiro.test(Datos$Dia1_30min_Post)
shapiro.test(Datos$Dia2_Desp_Post)
shapiro.test(Datos$Dia2_30min_Post)
shapiro.test(Datos$Dia3_Desp_Post)
shapiro.test(Datos$Dia3_30min_Post)

shapiro.test(Datos_CAR$CAR_1_Pre)
shapiro.test(Datos_CAR$CAR_2_Pre)
shapiro.test(Datos_CAR$CAR_3_Pre)
shapiro.test(Datos_CAR$CAR_1_Pos)
shapiro.test(Datos_CAR$CAR_2_Pos)
shapiro.test(Datos_CAR$CAR_3_Pos)

#T de student -----

t.test(Datos$Dia1_Desp_Pre, y= Datos$Dia1_Desp_Post, paried=TRUE)
t.test(Datos$Dia1_30min_Pre, y= Datos$Dia1_30min_Post, paried=TRUE)
t.test(Datos$Dia2_Desp_Pre, y= Datos$Dia2_Desp_Post, paried=TRUE)
t.test(Datos$Dia2_30min_Pre, y= Datos$Dia2_30min_Post, paried=TRUE)
t.test(Datos$Dia3_Desp_Pre, y= Datos$Dia3_Desp_Post, paried=TRUE)
t.test(Datos$Dia3_30min_Pre, y= Datos$Dia3_30min_Post, paried=TRUE)


t.test(Datos_CAR$CAR_1_Pre, y= Datos_CAR$CAR_1_Pos, paried=TRUE)
t.test(Datos_CAR$CAR_2_Pre, y= Datos_CAR$CAR_2_Pos, paried=TRUE)
t.test(Datos_CAR$CAR_3_Pre, y= Datos_CAR$CAR_3_Pos, paried=TRUE)


