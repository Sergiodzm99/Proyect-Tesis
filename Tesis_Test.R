#---------------------------Análisis de Datos: Escalas_TESIS---------------


#Libreria-----

library(readxl)
library(tidyverse)
library(ggeffects)

#---------------Datos------------------

datos <- read_excel("D:/Documentos/Otros/Labs/Seminario Ana/1. Papit/TESIS/Datos/Test_PrePost_AQ_DASS.xlsx")

#--------------------------Mini data.frame Pre, Post y Pre_Pos----------------

DatosPre <-
  datos %>% 
  select(Px:DASS_DEP) %>% 
  pivot_longer(
    cols = AQ_TOTAL:DASS_DEP, 
    names_to = "EscalasPre", 
    values_to = "DatosPre")

DatosPos <- 
  datos %>% 
  select(contains ("POST"),Px) %>% 
  pivot_longer(
    cols = AQ_TOTAL_POST:DASS_DEP_POST, 
    names_to = "EscalasPost", 
    values_to = "DatosPos")

DatosPRE_POS <- 
  data.frame(DatosPre,DatosPos)%>% 
  select(- Px.1, - EscalasPost) 

DatosPRE_POS <- dplyr::rename(DatosPRE_POS, c(Escalas = EscalasPre,
                                       Pre = DatosPre,
                                       Pos = DatosPos))

DatosPRE_POS <- DatosPRE_POS %>% 
  mutate(Dif = Pre - Pos)

Datos_Piv <-
  DatosPRE_POS %>% 
  select(-Dif) %>% 
  pivot_longer(
    cols = Pre:Pos, 
    names_to = "Condición", 
    values_to = "Puntaje"
  )

#-----------------------Distribucion----------------

# Histrograma 

library(ggdist)

Datos_Piv %>% 
  ggplot(aes(y = Puntaje, fill = Condición, alpha=0.1)) +
  stat_dist_halfeye() +
  coord_flip()+
  facet_wrap(~Escalas, scales = "free_x") + 
  guides(alpha=FALSE) + 
  theme_ggdist()

#Graficos de Caja------

#Datos, select (start Totales)

Datos_Piv$Condición <- factor(Datos_Piv$Condición, levels = c('Pre', 'Pos'))

ggplot(Datos_Piv, aes(x=Condición, y= Puntaje))+
  geom_boxplot(aes(fill=Condición)) +
  facet_wrap(~Escalas, scales = "free_y") + 
  theme_bw() + 
  theme(panel.grid = element_blank())


#------------------------Estadistica Descriptiva-----------------

DatosDescriptivos <- 
DatosPRE_POS %>% 
  group_by(Escalas) %>% 
  summarise(
    Prom_Pre = mean(Pre),
    Prom_Pos = mean(Pos),
    Dif = mean(Dif),
    Desv_Pre = sd(Pre),
    Desv_Pos = sd(Pos)) %>% 
  tibble()

      #writexl::write_xlsx(DatosDescriptivos, path="DatosDescriptivos.xlsx")

#-----------------------Shapiro wilck - test de normalidad----------------

shapiro.test(datos$AQ_TOTAL)
shapiro.test(datos$AQ_FIS)
shapiro.test(datos$AQ_HOS)
shapiro.test(datos$AQ_IRA)
shapiro.test(datos$AQ_VERBAL)
shapiro.test(datos$DASS_TOTAL)
shapiro.test(datos$DASS_ESTRES)
shapiro.test(datos$DASS_ANS)
shapiro.test(datos$DASS_DEP)

shapiro.test(datos$AQ_TOTAL_POST)
shapiro.test(datos$AQ_FIS_POST)
shapiro.test(datos$AQ_HOS_POST)
shapiro.test(datos$AQ_IRA_POST)
shapiro.test(datos$AQ_VERBAL_POST)
shapiro.test(datos$DASS_TOTAL_POST)
shapiro.test(datos$DASS_ESTRES_POST)
shapiro.test(datos$DASS_ANS_POST)
shapiro.test(datos$DASS_DEP)


#Vector de con los p_valor

  #p_value_Shap <- c(0.3319,0.1657,0.9649, 0.5156, 0.6505, 0.3132, 0.1906, 0.6739, 0.3861, 0.652, 0.3461, 0.5648, 0.8267, 0.2602, 0.1036, 0.2849, 0.2485, 0.3861)


#----------------------T-student-------------------------

#T de student

names(datos)

t.test(datos$AQ_TOTAL , y= datos$AQ_TOTAL_POST, paried=TRUE)
t.test(datos$AQ_FIS , y= datos$AQ_FIS_POST, paried=TRUE)
t.test(datos$AQ_HOS , y= datos$AQ_HOS_POST, paried=TRUE)
t.test(datos$AQ_IRA , y= datos$AQ_IRA_POST, paried=TRUE)
t.test(datos$AQ_VERBAL , y= datos$AQ_VERBAL_POST, paried=TRUE)
t.test(datos$DASS_TOTAL , y= datos$DASS_TOTAL_POST, paried=TRUE)
t.test(datos$DASS_ESTRES , y= datos$DASS_ESTRES_POST, paried=TRUE)
t.test(datos$DASS_ANS , y= datos$DASS_ANS_POST, paried=TRUE)
t.test(datos$DASS_DEP , y= datos$DASS_DEP_POST, paried=TRUE)


#Vector de con los p-valor

p_value <- c(0.06838,0.3288,0.02584,0.1391,0.6665,0.001483,0.04499,0.02269,0.005241)


#Incluir vector al data.frame

Datos_T <- data.frame(DatosPRE_POS, p_value) 

#Guardar base de datos nueva 

    ##writexl::write_xlsx(Datos_T, path="Datos_T.xlsx")

    #Datos_T <- read_excel("Datos_T.xlsx")


#----------------------------Visualizacion T-student Histograma--------------


#DataFrame------------------------

Datos_T_Sig <-
  Datos_T%>% 
  filter(p_value<=0.05) %>% 
  tibble() 

    #writexl::write_xlsx(Datos_T_Sig, path="Datos_T_Sig.xlsx")

Datos_T_Sig_Piv <- 
  Datos_T_Sig %>% 
  pivot_longer(
    cols = Pre:Pos, 
    names_to = "Condicion", 
    values_to = "Puntaje") %>% 
  select(-Dif,-p_value)

#Histograma T Student Sig---------------------

Datos_T_Sig_Piv %>% 
  ggplot(aes(y = Puntaje, fill = Condicion, alpha=0.1)) +
  stat_dist_halfeye() +
  coord_flip()+
  facet_wrap(~Escalas, scales = "free_x")

#G. Caja T Student Sig----------

Datos_T_Sig_Piv$Condicion <- factor(Datos_T_Sig_Piv$Condicion, levels = c('Pre', 'Pos'))

ggplot(Datos_T_Sig_Piv, aes(x=Condicion, y= Puntaje))+
  geom_boxplot(aes(fill=Condicion)) +
  facet_wrap(~Escalas, scales = "free_y") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

