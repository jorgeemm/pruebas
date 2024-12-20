####Apertura de los datos y librerías####
library(pacman)
p_load(tidyverse,haven,readxl,labelled)


datos <- read_sav("00_datos/3411.sav")
Datos <- read_xlsx("00_datos/titanic.xlsx") 
data <- read_excel("00_datos/Economist_data.xlsx")
datos_reg <- read_dta("00_datos/QoG_basic_2017.dta")


#CIS: recodificar estudios
datos<-datos %>% 
  rename(estudios=ESTUDIOS) %>% 
  mutate(estudios=na_if(estudios,7),
         estudios=na_if(estudios,9)) %>% 
  mutate(estudios=factor(estudios, levels=c(1,2,3,4,5,6),labels=c("sin_estudios", "primaria", "fp basica", "secundaria2", "fp medio", "superiores")))

#CIS: recodificar ideología
datos <- datos %>%
  rename(ideol = ESCIDEOL) %>%
  mutate(ideol = ifelse(ideol > 10, NA, ideol))

#CIS: recodificar sexo
datos<-datos %>% rename(sexo = SEXO)
datos<- datos %>%
  mutate(hombre = ifelse(sexo == 1, 1, 0)) %>%
  mutate(hombre = factor(hombre, levels = c(0, 1), labels = c("Mujer", "Hombre"))) 

#Variable mujer
datos<- datos %>%
  mutate(mujer = ifelse(sexo == 1, 0, 1))

#CIS: voto al PP dicotómica
datos <- datos %>%
  mutate(intvoto_pp = case_when(
    INTENCIONG == 2 ~ 1,  # Si INTENCIONG es 2 (PP), entonces 1
    INTENCIONG == 1 | (INTENCIONG >= 3 & INTENCIONG <= 8996) ~ 0,  # Si INTENCIONG es 1, o entre 3 y 8996, entonces 0
    INTENCIONG %in% c(9977, 9997, 9998, 9999) ~ NA_real_  # 9977, 9997, 9998, 9999 son NA
  ))
val_labels(datos$intvoto_pp) <- c(Otros = 0, PP = 1)

#CIS: Propensión voto al PP
datos <- datos %>% 
  mutate(prop_pp = case_when(
    PROBPARTIDOS_2 > 10 ~ NA_real_,  
    TRUE ~ PROBPARTIDOS_2       
  ))

#CIS: Propensión voto a VOX
datos <- datos %>% 
  mutate(prop_vox = case_when(
    PROBPARTIDOS_3 > 10 ~ NA_real_,  
    TRUE ~ PROBPARTIDOS_3      
  ))

#CIS: Propensión voto al PSOE
datos <- datos %>% 
  mutate(prop_psoe = case_when(
    PROBPARTIDOS_1 > 10 ~ NA_real_,  
    TRUE ~ PROBPARTIDOS_1     
  ))

# Propensión voto a SUMAR
datos <- datos %>% 
  mutate(prop_sumar = case_when(
    PROBPARTIDOS_6 > 10 ~ NA_real_,  
    TRUE ~ PROBPARTIDOS_6       
  ))

#CIS: situación laboral
datos <-datos %>% 
  mutate(situ_lab=as.numeric(SITLAB)) %>% 
  mutate(situ_lab=case_when(
    situ_lab %in% c(2,3)~2,
    situ_lab %in% c(4,5)~3,
    situ_lab == 6~4,
    situ_lab==7~5,
    situ_lab==8~6,
    situ_lab==9~NA,
    T~situ_lab)) %>% 
  mutate(situ_lab=factor(situ_lab, levels=c(1,2,3,4,5,6), labels=c("Trabajador/a","Pensionista","Desempleado/a","Estudiante","Trabajo doméstico","Otra situación" )))

#CIS: edad
datos<- datos %>% rename(edad = EDAD)

#CIS: estudios universitarios
datos <- datos %>% 
  mutate(estudios_universitarios = case_when(
    estudios %in% levels(estudios)[1:5] ~ 0,  # Agrupar niveles 1 a 5 en 0 (sin estudios universitarios)
    estudios %in% levels(estudios)[6] ~ 1,    # Agrupar nivel 6 en 1 (con estudios universitarios)
    TRUE ~ NA_real_                           # Otros valores se asignan como NA
  )) %>%
  # Convertir en factor y etiquetar las categorías
  mutate(estudios_universitarios = factor(estudios_universitarios,
                                          levels = c(0, 1),
                                          labels = c("sin EU", "con EU")))

#CIS: recuerdo voto
datos<-datos %>%
  mutate(recuerdo19=RECUVOTOG) %>% 
  mutate(recuerdo19 = ifelse(recuerdo19 >= 9977, NA, recuerdo19))

datos <- datos %>%
  mutate(recuerdo19 = case_when(
    recuerdo19 == 1 ~ 1,   # PSOE
    recuerdo19 == 2 ~ 2,   # PP
    recuerdo19 == 3 ~ 3,   # VOX
    recuerdo19 == 4 ~ 4,   # Podemos
    recuerdo19 == 5 ~ 5,   # Ciudadanos
    recuerdo19 == 7 ~ 6,   # Más Madrid
    recuerdo19 %in% c(6, 202, 501, 601, 901, 902, 903, 904, 1001, 1201, 1202, 1501, 1601, 1602, 8995) ~ 7, # Otros
    recuerdo19 == 8996 ~ 8, # En blanco
    TRUE ~ NA_real_        
  )) %>%
  mutate(recuerdo19 = factor(recuerdo19,
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                             labels = c("PSOE", "PP", "VOX", "Podemos", "Ciudadanos", "Más Madrid", "Otros", "En blanco")))


#CIS: situación económica España
datos <- datos %>%
  mutate(ecoesp = case_when(
    ECOESP %in% c(1, 2) ~ 1,          
    ECOESP %in% c(3, 4, 5) ~ 0,       
    ECOESP > 5 ~ NA_real_            
  ))

datos<- datos %>% mutate(ecoesp = factor(ecoesp,
                                             levels = c(0, 1),
                                             labels = c("negativa", "positiva"))) 



#CIS: situación económica personal
datos <- datos %>%
  mutate(ecoper = case_when(
    ECOPER %in% c(1, 2) ~ 1,          # agrupa 1 y 2 en 1
    ECOPER %in% c(3, 4, 5) ~ 0,       # agrupa 3, 4, 5 y 0 en 0
    ECOPER > 5 ~ NA_real_             # valores >5 son missing
  ))
datos<- datos %>% mutate(ecoper = factor(ecoper,
                                         levels = c(0, 1),
                                         labels = c("negativa", "positiva"))) 



#QOG: cambiar nombres cpi y gdp
datos_reg<-datos_reg %>% 
  mutate(cpi=ti_cpi,
         gdp=mad_gdppc)





