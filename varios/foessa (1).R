library(readr)
library(foreign)
library(dslabs)
library(tidyverse)
library(ggpubr)
library(reprex) # Compartir errores 
library(tibble)
library(fastDummies)
library(tidyr)
library(forcats)
library(MLTools)

foessa2018 <- read.spss(file.choose(),to.data.frame=TRUE)

View(foessa2018)

names(foessa2018)

levels(foessa2018$ppalocupacion)

# Crear una base de datos reducida con las variables de inter?s 

summary(foessa2018$D35)

foessa2 <- foessa2018 %>% select(CCAA = CCAA, 
                                 PROVINCIA = PROVINCIA, 
                                 tamano_hogar = PERSONAS, 
                                 edad = A1, 
                                 sexo = A2, 
                                 educacion = B12, 
                                 salud = C21, 
                                 dependencia = C26, 
                                 ingresos_UC = INGR_UC_FIABLE,
                                 ingresos_hogar = INGR_SUMAFIABLE_HOGAR,
                                 excl4grupos = excl4grupos, 
                                 exclusion = exclusion, 
                                 etnia = etnia, 
                                 monoparental = hmp, 
                                 anciano = anciano, 
                                 menor = menor, 
                                 joven = joven, 
                                 discapacidad = discapacidad, 
                                 barrio_dummy = u1agrup2, 
                                 barrio = U2, 
                                 alojamiento = U3, 
                                 tamano_municipio = tam_habitat, 
                                 ocupado = ocupado, 
                                 parado = parado, 
                                 gasto_energia = E64_3, 
                                 gasto_agua = E64_4, 
                                 retrasos = E65, 
                                 clase_subjetiva = E70, 
                                 evolucion12 = E73, 
                                 avisos_cortes = E75_1_1, 
                                 dinero_gastoscasa = E75_1_2, 
                                 reducir_gfijos = E75_1_4, 
                                 reducir_galim = E75_2_1, 
                                 dieta_inadec = E75_2_2, 
                                 reducir_ocio = E75_4_1, 
                                 perdida_relaciones = E75_4_3, 
                                 insalubridad = F78_2, 
                                 entorno_degradado = F78_4, 
                                 barrio_conflictivo = F78_5, 
                                 necesidad_vivienda = F79, 
                                 dispone_agua = F82_B_1, 
                                 dispone_agua_cal = F82_B_2, 
                                 dispone_elect = F82_B_3, 
                                 dispone_calef = F82_B_6, 
                                 temp_adecuada = F82_B_18, 
                                 ppalocupacion = ppalocupacion)


View(foessa2)

# Ver porcentaje de la muestra que es gitana 

fct_count (foessa2$etnia) 
fct_count(foessa2$exclusion)

class(foessa2$tamano_hogar)
glimpse(foessa2)


#Indicador temperatura inadecuada en el hogar 

summary(foessa2$temp_adecuada)

ind_temp <- 100*prop.table(table(foessa2$temp_adecuada))
ind_temp
#write.table(ind_temp, file= "table_indtemp_foessa.csv", sep=",")

#Indicador retrasos 

levels(foessa2$retrasos)
foessa2$retrasos <- as.character(foessa2$retrasos)
foessa2$retrasos <- str_trim(foessa2$retrasos)
foessa2$retrasos <- as.factor(foessa2$retrasos)

#Simplificamos los niveles de retrasos
foessa2$retrasos1 <- fct_collapse(foessa2$retrasos, 
                                  "S?" = c("S?, dos veces o m?s","S?, solamente una vez"), 
                                  "No" = "No", 
                                  "NSNC" = c("No contesta", "No sabe"))

summary(foessa2$retrasos1)

#Creamos una tabla sin los valores de NSNC para el indicador de retrasos (si fueran NA, la funci?n table ya lo ignorar?a)
df <- filter(foessa2, retrasos1 != "NSNC") 
df$retrasos1<- fct_drop(df$retrasos1)


ind_ret <- 100*prop.table(table(df$retrasos1))
write.table(ind_ret, file= "table_indret2_foessa.csv", sep=",")

#Indicador Gasto energ?tico desproporcionado (2M)

#Recodificamos gasto energ?a - que es un factor - para convertir el level No tiene gasto en O
foessa2$gasto_energia <- fct_recode(foessa2$gasto_energia, "0" = "No tiene  gasto")
#Transformamos el factor en num?rico 
foessa2$gasto_energia_num <- as.numeric(as.character(foessa2$gasto_energia))
levels(foessa2$gasto_energia)
class(foessa2$gasto_energia_num)
summary(foessa2$gasto_energia_num)
class(foessa2$ingresos_hogar)

# 1. Calculation of the share of (equivalised) energy expenditure in
#disposable income for each observation in the dataset

foessa2 <- foessa2 %>% mutate(share_energy = gasto_energia_num*100/ingresos_hogar)
is.na(foessa2$share_energy) <- sapply(foessa2$share_energy, is.infinite)
summary(foessa2$share_energy)

length(foessa2$share_energy)
4728*100/29953 #15.7% valores perdidos 

summary(foessa2$share_energy)

#Creamos indicador que indique si el porcentaje de gasto energ?tico sobre los 
#ingresos del hogar se situan por encima del doble de la mediana 
foessa2 <- foessa2 %>% mutate("TWO_M" = case_when(
  share_energy > 4.128*2 ~ "S?", 
  TRUE ~ "No"))

foessa2$TWO_M <- as.factor(foessa2$TWO_M)#Convertir a factor 
length(foessa2$TWO_M)

#Creamos tabla con resultados
ind_2M <- 100*prop.table(table(foessa2$TWO_M))
write.table(ind_2M, file= "table_2M_foessa.csv", sep=",")

#INDICADOR M/2 o pobreza energ?tica escondida 
foessa2 <- foessa2 %>% mutate("HEP" = case_when(
  share_energy < 4.128/2 ~ "S?", 
  TRUE ~ "No"
))

foessa2$HEP <- as.factor(foessa2$HEP)#Convertir a factor
summary(foessa2$HEP)

#Creamos tabla con resultados
ind_HEP <- 100*prop.table(table(foessa2$HEP))
write.table(ind_HEP, file= "table_HEP_foessa.csv", sep=",")

#Overlapping entre indicadores 
summary(foessa2$retrasos1)
summary(foessa2$temp_adecuada)
summary(foessa2$TWO_M)
summary(foessa2$HEP)

foessa2 <- foessa2 %>% mutate(PE = case_when(
  retrasos1 == "S?" ~ "PE", 
  temp_adecuada == "No" ~ "PE", 
  TWO_M == "S?" ~ "PE", 
  HEP == "S?" ~ "PE", 
  TRUE ~ "NO PE"
))

summary(foessa2$PE)

foessa2$PE <- as.factor(foessa2$PE)


#Creamos una variable para saber por cuantos de los indicadores una persona est? afectada 
df <- dummy_cols(foessa2, 
                 select_columns = c("retrasos1", "temp_adecuada", "TWO_M", "HEP"), 
                 ignore_na = TRUE)
df <- df %>% mutate(PE_Suma = retrasos1_S? + temp_adecuada_No + TWO_M_S? + HEP_S?)
df$PE_Suma <- as.factor(df$PE_Suma)

ind_PESum <- 100*prop.table(table(df$PE_Suma))
ind_PESum
write.table(ind_PESum, file= "table_PESum_foessa.csv", sep=",")


foessa2$PE <- as.factor(foessa2$PE)

ind_PE <- 100*prop.table(table(foessa2$PE))
ind_PE
write.table(ind_PE, file= "table_PE_foessa.csv", sep=",")


# VARIABLES DE CARACTERIZACI?N 

# QUINTIL DE RENTA 					

summary(foessa2$ingresos_UC)
foessa2$quintiles <-  cut(
  foessa2$ingresos_UC,
  breaks = quantile(foessa2$ingresos_UC, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
  labels = c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5"),
  right  = FALSE,
  include.lowest = TRUE
)

summary(foessa2$quintiles)

#Funci?n para crear tablas y 'imprimirlas'

crear_tabla <- function(x, y){
  z <- 100*prop.table(table(x, y), margin = 1)
  print(z)
}

#A?adir a funci?n si se quiere 
#write.table (z, file= "new_table.csv", sep=",", col.names = NA, qmethod = "double")

levels(foessa2$retrasos1)
foessa2$retrasos1 <- recode_factor(foessa2$retrasos1, NSNC = NA_character_)

#Tablas PE con quintiles 

crear_tabla (foessa2$quintiles, foessa2$temp_adecuada)
crear_tabla(foessa2$quintiles, foessa2$retrasos1)
crear_tabla(foessa2$quintiles, foessa2$TWO_M)
crear_tabla(foessa2$quintiles, foessa2$HEP)

#TAMA?O DEL HOGAR
foessa2$tamano_hogarf <- as.factor(foessa2$tamano_hogar)
levels(foessa2$tamano_hogarf)

foessa2$tamano_hogarf <- fct_collapse(foessa2$tamano_hogarf, 
                                 "1 miembro" = c("1"), 
                                 "2 miembros" = c("2"), 
                                 "3 miembros" = c("3"), 
                                 "4 miembros" = c("4"), 
                                 "5 miembros o m?s" = c("5", "6", "7", "8", "9", "10"), 
                                 other_level = NULL)

#Tablas PE seg?n tama?o del hogar
crear_tabla (foessa2$tamano_hogarf, foessa2$temp_adecuada)
crear_tabla(foessa2$tamano_hogarf, foessa2$retrasos1)
crear_tabla(foessa2$tamano_hogarf, foessa2$TWO_M)
crear_tabla(foessa2$tamano_hogarf, foessa2$HEP)

#OCUPACI?N DE LA PERSONA DE REFERENCIA 

crear_tabla (foessa2$ppalocupacion, foessa2$temp_adecuada)
crear_tabla(foessa2$ppalocupacion, foessa2$retrasos1)
crear_tabla(foessa2$ppalocupacion, foessa2$TWO_M)
crear_tabla(foessa2$ppalocupacion, foessa2$HEP)

#TIPO DE HOGAR 

# Hogar con una persona m?s de 65 a?os 
crear_tabla (foessa2$anciano, foessa2$temp_adecuada)
crear_tabla(foessa2$anciano, foessa2$retrasos1)
crear_tabla(foessa2$anciano, foessa2$TWO_M)
crear_tabla(foessa2$anciano, foessa2$HEP)

# Hogar con dos adultos (sin ni?os) y uno de ellos mayor de 65 a?os

foessa2 <- foessa2 %>% mutate(dos_adult_65 = case_when(
  tamano_hogarf == "2 miembros" & menor == "Hogar sin menores de 18 a?os" & anciano == "Hogar con personas de 65 o m?s a?os" ~ "Hogar con dos adultos sin hijos y almenos uno +65", 
  TRUE ~ "Otros hogares"
))

foessa2$dos_adult_65 <- as.factor(foessa2$dos_adult_65)
summary(foessa2$dos_adult_65)

crear_tabla (foessa2$dos_adult_65, foessa2$temp_adecuada)
crear_tabla(foessa2$dos_adult_65, foessa2$retrasos1)
crear_tabla(foessa2$dos_adult_65, foessa2$TWO_M)
crear_tabla(foessa2$dos_adult_65, foessa2$HEP)

# Monoparental/Monomarental
crear_tabla (foessa2$monoparental, foessa2$temp_adecuada)
crear_tabla(foessa2$monoparental, foessa2$retrasos1)
crear_tabla(foessa2$monoparental, foessa2$TWO_M)
crear_tabla(foessa2$monoparental, foessa2$HEP)

#Calefaccion
crear_tabla (foessa2$dispone_calef, foessa2$temp_adecuada)
crear_tabla(foessa2$dispone_calef, foessa2$retrasos1)
crear_tabla(foessa2$dispone_calef, foessa2$TWO_M)
crear_tabla(foessa2$dispone_calef, foessa2$HEP)

#OTRAS VARIABLES DE CARACTERIZACION 

t1 <- table(foessa2$PE,foessa2$quintiles)
plot(t1, col = c("red", "blue"), main = "Pobreza energética por quintiles")

chisq.test(t1)  #Relación singificativa
chisq.test(x = t1)$residuals

t2 <- 100*prop.table(table(foessa2$temp_adecuada, foessa2$etnia),1)
t2
plot(t2, col = c("red", "blue"), main = "Temperatura inadecuada y etnia")

chisq.test(t2, simulate.p.value = TRUE)  #relación significativa entre Temp inadecuada y etnia 

t3 <- 100*prop.table(table(foessa2$retrasos1, foessa2$etnia),2)
t3
plot(t3, col = c("red", "blue"), main = "Retrasos en el pago y etnia")
chisq.test(t3, simulate.p.value = TRUE) #Significativo

t4 <- 100*prop.table(table(foessa2$TWO_M, foessa2$etnia),2)
t4
plot(t4, col = c("red", "blue"), main = "2M y etnia")
chisq.test(t4, simulate.p.value = TRUE) #Significativo

t5 <- 100*prop.table(table(foessa2$HEP, foessa2$etnia),2)
t5
plot(t4, col = c("red", "blue"), main = "2M y etnia")
chisq.test(t5) #NO Significativo --> Etnia no es relevante

#CREAR UNA BASE DATOS PARA LA REGRESIÓN LOGÍSTICA
names(foessa2)
foessa3 <- foessa2 %>% select(PE, 
                              #quintiles, 
                              etnia, 
                              tamano_municipio, 
                              barrio,
                              parado,
                              joven, 
                              discapacidad, 
                              anciano)

table(foessa3$PE, foessa3$quintiles)
table(foessa3$PE, foessa3$etnia)
table(foessa3$PE, foessa3$tamano_municipio)
table(foessa3$PE, foessa3$barrio)
table(foessa3$PE, foessa3$parado)
table(foessa3$PE, foessa3$joven)
table(foessa3$PE, foessa3$discapacidad)
table(foessa3$PE, foessa3$quintiles)

logistic <- glm(PE ~ etnia, 
                data = foessa3, 
                family = "binomial")
summary(logistic)

coef(logistic)

#Función para convertir logit a probabilidad
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(logistic))

predict(logistic, data.frame(etnia = "Todos españoles o UE15"), type = "response")

#The relationship between logit and probability is not linear, but of s-curve type.
#The coefficients in logit form can be be treated as in normal regression in terms of computing the y-value.
#Transform the logit of your y-value to probability to get a sense of the probability of the modeled event.
# More info: https://sebastiansauer.github.io/convert_logit2prob/ 