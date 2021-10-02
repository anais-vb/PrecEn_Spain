library(foreign)
pobgit <- read.spss("pobgit.sav", to.data.frame=TRUE)
warnings()
print(pobgit)
view(pobgit)

#install.packages("fastDummies")
#install.packages("gtools")
#install.packages("plyr")
update.packages("tidyverse")

#load packages 
library(dslabs)
library(tidyverse)
library(ggpubr)
library(reprex) # Compartir errores 
library(tibble)
library(fastDummies)
library(tidyr)
library(forcats)
library(gtools)
library(plyr)
library(knitr)


#Exploring the dataset and cleaning it 
dim(pobgit) #it says the number of observations + number of variables -   It has 1492 obs. and 317 variables 
names(pobgit) #names of variables, we need to select the variables we are interested in 

##A9 - Ingresos totales del hogar 

#Crear una tabla trabajo 
pobgit2 <- as.tibble (pobgit)

print(pobgit2)

# Aplicar función de limpieza de datos factoriales mixtos a numérico 

limp_factor_mixto <- function(X){
  var_num <- as.character(X)
  pobgit2 %>% mutate(var_num = case_when(var_num %in% c("No", "NS", "NC") ~ "0", 
                                          TRUE ~ var_num))
  var_num <- as.numeric(var_num)
  var_num[is.na(var_num)] <- 0
  return(var_num)
}

#Limpiamos la variable A9 que se refiere a los ingresos totales de un hogar 
pobgit2$A9_num <- limp_factor_mixto(pobgit2$A9)

#Limpiamos el conjunto de variables A8 que se refieren a los ingresos del hogar 

    #A8_1: Salarios 
    pobgit2$A8_1_num <- limp_factor_mixto(pobgit2$A8_1)
    print (pobgit2$A8_1_num)
    
    #A8_2: Pensiones 
    pobgit2$A8_2_num <- limp_factor_mixto(pobgit2$A8_2)
    print(pobgit2$A8_2_num)
    
    #A8_3: Desempleo 
    pobgit2$A8_3_num <- limp_factor_mixto(pobgit2$A8_3)
    
    #A8_4: Ayudas 1
    pobgit2$A8_4_num <- limp_factor_mixto(pobgit2$A8_4)
    
    #A8_5: Ayudas 2
    pobgit2$A8_5_num <- limp_factor_mixto(pobgit2$A8_5)
    
    #A8_6: Transferencias 
    pobgit2$A8_6_num <- limp_factor_mixto(pobgit2$A8_6)
    
    #A8_7 Otros Ingresos 
    pobgit2$A8_7_num <- limp_factor_mixto(pobgit2$A8_7)
    

# Summary de la variable numérica de ingresos mensuales en el hogar 
print(pobgit2$A9_num)

# Creamos función para calcular los ingresos totales del hogar de acuerdo con los ingresos por conceptos y totales 

pobgit2 <- pobgit2 %>% mutate (ing_hogar_conc = select(., A8_1_num:A8_7_num) %>% replace(is.na(.), 0) %>% rowSums())
print(pobgit2$ing_hogar_conc)

#Creamos fórmula para (1) detectar qué importe entre los ingresos totales o por conceptos es más alto, y seleccionarlo y 
#(2) detectar si en A9 no hay ingresos, acudir a los conceptos anteriores aunque sean más bajos 

pobgit2$ing_hogar_total <- ifelse(pobgit2$ing_hogar_conc > pobgit2$A9_num, pobgit2$ing_hogar_conc, pobgit2$A9_num)
print(pobgit2$ing_hogar_total)

pobgit2 %>%  select(A9_num, ing_hogar_conc, ing_hogar_total) %>% print()

#Creamos función para limpiar las variables sobre edad, en las que queremos que se mantengan las NA

limp_factor_mixto2 <- function(X){
  var_z <- as.character(X)
  pobgit2 %>% mutate(var_z = case_when(var_z %in% c("No", "NS", "NC") ~ "0", 
                                         TRUE ~ var_z))
  var_z <- as.numeric(var_z)
  #var_z[is.na(var_z)] <- 0
  return(var_z)
}

#Limpiar las variables a usar 
pobgit2$A1_04_num <- limp_factor_mixto2(pobgit2$A1_04)
pobgit2$A2_04_num <- limp_factor_mixto2(pobgit2$A2_04)
pobgit2$A3_04_num <- limp_factor_mixto2(pobgit2$A3_04)
pobgit2$A4_04_num <- limp_factor_mixto2(pobgit2$A4_04)
pobgit2$A5_04_num <- limp_factor_mixto2(pobgit2$A5_04)
pobgit2$A6_04_num <- limp_factor_mixto2(pobgit2$A6_04)
pobgit2$A7_04_num <- limp_factor_mixto2(pobgit2$A7_04)
pobgit2$A8_04_num <- limp_factor_mixto2(pobgit2$A8_04)
pobgit2$A9_04_num <- limp_factor_mixto2(pobgit2$A9_04)
pobgit2$A10_04_num <- limp_factor_mixto2(pobgit2$A10_04)

str(pobgit2$A9_04_num)

#TAMAÑO EQUIVALENTE DEL HOGAR (A1_04 - A7_04)

# Calculo del valor de equivalencia "OCDE modificada", según el cual:
# 1r adulto = 1
# 2o adulto y siguientes = 0.5
# Niños de menos de 14 años = 0.3 


#Crear funció para calcular el peso equivalente en la escala OECD de cada variable 

peso_variable_oecd <- function(x){
  y <- ifelse(x >= 16, "0.5", ifelse(x < 16, "0.3", "0"))
  y <- as.numeric(y)
}

#Calcular el peso de cada miembro de la unidad familiar en cada caso 
pobgit2$A1_04_oecd <- peso_variable_oecd(pobgit2$A1_04_num)
pobgit2$A2_04_oecd <- peso_variable_oecd(pobgit2$A2_04_num)
pobgit2$A3_04_oecd <- peso_variable_oecd(pobgit2$A3_04_num)
pobgit2$A4_04_oecd <- peso_variable_oecd(pobgit2$A4_04_num)
pobgit2$A5_04_oecd <- peso_variable_oecd(pobgit2$A5_04_num)
pobgit2$A6_04_oecd <- peso_variable_oecd(pobgit2$A6_04_num)
pobgit2$A7_04_oecd <- peso_variable_oecd(pobgit2$A7_04_num)
pobgit2$A8_04_oecd <- peso_variable_oecd(pobgit2$A8_04_num)
pobgit2$A9_04_oecd <- peso_variable_oecd(pobgit2$A9_04_num)
pobgit2$A10_04_oecd <- peso_variable_oecd(pobgit2$A10_04_num)

# Una manera alternativa habría sido crear un for loop adaptado a la fila, sumando de forma cumulativa 
pobgit2$A5_04_oecd %>% class()
print(pobgit2 %>% select(A1_04_oecd:hogar_oecd))

#Sumar los pesos de cada hogar (fila/observación)
pobgit2 <- pobgit2 %>% mutate(hogar_oecd = select(., A1_04_oecd:A10_04_oecd) %>% replace(is.na(.), 0) %>% rowSums())
pobgit2$hogar_oecd <- pobgit2$hogar_oecd + 1

#Dividir los ingresos totales del hogar por el tamaño equivalente del hogar
pobgit2$ing_equivalente <- pobgit2$ing_hogar_total/pobgit2$hogar_oecd

print(pobgit2 %>% select(A1_04_oecd:ing_equivalente))
print(pobgit2 %>% select(ing_hogar_total))

summary(pobgit2$ing_equivalente) # Ya tenemos los ingresos equivalentes de cada hogar

# Calcular quintiles de ingresos equivalentes 
pobgit2$quintiles_ingresos <- quantcut(pobgit2$ing_equivalente, q=5, na.rm=TRUE)
levels(pobgit2$quintiles_ingresos)

pobgit2$quintiles_ingresos <- revalue(pobgit2$quintiles_ingresos, c("[0,167]"= "Quintil 1",  "(167,281]" = "Quintil 2", "(281,400]" = "Quintil 3", "(400,560]" = "Quintil 4", "(560,2.28e+03]" = "Quintil 5"))


## CALCULO INDICADOR TEMPERATURA ADECUADA EN FUNCIÓN DE LA METODOLOGÍA EPOV 
# EPOV atribuye el valor de temp_adecuada a cada uno de los miembros del hogar y a partir de entonces calcula el indicador por población

summary(pobgit2$P51_3) #vemos que la variable de temperatura adecuada se distribuye por hogares (cada observación es un hogar)
print(pobgit2$P50) #vemos que esta variable es un factor que nos indica el tamaño del hogar (número de miembros)

#Crear una nueva variable que incluya el tamaño del hogar (miembros) en formato numerico
pobgit2$P50_num <- as.numeric(as.character(pobgit2$P50)) # Creamos la nueva variable de tipo numérico que nos dice el número de miembros del hogar

summary(pobgit2$P50_num)#Resumen numérico del tamaño de los hogares de la muestra 
print(pobgit2$P50_num)

#Convertir  variable factoriales a variables dummies para poder realizar análisis estadísticos 
View(pobgit2)

class(pobgit2$P51_3)

#Creación de variables dummies para la variable de temperatura adecuada en el hogar 
pobgit2 <- dummy_cols(
  pobgit2,
  select_columns = 'P51_3',
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = TRUE,
  split = NULL,
  remove_selected_columns = FALSE
)

summary(pobgit2$P51_3)

##  INDICADOR PE - TEMPERATURA INADECUADA EN EL HOGAR ("Inability to keep home adequately warm")

#Numero de hogares sin una temperatura adecuada 
hogares_temp <- sum(pobgit2$P51_3_No)
hogares_temp # 452 hogares sin temperatura adecuada 

#Porcentaje de hogares sin una temperatura adecuada - quitamos los valores perdidos de la muestra  (4)
anyNA(pobgit2$P51_3) #No hay valores perdidos de tipo NA
sum(pobgit2$'P51_3_NS/NC') # Hay 4 valores perdidos de NS/NC - 0.26% valores perdidos 
porcentaje_hogares_temp <- (hogares_temp/(nrow(pobgit2)-sum(pobgit2$`P51_3_NS/NC`)))*100
porcentaje_hogares_temp # 30.37% de los hogares no tienen una temperatura adecuada 

#Indicador de PE - Temperatura adecuada por población 
temp_df <- pobgit2 %>% filter(P51_3 == "No")
poblacion_temp <- sum(temp_df$P50_num)
poblacion_temp # 1801 personas con una temperatura inadecuada en el hogar 
poblacion_total <- sum(pobgit2$P50_num)

pe_indicador_temp <- poblacion_temp/poblacion_total*100
pe_indicador_temp # El 29.941% de la población no puede mantener una temperatura adecuada en el hogar 

## INDICADOR PE - RETRASOS EN EL PAGO DE FACTURAS ("Arrears on utility bills")

#Creación de variables dummies para la variable de retrasos en las facturas 
pobgit2 <- dummy_cols(
  pobgit2,
  select_columns = 'P51_5',
  remove_first_dummy = FALSE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = TRUE,
  split = NULL,
  remove_selected_columns = FALSE
)

View(pobgit2)

hogares_ret <- sum(pobgit2$P51_5_Sí)
hogares_ret # 758 hogares sin temperatura adecuada 

#Porcentaje de hogares con retrasos en las facturas - quitamos los valores perdidos de la muestra  (49)
anyNA(pobgit2$P51_5) #No hay valores perdidos de tipo NA
sum(pobgit2$'P51_5_NS/NC') # Hay 49 valores perdidos de NS/NC - 3.28% de valores perdidos 
porcentaje_hogares_ret <- (hogares_ret/(nrow(pobgit2)-sum(pobgit2$`P51_5_NS/NC`)))*100
porcentaje_hogares_ret # 52.52% de los hogares tienen retrasos en las facturas de suministros básicos 

#Indicador de PE - Población con retrasos en las facturas de suministros 
ret_df <- pobgit2 %>% filter(P51_5 == "Sí")
poblacion_ret <- sum(ret_df$P50_num)
poblacion_ret # 3128 personas con retrasos en las facturas
poblacion_total <- sum(pobgit2$P50_num)

pe_indicador_ret <- poblacion_ret/poblacion_total*100
pe_indicador_ret # El 52% de la población con retrasos en las facturas


#EXPLORAR EL PORCENTAJE DE FAMILIAS QUE SUFREN AMBOS INDICADORES 
pobgit2_retrasos_temp <- pobgit2 %>% filter(P51_3 == "No" & P51_5 == "Sí")
hogares_ret_temp <- nrow(pobgit2_retrasos_temp)
hogares_ret_temp_perc <- hogares_ret_temp/1492*100
hogares_ret_temp_perc #El 21.64879% de los hogares no pueden mantener una temperatura adecuada Y tienen retrasos en las facturas 


#VARIABLES DE CARACTERIZACIÓN 

#Crear una variable que nos indique si la familia está afectada por uno de los indicadores de pobreza energética disponibles

pobgit2 <- mutate(pobgit2, pe = ifelse(
    P51_3 == "No", "PE", ifelse(
   P51_5 == "Sí", "PE", "No PE")))

class(pobgit2$pe)
levels(pobgit2$pe)

prop.table(table(pobgit2$pe))*100

pobgit2$pe <- as.factor(pobgit2$pe)

summary(pobgit2$quintiles_ingresos)
# POBREZA ENERGÉTICA POR QUINTILES DE RENTA POR UNIDAD DE CONSUMO 

ctable1 <- addmargins(prop.table (table(pobgit2$pe, pobgit2$quintiles_ingresos)))*100
ctable1 # Table PE por quintiles (hogares con ambos indicadores por quintiles)

write.table(ctable1, file = "ctable1.txt", sep = ",", quote = FALSE, row.names = F)

fct_count(pobgit2$quintiles_ingresos) #Ver total de hogares de la muestra en cada quintil 

#Temperatura inadecuada por quintiles por hogar y población 

# Q1
pobgit2 %>% filter (quintiles_ingresos == "Quintil 1") %>% summarise (sum(P50_num)) # población de 1411 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 1") %>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) # Población 419 
419/1411*100 # 29.69525%

temp_q1 <- pobgit2 %>% filter(quintiles_ingresos == "Quintil 1") %>% summarise(sum(P51_3_No))
temp_q1 <- (temp_q1/301)*100
temp_q1 # Porcentaje de hogares en quintil 1 con temp inadecuada 

# Q2
pobgit2 %>% filter (quintiles_ingresos == "Quintil 2") %>% summarise (sum(P50_num)) # población de 1333 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 2") %>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) # Población 489
489/1333*100 # 36.68417

temp_q2 <- pobgit2 %>% filter(quintiles_ingresos == "Quintil 2") %>% summarise(sum(P51_3_No))
temp_q2 <- (temp_q2/296)*100
temp_q2  # Porcentaje de hogares en quintil 2 con temp inadecuada 


# Q3
pobgit2 %>% filter (quintiles_ingresos == "Quintil 3") %>% summarise (sum(P50_num)) # población de 1287 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 3") %>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) # Población 450
450/1287*100 # 34.96503

temp_q3 <- pobgit2 %>% filter(quintiles_ingresos == "Quintil 3") %>% summarise(sum(P51_3_No))
temp_q3 <- (temp_q3/332)*100
temp_q3  # Porcentaje de hogares en quintil 3 con temp inadecuada 

# Q4
pobgit2 %>% filter (quintiles_ingresos == "Quintil 4") %>% summarise (sum(P50_num)) # población de 944 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 4") %>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) # Población 264
264/944*100 # 27.9661

temp_q4 <- pobgit2 %>% filter(quintiles_ingresos == "Quintil 4") %>% summarise(sum(P51_3_No))
temp_q4 <- (temp_q4/264)*100
temp_q4  # Porcentaje de hogares en quintil 4 con temp inadecuada 

# Q5
pobgit2 %>% filter (quintiles_ingresos == "Quintil 5") %>% summarise (sum(P50_num)) # población de 1040 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 5") %>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) # Población 179
179/1040*100 # 17.21154

temp_q5 <- pobgit2 %>% filter(quintiles_ingresos == "Quintil 5") %>% summarise(sum(P51_3_No))
temp_q5 <- (temp_q5/299)*100
temp_q5  # Porcentaje de hogares en quintil 5 con temp inadecuada 


#Retrasos en las facturas por quintiles por población 

# Q1
pobgit2 %>% filter (quintiles_ingresos == "Quintil 1") %>% summarise (sum(P50_num)) # población de 1411 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 1") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) # Población 784 
784/1411*100 # 55.56343%

# Q2
pobgit2 %>% filter (quintiles_ingresos == "Quintil 2") %>% summarise (sum(P50_num)) # población de 1333 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 2") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) # Población 828
828/1333*100 # 62.11553

# Q3
pobgit2 %>% filter (quintiles_ingresos == "Quintil 3") %>% summarise (sum(P50_num)) # población de 1287 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 3") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) # Población 668
668/1287*100 # 51.90365

# Q4
pobgit2 %>% filter (quintiles_ingresos == "Quintil 4") %>% summarise (sum(P50_num)) # población de 944 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 4") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) # Población 477
477/944*100 # 50.52966

# Q5
pobgit2 %>% filter (quintiles_ingresos == "Quintil 5") %>% summarise (sum(P50_num)) # población de 1040 
pobgit2 %>% filter (quintiles_ingresos == "Quintil 5") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) # Población 371
371/1040*100 # 35.67308

#POBREZA ENERGÉTICA POR TAMAÑO DEL HOGAR 

levels(pobgit2$P50)

#Creamos una nueva variable simplificando los valores de la variable P50 (tamaño del hogar)
pobgit2$tamhogar <- fct_collapse(pobgit2$P50, 
                                 "1 miembro" = c("1"), 
                                 "2 miembros" = c("2"), 
                                 "3 miembros" = c("3"), 
                                 "4 miembros" = c("4"), 
                                 "5 miembros o más" = c("5", "6", "7", "8", "9", "10", "11", "12", "15"), 
                                 other_level = NULL)
fct_count(pobgit2$tamhogar)

# Temperatura inadecuada por tamaño del hogar 

# 1 miembro 
pobgit2 %>% filter (tamhogar == "1 miembro") %>% 
  summarise (sum(P50_num)) # Población 74

pobgit2 %>% filter (tamhogar == "1 miembro") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 24 

24/74*100 # 32.43243

# 2 miembros 
pobgit2 %>% filter (tamhogar == "2 miembros") %>% 
  summarise (sum(P50_num)) # Población 514

pobgit2 %>% filter (tamhogar == "2 miembros") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 192 

192/514*100 # 37.35409

# 3 miembros 
pobgit2 %>% filter (tamhogar == "3 miembros") %>% 
  summarise (sum(P50_num)) # Población 825

pobgit2 %>% filter (tamhogar == "3 miembros") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 261 

261/825*100 # 31.63636

# 4 miembros 
pobgit2 %>% filter (tamhogar == "4 miembros") %>% 
  summarise (sum(P50_num)) # Población 1332

pobgit2 %>% filter (tamhogar == "4 miembros") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 312 

312/1332*100 # 23.42342

# 5 miembros o más
pobgit2 %>% filter (tamhogar == "5 miembros o más") %>% 
  summarise (sum(P50_num)) # Población 3270

pobgit2 %>% filter (tamhogar == "5 miembros o más") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 1012 

1012/3270*100 # 30.94801

# Retrasos en las facturas por tamaño del hogar 

# 1 miembro 
pobgit2 %>% filter (tamhogar == "1 miembro") %>% 
  summarise (sum(P50_num)) # Población 74

pobgit2 %>% filter (tamhogar == "1 miembro") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 35 

35/74*100 # 47.2973

# 2 miembros 
pobgit2 %>% filter (tamhogar == "2 miembros") %>% 
  summarise (sum(P50_num)) # Población 514

pobgit2 %>% filter (tamhogar == "2 miembros") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 244 

244/514*100 # 47.47082

# 3 miembros 
pobgit2 %>% filter (tamhogar == "3 miembros") %>% 
  summarise (sum(P50_num)) # Población 825

pobgit2 %>% filter (tamhogar == "3 miembros") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 450 

450/825*100 # 54.54545

# 4 miembros 
pobgit2 %>% filter (tamhogar == "4 miembros") %>% 
  summarise (sum(P50_num)) # Población 1332

pobgit2 %>% filter (tamhogar == "4 miembros") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 600 

600/1332*100 # 45.04505

# 5 miembros o más
pobgit2 %>% filter (tamhogar == "5 miembros o más") %>% 
  summarise (sum(P50_num)) # Población 3270

pobgit2 %>% filter (tamhogar == "5 miembros o más") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 1799 

1799/3270*100 # 55.01529


#POBREZA ENERGÉTICA POR SITUACIÓN DE ACTIVIDAD 

summary(pobgit2$P5) # Quien es la persona de referencia 

#En caso que la persona de referencia sea quien responde -- P13
summary(pobgit2$P13) # Actividad principal si quien contesta la encuesta es la persona de referencia 

#En caso que la persona de referencia sea otra
# A2 - preguntas sobre la relación con la persona entrevistada 
# A6 - actividad principal de la persona en cuestión 

pobgit2$P5_car <- as.character(pobgit2$P5)
pobgit2$P13_car <- as.character(pobgit2$P13)

summary(pobgit2$P5)
names(pobgit2)

pobgit2 <- pobgit2 %>% mutate(actp_yo = case_when(
  P5 == "Es usted la persona de referencia" ~ "Principal", 
  TRUE ~ "No Principal"
))

#No usamos case_when porque al trabajar con factores dificulta mucho la tarea 

# Cambiaremos las etiqeutas de valores en P5 para que coincidan con las existentes en las variables de familiares 

pobgit2$P5 <- revalue(pobgit2$P5, c("Es usted la persona de referencia" = "Es usted la persona de referencia", 
                      "Es su hijo/a" = "Hijo/a, hijastro/a", 
                      "Es su nieto/a" = "Nieto/a, nieto políticoo/a (o pareja de los mismos)", 
                      "Es otro pariente" = "Otro pariente", 
                      "Es su cónyuge/pareja" = "Cónyuge o pareja", 
                      "Es su suegro/a" = "Suegro/a o pareja de los mismos", 
                      "Es su abuelo/a" = "Abuelo/a", 
                      "Otros no parientes" = "Sin parentesco con usted", 
                      "Es su padre/madre" = "Padre/Madre", 
                      "Es su cuñado/a" = "Cuñado/a", 
                      "Es su sobrino/a" = "Sobrino/a", 
                      "Es su hermano/a" = "Hermano/a, hermanastro/a", 
                      "Es su yerno/nuera" = "Nuera/Yerno (o pareja del hijo/a, hijastro/a)", 
                      "Es su primo/a" = "Primo/a", 
                      "NC" = "NC"
                      ))

#Añadimos un level a los factores a comparar, para no obtener errores posteriormente por no coincidencia 
pobgit2$A1_02 <- fct_expand(pobgit2$A1_02,"Es usted la persona de referencia" )
pobgit2$A2_02 <- fct_expand(pobgit2$A2_02,"Es usted la persona de referencia" )
pobgit2$A3_02 <- fct_expand(pobgit2$A3_02,"Es usted la persona de referencia" )
pobgit2$A4_02 <- fct_expand(pobgit2$A4_02,"Es usted la persona de referencia" )
pobgit2$A5_02 <- fct_expand(pobgit2$A5_02,"Es usted la persona de referencia" )
pobgit2$A6_02 <- fct_expand(pobgit2$A6_02,"Es usted la persona de referencia" )
pobgit2$A7_02 <- fct_expand(pobgit2$A7_02,"Es usted la persona de referencia" )
pobgit2$A8_02 <- fct_expand(pobgit2$A8_02,"Es usted la persona de referencia" )
pobgit2$A9_02 <- fct_expand(pobgit2$A9_02,"Es usted la persona de referencia" )
pobgit2$A10_02 <- fct_expand(pobgit2$A10_02,"Es usted la persona de referencia" )

summary(pobgit2$A1_02)

levels(pobgit2$P5)

levels(pobgit2$A9_02)

# Vamos a probar a transformar las variables a copiar en character en vez de factor... 
pobgit3 <- pobgit2
str(pobgit3)

pobgit3$P13 <- as.character(pobgit3$P13)
pobgit3$P5 <- as.character(pobgit3$P5)
pobgit3$A1_06 <- as.character(pobgit3$A1_06)
pobgit3$A2_06 <- as.character(pobgit3$A2_06)
pobgit3$A3_06 <- as.character(pobgit3$A3_06)
pobgit3$A4_06 <- as.character(pobgit3$A4_06)
pobgit3$A5_06 <- as.character(pobgit3$A5_06)
pobgit3$A6_06 <- as.character(pobgit3$A6_06)
pobgit3$A7_06 <- as.character(pobgit3$A7_06)
pobgit3$A8_06 <- as.character(pobgit3$A8_06)
pobgit3$A9_06 <- as.character(pobgit3$A9_06)
pobgit3$A10_06 <- as.character(pobgit3$A10_06)
pobgit3$A1_02 <- as.character(pobgit3$A1_02)
pobgit3$A2_02 <- as.character(pobgit3$A2_02)
pobgit3$A3_02 <- as.character(pobgit3$A3_02)
pobgit3$A4_02 <- as.character(pobgit3$A4_02)
pobgit3$A5_02 <- as.character(pobgit3$A5_02)
pobgit3$A6_02 <- as.character(pobgit3$A6_02)
pobgit3$A7_02 <- as.character(pobgit3$A7_02)
pobgit3$A8_02 <- as.character(pobgit3$A8_02)
pobgit3$A9_02 <- as.character(pobgit3$A9_02)
pobgit3$A10_02 <- as.character(pobgit3$A10_02)


class(pobgit3$P5)
levels(pobgit3$P13)
levels(pobgit3$A1_06)
levels(pobgit3$A1_02)
  
#Creamos una nueva variable con la actividad principal de la persona de referencia en el hogar (act_p)
pobgit3 <- pobgit3 %>% mutate(act_p = ifelse(P5 == "Es usted la persona de referencia", P13, ifelse(
  P5 == A1_02, A1_06, ifelse(
    P5 == A2_02, A2_06, ifelse(
      P5 == A3_02, A3_06, ifelse(
        P5 == A4_02, A4_06, ifelse(
          P5 == A5_02, A5_06, ifelse(
            P5 == A6_02, A6_06, ifelse(
              P5 == A7_02, A7_06, ifelse(
                P5 == A8_02, A8_06, ifelse(
                  P5 == A9_02, A9_06, ifelse(
                    P5 == A10_02, A10_06, NA))))))))))))

view(pobgit3)
class(pobgit3$act_p)

#Comprobamos cuantos NA hemos obtenido 
sum(is.na(pobgit3$act_p)) #Hay 10 NA, por lo tanto no son significativos en relación al total 


pobgit3$act_p <- as.factor(pobgit3$act_p)
levels(pobgit3$act_p) # Ahora mismo hay 22 levels que debemos recodificar a ocupados, parados, jubilados, inactivos 

# Inactivos incluira las situaciones de inactividad reconocidas por el INE que no son jubilados: estudiantes, pensionistas no jubilados, incapacitados, labores del hogar... 

pobgit3$act_p <- fct_explicit_na(pobgit3$act_p, na_level = "Missing")


pobgit3$act_p <- fct_collapse(pobgit3$act_p,
             ocupados=c("Baja por enfermedad o maternidad/paternidad","Trabajador/a en un negocio familiar","Trabajador/a por cuenta ajena (Contrato fijo)","Trabajador/a por cuenta ajena (Contrato temporal)","Trabajador/a por cuenta ajena (Fijo)","Trabajador/a por cuenta ajena sin contrato","Trabajador/a por cuenta propia","Trabajando en un negocio familiar","Trabajando por cuenta propia"),
             parados=c("Parado/a","Parado/a, buscando primer empleo","Parado/a, que trabajó anteriormente"),
             jubilados=c("Jubilado o retirado del trabajo","Jubilado/a o retirado/a del trabajo"),
             inactivos=c("Estudiando/realizando cursos de formación","Incapacitado/a permanente","Labores del hogar","Otra situación","Realizando sin remuneración trabajos sociales, actividades benéficas","Trabajo doméstico no remunerado"),
             other_level= "Missing")


#Hogares que no pueden mantener el hogar a una temperatura adecuada según la actividad
table(pobgit3$act_p, pobgit3$P51_3)

# Hogares con retrasos en las facturas según la actividad 
table(pobgit3$act_p, pobgit3$P51_5)

# % Población - TEMPERATURA INADECUADA  

#Población missing 
pobgit3 %>% filter (act_p == "Missing") %>% 
  summarise (sum(P50_num)) #72

# Población ocupada
pobgit3 %>% filter (act_p == "ocupados") %>% 
  summarise (sum(P50_num)) #2498 

pobgit3 %>% filter (act_p == "ocupados") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #574 No pueden mantener una temperatura adecuada 

574/2498*100 #22.97838% No pueden mantener una temperatura adecuada 

# Población parada
pobgit3 %>% filter (act_p == "parados") %>% 
  summarise (sum(P50_num)) #1637 

pobgit3 %>% filter (act_p == "parados") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #973 #No pueden mantener una temperatura adecuada  

973/1637*100 #59.438% No pueden mantener una temperatura adecuada 

#Población inactiva
pobgit3 %>% filter (act_p == "inactivos") %>% 
  summarise (sum(P50_num)) #1151 

pobgit3 %>% filter (act_p == "inactivos") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #584 #No pueden mantener una temperatura adecuada 

584/1151*100 #50.73849% No pueden mantener una temperatura adecuada 


#Población Jubilada
pobgit3 %>% filter (act_p == "jubilados") %>% 
  summarise (sum(P50_num)) #657 

pobgit3 %>% filter (act_p == "jubilados") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #216 #No pueden mantener una temperatura adecuada 

216/657*100 #32.87671% No pueden mantener una temperatura adecuada 


# % Población - RETRASOS EN LOS PAGOS 

# Población ocupada
pobgit3 %>% filter (act_p == "ocupados") %>% 
  summarise (sum(P50_num)) #2498 

pobgit3 %>% filter (act_p == "ocupados") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #1104 #Tienen retrasos en los pagos 

1104/2498*100 #44.19536% tienen retrasos en pagos 

# Población parada
pobgit3 %>% filter (act_p == "parados") %>% 
  summarise (sum(P50_num)) #1637 

pobgit3 %>% filter (act_p == "parados") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #973 #Tienen retrasos en los pagos 

973/1637*100 #59.438% tienen retrasos en pagos 

#Población inactiva
pobgit3 %>% filter (act_p == "inactivos") %>% 
  summarise (sum(P50_num)) #1151 

pobgit3 %>% filter (act_p == "inactivos") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #698 #Tienen retrasos en los pagos 

698/1151*100 #60.64292% tienen retrasos en pagos 


#Población Jubilada
pobgit3 %>% filter (act_p == "jubilados") %>% 
  summarise (sum(P50_num)) #657 

pobgit3 %>% filter (act_p == "jubilados") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #332 #Tienen retrasos en los pagos 

332/657*100 #50.53272% tienen retrasos en pagos 


# POBREZA ENERGÉTICA EN FUNCIÓN DEL TIPO DE HOGAR 

# Hogares con un miembro de más de 65 años 

class(pobgit3$P1)
class(pobgit3$A1_04_num)

pobgit3 <- pobgit3 %>% mutate(miembro_mayor = case_when(
  P1 >= 65 ~ 'Sí', 
  A1_04_num >= 65 ~ 'Sí',
  A2_04_num >= 65 ~ 'Sí',
  A3_04_num >= 65 ~ 'Sí',
  A4_04_num >= 65 ~ 'Sí',
  A5_04_num >= 65 ~ 'Sí',
  A6_04_num >= 65 ~ 'Sí',
  A7_04_num >= 65 ~ 'Sí',
  A8_04_num >= 65 ~ 'Sí',
  A9_04_num >= 65 ~ 'Sí',
  A10_04_num >= 65 ~  'Sí',
  TRUE ~ 'No'
))

pobgit3$miembro_mayor <- as.factor(pobgit3$miembro_mayor)
class(pobgit3$miembro_mayor)
summary(pobgit3$miembro_mayor) #263 hogares tienen un miembro de más de 65 años

# Población Hogares con una persona +65 con temperatura inadecuada 

pobgit3 %>% filter (miembro_mayor == "Sí") %>% 
  summarise (sum(P50_num)) #891 personas viven hogares con personas de +65

pobgit3 %>% filter (miembro_mayor == "Sí") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) # 291 personas no pueden mantener la temp 

291/891*100 #32.65993% No pueden mantener una temperatura adecuada 


# Población Hogares con una persona +65 con retrasos en el pago de facturas 
pobgit3 %>% filter (miembro_mayor == "Sí") %>% 
  summarise (sum(P50_num)) #891 personas viven hogares con personas de +65

pobgit3 %>% filter (miembro_mayor == "Sí") %>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) # 425 personas tienen retrasos en el pago de facturas

425/891*100 #47.69921% tienen retrasos en el pago de facturas


# Hogares con dos adultos sin hijos con almenos uno de ellos de más de 65 años 

pobgit3 <- pobgit3 %>% mutate(pareja_mayor = case_when(
  P1 >= 65 ~ 'Sí', 
  A1_04_num <= 65 ~ 'Sí',
  TRUE ~ 'No'
))

pobgit3$pareja_mayor <- as.factor(pobgit3$pareja_mayor)
summary(pobgit3$pareja_mayor)

pobgit4 <- pobgit3 %>%  filter (P50_num == 2)
summary(pobgit4$pareja_mayor) # 231  hogares con dos adultos con almenos uno de ellos de más de 65 años

#Total población de adultos con almenos uno de ellos de más de 65 años
pobgit3 %>%  filter (P50_num == 2) %>% filter (pareja_mayor == "Sí")%>% summarise (sum(P50_num)) #462 personas 

#Población de parejas+65 con temperatura inadecuada 
pobgit3 %>%  filter (P50_num == 2) %>% filter (pareja_mayor == "Sí")%>% filter (P51_3 == "No") %>% summarise (sum(P50_num)) #164

164/462*100 #35.49784% tienen temp inadecuada

#Población de parejas+65 con temperatura inadecuada 
pobgit3 %>%  filter (P50_num == 2) %>% filter (pareja_mayor == "Sí")%>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) #212

212/462*100 #45.88745% tienen retrasos en pagos 

# Hogares monoparentales o monomarentales 

pobgit3 <- pobgit3 %>% mutate(monoparental = case_when(
  P50_num == 1 ~ 'No',
  A1_04_num > 18 ~ 'No',
  A2_04_num > 18 ~ 'No',
  A3_04_num > 18 ~ 'No',
  A4_04_num > 18 ~ 'No',
  A5_04_num > 18 ~ 'No',
  A6_04_num > 18 ~ 'No',
  A7_04_num > 18 ~ 'No',
  A8_04_num > 18 ~ 'No',
  A9_04_num > 18 ~ 'No',
  A10_04_num > 18 ~  'No',
  TRUE ~ 'Sí'
))

pobgit3$monoparental <- as.factor(pobgit3$monoparental)
summary(pobgit3$monoparental) #70 hogares son monoparental o monoparental

pobgit3 %>% filter(monoparental == "Sí") %>% summarise (sum(P50_num)) #población de 261


#Población monoparental (ambos sexos) con temperatura inadecuada 
pobgit3 %>%
  filter (monoparental == "Sí") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #130

130/261*100 #49.80843% tienen temp inadecuada

#Población monoparental (ambos sexos) con retrasos en los pagos
pobgit3 %>%  filter (monoparental == "Sí") %>% filter (P51_5 == "Sí") %>% summarise (sum(P50_num)) #158

158/261*100 #60.5364% tienen retrasos en pagos 

#Población monoMARENTAL con temperatura inadecuada 
pobgit3 %>% 
  filter(monoparental == "Sí") %>% 
  filter (P2 == "Mujer") %>% 
  summarise (sum(P50_num)) #Población monomarental 154 

pobgit3 %>%
  filter (P2 == "Mujer") %>%
  filter (monoparental == "Sí") %>% 
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #76

76/154*100 #49.35065% tienen temp inadecuada

#Población monoMARENTAL con retrasos en los pafos
pobgit3 %>%  
  filter (P2 == "Mujer") %>%
  filter (monoparental == "Sí")%>% 
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #98

98/154*100 #63.63636% tienen retrasos en pagos 

#OTRAS VARIABLES DE CARACTERIZACIÓN 

# Densidad poblacional 

summary(pobgit2$TAMANO)

#Hemos creado una tabla de datos para POBLACIÓN 
pobgit_poblacion <- as.data.frame(lapply(pobgit2, rep, pobgit2$P50_num))

glimpse(df_1)

#Población total según densidad poblacional 

t_densidad_pob <- prop.table(table(pobgit_poblacion$TAMANO))*100
write.table(t_densidad_pob, file= "densidad_pob.csv", sep=",")



#Temp inadecuada segun densidad poblacional (POBLACION)
dft_densidad <- filter(pobgit_poblacion, P51_3 != "NS/NC") 
dft_densidad$P51_3 <- fct_drop(dft_densidad$P51_3)

summary(dft_densidad$P51_3)

table_temp_densidad_pob <- prop.table(table(dft_densidad$P51_3, dft_densidad$TAMANO), 2)*100
write.table(table_temp_densidad_pob, file= "table_temp_densidad_pob.csv", sep=",")

dft_densidad$temp_pob

# Retrasos en los pagos segun densidad poblacional (POBLACION)

dfr_densidad <- filter(pobgit_poblacion, P51_5 != "NS/NC") 
dfr_densidad$P51_5 <- fct_drop(dfr_densidad$P51_5)

table_ret_densidad_pob <- prop.table(table(dfr_densidad$P51_5, dfr_densidad$TAMANO), 2)*100
write.table(table_ret_densidad_pob, file= "table_ret_densidad_pob.csv", sep=",")


#Temp inadecuada segun densidad poblacional (hogares)
dft_densidad <- filter(pobgit2, P51_3 != "NS/NC") 
dft_densidad$P51_3 <- fct_drop(dft_densidad$P51_3)

summary(dft_densidad$P51_3)

table_temp_densidad <- prop.table(table(dft_densidad$P51_3, dft_densidad$TAMANO), 2)*100
write.table(table_temp_densidad, file= "table_temp_densidad.csv", sep=",")


# Retrasos en los pagos segun densidad poblacional (hogares)

dfr_densidad <- filter(pobgit2, P51_5 != "NS/NC") 
dfr_densidad$P51_5 <- fct_drop(dfr_densidad$P51_5)

summary(dfr_densidad$P51_5)

table_ret_densidad <- prop.table(table(dfr_densidad$P51_5, dfr_densidad$TAMANO), 2)*100
write.table(table_ret_densidad, file= "table_ret_densidad.csv", sep=",")


# Tabla frecuencias - margin: valor de 1 si se desean proporciones por filas, 2 si se desean por columnas, NULL si se desean frecuencias globales.
temp_densidad
temp_densidad_perc <- prop.table(temp_densidad, margin = 2)

print(temp_densidad_perc)

# Perfil de familia (U2)
view(pobgit2)

#Creamos tabla con los datos de PE + Perfil de familia 
x <- table(pobgit2$pe, pobgit2$U2)
class(x)
x <- prop.table(x)

write.csv(x, file="Prueba1.csv")

#Población afectada temp inadecuada según perfil de familia 
levels(pobgit2$U2) #Tiene 8 niveles, los simplificaremos a 4 almenos 
summary(pobgit2$U2s)

pobgit2$U2s <- fct_collapse(pobgit2$U2, 
                           'Muy pobre/Marginal' = c("Marginal","Muy pobre"),
                           'Pobre/Humilde' = c("Pobre","Se las apañan"), 
                           'Confortable/Próspera' = c("Confortable, por encima de la media","Próspera, acomodada"), 
                           'NS/NC' = c("NS","NC"))

pobgit2 %>%  
  filter( U2s == "Muy pobre/Marginal") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #635 /poblacion total 1588

635/1588*100 # 39.98741

pobgit2 %>%  
  filter( U2s == "Pobre/Humilde") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #1034 /3628

1034/3628*100 #28.50055

pobgit2 %>%  
  filter( U2s == "Confortable/Próspera") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #47 / 574

47/574*100 #8.188153

pobgit2 %>% 
  filter( U2s == "NS/NC") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #85/225 

85/225*100 #4.7196

#Porcentaje de población total según perfil familiar 

1588/6015*100 # 26.40067% Marginal/Muy pobre
3628/6015*100 # 60.31588 Pobre Humilde
574/6015*100 # 9.54281 Confortable 
225/6015*100 # 3.740648 NS/NC

#Retrasos en el pago de facturas según perfil familiar

pobgit2 %>%  
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #3128 de 6015 - población afectada por temp. inadecuada 

pobgit2 %>%  
  filter( U2s == "Muy pobre/Marginal") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #992 /poblacion total 1588

992/1588*100 #62.46851

pobgit2 %>%  
  filter( U2s == "Pobre/Humilde") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #1896 /3628

1896/3628*100 #52.2602

pobgit2 %>%  
  filter( U2s == "Confortable/Próspera") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #160 / 574

160/574*100 # 27.87456

pobgit2 %>%  
  filter( U2s == "NS/NC") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #80/225 

80/225*100 #35.55556


# PERFIL DEL BARRIO (U1)
summary (pobgit2$U1)

pobgit2$U1s <- fct_collapse(pobgit2$U1, 
                            'Suburbio/Zona Marginal' = "Suburbio, zona marginal",
                            'Barrio obrero' = c("Barrio obrero deteriorado","Barrio obrero en buenas condiciones"), 
                            'Barrio Antiguo' = c("Barrio antiguo deteriorado", "Barrio antiguo en buenas condiciones"), 
                            'Zona Rural' = "Viviendas diseminadas o zona rural", 
                            'Zona Residencial Media/Alta'= c("Zona residencial de nivel medio", "Zona residencial de clase alta"),
                            'NS' = "NS")
summary(pobgit2$U1s)

#Temperatura inadecuada según perfil del barrio
pobgit2 %>%  
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #1801

pobgit2 %>%  
  filter( U1s == "Zona Rural") %>%
   filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #56 / 186

56/186*100 # 30.10753

pobgit2 %>%  
  filter( U1s == "Suburbio/Zona Marginal") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #470 /1570

470/1570*100 #29.93631

pobgit2 %>%  
  filter( U1s == "Barrio obrero") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #946 / 2945

946/2945*100 #32.12224

pobgit2 %>%  
  filter( U1s == "Barrio Antiguo") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #263/1094 

263/1094*100 #24.04022

pobgit2 %>%  
  filter( U1s == "Zona Residencial Media/Alta") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #13/56 

13/56*100 #23.21429

pobgit2 %>%  
  filter( U1s == "NS") %>%
  filter (P51_3 == "No") %>% 
  summarise (sum(P50_num)) #53/164 

53/164*100 #32.31707

#Retrasos en el pago de facturas según perfil del barrio

pobgit2 %>%  
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #3128 de 6015 - población afectada por temp. inadecuada 

pobgit2 %>%  
  filter( U1s == "Zona Rural") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #117 /186

117/186*100 # 62.90323

pobgit2 %>%  
  filter( U1s == "Suburbio/Zona Marginal") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #731/1572

731/1572*100 #46.50127 

pobgit2 %>%  
  filter( U1s == "Barrio obrero") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #1580 / 2945

1580/2945*100 # 53.65025

pobgit2 %>%  
  filter( U1s == "Barrio Antiguo") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #547/1094 

547/1094*100 #50

pobgit2 %>%  
  filter( U1s == "Zona Residencial Media/Alta") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #23/56 

23/56*100 #41.07143

pobgit2 %>%  
  filter( U1s == "NS") %>%
  filter (P51_5 == "Sí") %>% 
  summarise (sum(P50_num)) #130/164 

130/164*100 #79.26829

#Porcentaje de población total según perfil de barrio

186/6015*100 # Zona Rural 3.092269
1572/6015*100 # Suburbio/ Zona Marginal 26.13466
2945/6015*100 # Barrio Obrero 48.96093
1094/6015*100 # Barrio Antiguo 18.18786
56/6015*100 # Zona residencial media/alta 0.9310058
164/6015*100 # NSNC 2.726517
