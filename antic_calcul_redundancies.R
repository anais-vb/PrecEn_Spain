library(tidyverse)
install.packages("ggthemes")
library(ggthemes)

# Creamos base de datos con indicadores de pobreza energetica 
Indicador <- c('2M (% hogares)', 
               'HEP (% hogares)', 
               'Retrasos (% población)', 
               'Temperatura (% población)')
Año2016 <- c(16.7,11.3, 7.8, 10.1)
Año2017 <- c(17.3, 10.7, 7.4, 8)
Año2018 <- c(16.9, 11, 7.2, 9)
Año2019 <- c(10.1, 8, 9, 7.6)

PE_ESP <- data.frame(Indicador, Año2016, Año2017, Año2018, Año2019)

PE_ESP <- PE_ESP %>% pivot_longer(c('Año2016', 'Año2017', 'Año2018', 'Año2019'), names_to = "Año", values_to = "Valor")


PE_ESP$Año <- recode(PE_ESP$Año, 
                      "Año2016" = 2016,
                      "Año2017" = 2017, 
                      "Año2018" = 2018, 
                      "Año2019" = 2019)


# FORMULA ANTIGUA CALCULO DE REDUNDANCIAS 

# CREAMOS UNA FUNCIÓN PARA CALCULAR LA REDUNDANCIA 

redundancia <- function(var1,var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  #Total población
  total <-  nrow(foessa2)
  
  #Porcentaje de población afectada por el indicador 1
  ind_1_p <- nrow(foessa2 %>% filter(!!var1 == "Sí"))
  ind_1_p <- (ind_1_p/total)*100
  
  #Porcentaje de población afectada por el indicador 2
  ind_2_p <- nrow(foessa2 %>% filter(!!var2 == "Sí"))
  ind_2_p <- (ind_2_p/total)*100
  
  # Porcentaje de población afectada por ambos indicadores 
  ind_mix <- nrow(foessa2 %>% filter(!!var1 == "Sí" & !!var2 == "Sí"))
  ind_mix <- (ind_mix/total)*100
  
  dos <- c(ind_1_p, ind_2_p)
  
  #Cálculo de la redundancia
  red <- ind_mix/min(dos)
  red
}

#CÁLCUL AMB PESOS

redundancia <- function(var1,var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  
  #Total población
  t1 <- foessa2 %>% as.data.frame(wtable(var1, weights = foessa2$peso))
  
  t1 <- t1 %>% filter(Var1 == "Sum" %>% select(Freq) %>% as.numeric()
                      t2 <- foessa2 %>% as.data.frame(wtable(var2, weights = foessa2$peso))
                      t2 <- t2 %>% filter(Var1 == "Sum" %>% select(Freq) %>% as.numeric()
                                          t <- (t1 + t2)/2
                                          
#Porcentaje de población afectada por el indicador 
ind_1_p <- foessa2 %>% count(!!var1, wt=foessa2$peso) %>% filter(!!var1 == "Sí") %>% select(n) %>% as.numeric()
ind_1_p <- (ind_1_p/t1)*100
                                          
#Porcentaje de población afectada por el indicador 2
ind_2_p <- foessa2 %>% count(!!var2, wt=foessa2$peso) %>% filter(!!var1 == "Sí") %>% select(n) %>% as.numeric()
ind_2_p <- (ind_2_p/t2)*100
                                          
# Porcentaje de población afectada por ambos indicadores 
ind_mix <- nrow(foessa2 %>% filter(!!var1 == "Sí" & !!var2 == "Sí"))
ind_mix <- (ind_mix/t)*100
                                          
dos <- c(ind_1_p, ind_2_p)
                                          
#Cálculo de la redundancia
red <- ind_mix/min(dos)
red
}

class(foessa2$Vulnerabilidad_Energetica)

assoc.twocat(foessa2$Vulnerabilidad_Energetica, 
             foessa2$tenencia_rec, weights = foessa2$peso)
                                  