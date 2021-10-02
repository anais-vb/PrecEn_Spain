

# Variable sobre deficiencias del hogar: 

levels(foessa2$insalubridad) # Variable sobre insalubridad en el hogar 
levels(foessa2$ruina)# Variable sobre si el hogar se encuentra en estado de ruina 

# Construcción de la variable sobre deficiencias del hogar 
foessa2 <- foessa2 %>% mutate("def_hogar" = case_when(
  insalubridad == "Sí" ~ "Sí",
  insalubridad == "No" ~ "No", 
  ruina == "Sí" ~ "Sí", 
  ruina == "No" ~ "No", 
  TRUE ~ "No"
))

# Construcción de la variable sobre grupos vulnerables 
# Grupos a incluir: anciano, menor, discapacidad, dependencia, monoparental -- justificar en la literatura 

names(foessa2)

# Recodificar niveles de anciano
levels(foessa2$anciano)
foessa2$anciano <- fct_recode(foessa2$anciano, Sí ="Hogar con personas de 65 o más años", No ="Hogar sin personas de 65 o más años")

# Recodificar niveles de menor
levels(foessa2$menor)
foessa2$menor <- fct_recode(foessa2$menor, Sí ="Hogar con uno o más menores de 18 años", No ="Hogar sin menores de 18 años")

# Recodificar niveles de discapacidad
levels(foessa2$discapacidad)
foessa2$discapacidad <- fct_recode(foessa2$discapacidad, Sí ="Hogar con uno o más discapacitados según C25", No ="Hogar sin discapacitados según C25")

# Recodificar niveles de dependencia - simplificar y recodificar en Sí/No

foessa2 <- foessa2 %>% mutate ("dependencia_rec" = case_when(
  dependencia == "Sí, gran dependencia (Grado 3)" ~ "Sí", 
  dependencia == "Sí, severa (Grado 2)" ~ "Sí",
  dependencia == "Sí, moderada (Grado 1)" ~ "Sí",
  dependencia == "Sí, pero no sabe el nivel" ~ "Sí",
  dependencia == "No, pero la he solicitado y estoy a la espera de respuesta" ~ "No",
  dependencia == "No, pero la he solicitado y lo han denegado" ~ "No",
  dependencia == "No, nunca lo he solicitado" ~ "No", 
  TRUE ~ "No"
))

foessa2$dependencia_rec <- as.factor(foessa2$dependencia_rec)
summary(foessa2$dependencia_rec)

#Monoparental 
class(foessa2$monoparental_rec)
foessa2$monoparental_rec <- fct_recode(foessa2$monoparental, Sí ="Hay nucleo monoparental" , No ="No hay nucleo monoparental")


# Construir variable que incluya cualquiera de los siguientes supuestos de vulnerabilidad 
foessa2 <- foessa2 %>% mutate("grupos_vulnerables" = case_when(
  anciano == "Sí" ~ "Sí", 
  dependencia_rec == "Sí" ~ "Sí", 
  menor == "Sí" ~ "Sí", 
  discapacidad == "Sí" ~ "Sí", 
  monoparental_rec == "Sí" ~ "Sí", 
  TRUE ~ "No"
))

foessa2$grupos_vulnerables <- as.factor(foessa2$grupos_vulnerables)
summary(foessa2$grupos_vulnerables)


redundancia(var1 = HEP, 
            var2 = grupos_vulnerables)


# Comprobación de la variable sobre deficiencias del hogar
prop.table(table(foessa2$def_hogar)) # UN 9,2% de la población vive en un hogar con deficiencias 

```

levels(foessa2$tenencia)


foessa2 <- foessa2 %>% mutate("tenencia_rec" = case_when(
  tenencia == "Por compra, totalmente pagada" ~ "Propiedad", 
  tenencia == "Por compra, por pagos pendientes" ~ "Propiedad", 
  tenencia == "Por herencia o donación" ~ "Propiedad", 
  tenencia == "Por otras personas hogares o instituciones" ~ "Gratuita/Semigratuita", 
  tenencia == "Por patrón/empresa por razón de trabajo" ~ "Gratuita/Semigratuita",
  tenencia == "Particular, con muebles" ~ "Alquiler",
  tenencia == "Particular, sin muebles" ~ "Alquiler",
  tenencia == "Realquilada" ~ "Alquiler",
  tenencia == "Alquiler social" ~ "Alquiler",
  tenencia == "Otras" ~ "Otras",
  tenencia == "Ocupada ilegalmente" ~ "Otras"
))

foessa2$tenencia_rec <- as.factor(foessa2$tenencia_rec)
summary(foessa2$tenencia_rec)



# odds ratio con epitools
library(epitools)

foessa2 <- foessa2 %>% mutate(vulnerabilidad_d = case_when(
  Vulnerabilidad_Energetica == "No vulnerable" ~ "No vulnerable",
  Vulnerabilidad_Energetica == "Vulnerabilidad baja" ~ "Vulnerable", 
  Vulnerabilidad_Energetica == "Vulnerabilidad alta" ~ "Vulnerable"))
t1 <- table(foessa2$quintiles, foessa2$vulnerabilidad_d)

oddsratio(t1)$measure



# inferencial _ borrado del documento principal

## Prueba de independencia entre Vulnerabilidad Energética y tipo de familia 

En primer lugar, debemos crear una nueva variable de tipo categórico que nos permita asignar las tipologías destacadas de unidades familiares a cada una de las observaciones de nuestra base de datos. Por el momento, y usando los tipos de família destacados por la Estrategía Nacional contra la Pobreza Energética para la caracterización de la población en situación de vulnerabilidad energética, identificacaremos: 
  * Hogares con (al menos) una persona de más de 65 años (variable)
* Hogares con dos adultos (sin niños) y que al menos uno de los adultos sea mayor de 65 años 
* Hogares monomarentales y monoparentales 

Además de estos tipos de hogares, hemos decidido añadir otras tipologías que consideramos relevantes: 
  * Hogares con una persona con discapacidad 

```{r}

```





## Prueba de independencia entre PE y colectivos minoritarios 

```{r}
levels(foessa2$etnia)[levels(foessa2$etnia)=="Todos españoles o UE15"] <- "Nacional/UE"
levels(foessa2$etnia)[levels(foessa2$etnia)=="Algún extracomunitario o UE ampliacion"] <- "Extracomunitario"

levels(foessa2$etnia)

ta <- table(foessa2$PE, foessa2$etnia)
ta

chisq1 <- chisq.test(ta)
chisq1
```
En este caso, la relación entre variables es estadísticamente significativa. Una vez establecido este punto, si queremos saber qué celdas de la base de datos contribuyen en mayor medida a la puntuación del Chi Cuadrado, debemos calcularlo para cada una de las celdas. Para ello necesitamos los residuos de Pearson, que ya se encuentran incluidos en el cálculo del Chi Cuadrado. 

```{r}
round(chisq1$residuals, 3)
```
A cotninuación visualizaremos los residuos de Pearson mediante el paquete corrplot: 
  ```{r}
corrplot(chisq1$residuals, 
         is.cor = FALSE)

```

La manera de interpretar el anterior gráfico es la siguiente: 
  * Los residuos positivos se representan en azul, y estos indican una asociación positiva entre las variables de la fila y columna. En este caso vemos que los circulos azulas de mayor tamaño se encuentran entre las celdas de población afectada por alguno de los indicadores de vulnerabilidad energética y la población extracomunitaria (migrante) y la población gitana. 
* Los residuos negativos se representan con circulos rojos, este color implica una asociación negativa entre las variables correspondientes. En este cas, podmeos ver una importante asociación negativa entre la condición de **no** estar afectado por ningún indicador de vulnerabilidad energética y ser una persona migrante extracomunitaria o bien ser gitano/a. 

Además de esta prueba de independencia entre la variable `etnia` y la variable agregada de `PE`, podemos analizar la relación y aplicar la prueba de independencia para cada una de las variables correspondienes a los indicadores de vulnerabilidad energética que componen la variable agregada `PE`: 
  
  **a) Etnia y temperatura adecuada del hogar**
  
  ```{r}
t2 <- 100*prop.table(table(foessa2$temp_adecuada, foessa2$etnia),1)
t2
```
Aplicamos la prueba de independencia: 
  
  ```{r}
chisq.test(t2, simulate.p.value = TRUE)  
```
Vemos que existe una relación  significativa entre la variable de temperatura inadecuada en el hogar y etnia. 

**b) Etnia y retrasos en el pago de facturas de suministros**
  
  ```{r}
t3 <- 100*prop.table(table(foessa2$retrasos1, foessa2$etnia),2)
t3
```
Aplicamos la prueba de independencia: 
  
  ```{r}
chisq.test(t3, simulate.p.value = TRUE) #Significativo
```
Vemos que existe una relación  significativa entre la variable de retrasos en el pago de facturas de suministros y etnia. 

**c) Etnia y gasto energético desproporcionado (2M)**
  
  ```{r}
t4 <- 100*prop.table(table(foessa2$TWO_M, foessa2$etnia),2)
t4
```
Aplicamos la prueba de independencia: 
  
  ```{r}
chisq.test(t4, simulate.p.value = TRUE) #Significativo
```
Vemos que existe una relación  significativa entre la variable de gasto energético desproporcionado y etnia. 


**d) Etnia y pobreza enregética escondida (2M)**
  
  ```{r}
t5 <- 100*prop.table(table(foessa2$HEP, foessa2$etnia),2)
t5
```
Aplicamos la prueba de independencia: 
  
  ```{r}
chisq.test(t5) #NO Significativo --> Etnia no es relevante
```
En el último caso, al aplicar la prueba de independencia entre la variable de pobreza energética escondida y etnia, vemos que *no existe una relación significativa* entre las variables. 



