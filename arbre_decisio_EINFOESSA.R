install.packages(c("rpart", "caTools", "party", "partykit", "rpart.plot"))


library(rpart)
library (dplyr)
library(caTools)
library(party)
library(partykit)
library(rpart.plot)
library(caret)



# Seleccionamos las variables que incluíremos en nuestra base de datos. 
# Eliminaremos las variables numéricas, en tanto utilizaremos el árbol de decisión como método de clasificación
# Algunas variables numéricas que nos interesan, las recodificaremos como categóricas 
# Eliminaremos aquellas variables que añaden ruido innecesario. 

str(foessa2)

# Edad por franjas 

foessa2 <- mutate(foessa2, edad_franjas = case_when(
  edad <= 17 ~ "0-17 años", 
  edad >= 18 & edad <= 25 ~ "18-25 años", 
  edad >= 26 & edad <= 35 ~ "26-35 años", 
  edad >= 36 & edad <= 50 ~ "36-50 años", 
  edad >= 51 & edad <= 64 ~ "51-64 años", 
  edad >= 65 ~ "Más de 65 años"
))

foessa2$edad_franjas <- as.factor(foessa2$edad_franjas)


# Eliminamos las variables que no nos interesan (103 iniciales)

foessat <- select(foessa2, -c(CCAA, 
                              PROVINCIA, 
                              edad, 
                              tamano_hogar, 
                              ingresos_UC, 
                              ingresos_hogar, 
                              gasto_energia, 
                              gasto_agua, 
                              rehab_cocina, 
                              rehab_baño, 
                              rehab_instal, 
                              rehab_calef, 
                              rehab_ventana, 
                              rehab_tabiques,
                              rehab_suelo,
                              rehab_barreras, 
                              ingresos_calidad, 
                              gasto_energia_num, 
                              share_energy,
                              TWO_M_Sí,
                              HEP_Sí,
                              temp_adecuada_No_w,
                              retrasos1_Sí_w,
                              TWO_M_Sí_w,
                              HEP_Sí_w, 
                              Vulnerabilidad_num,
                              retrasos1_Sí, 
                              temp_adecuada_r, 
                              temp_adecuada_No, 
                              TWO_M, 
                              HEP, 
                              retrasos, 
                              temp_adecuada, 
                              retrasos1, 
                              Vulnerabilidad_Energetica,
                              PE, 
                              dinero_gastoscasa, 
                              avisos_cortes
                              ))

# Eliminamos las etiquetas de las variables

# install.packages("sjlabelled")
library(sjlabelled)

foessat <- remove_all_labels(foessat)

str(foessat) # El resultado final nos da una base de datos con 69 variables categóricas


# Primero, dividimos nuestros datos en dos partes como datos de test i entrenamiento

set.seed(123)
sample_data = sample.split(foessat, SplitRatio = 0.75)
train_data <- subset(foessat, sample_data == TRUE)
test_data <- subset(foessat, sample_data == FALSE)


# Creamos el arbol de decisión 

rtree <- rpart(Vulnerabilidad_dummy ~ ., train_data, 
               method = "class")
arbol1 <- rpart.plot(rtree, 
           extra = 4)

# Estudiamos la evolución del error a medida que el árbol va creciendo
summary(rtree) # estadísticas detalladas de cada nodo

printcp(rtree) # estadísticas de resultados

plotcp(rtree) # evolución del error a medida que se incrementan los nodos

# Validamos la capacidad de predicción del árbol con el fichero de validación
testPredRpart <- predict(rtree, newdata = test_data, type = "class")

# Visualizamos una matriz de confusión
table(testPredRpart, test_data$Vulnerabilidad_dummy)

# Calculamos el % de aciertos 
sum(testPredRpart == test_data$Vulnerabilidad_dummy)/ length(test_data$Vulnerabilidad_dummy)*100

# El resultado nos clasifica correctamente un 69,2% de los registros, por lo que es mejorable. 

