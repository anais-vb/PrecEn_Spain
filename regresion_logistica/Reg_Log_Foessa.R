#' ---
#' title: "Regresión logística"
#' author: "Anais Varo"
#' date: "03/02/2021"
#' output: html_document
#' ---
#' 
#' ## Aplicación de la regresión logística a la encuesta FOESSA 
#' En este apartado aplicaremos el método de regresión logística para determinar qué variables influyen en la pobreza y precariedad energética. La regresión logística forma parte de los llamados modelos lineales generalizados y trata de estimar la probabilidad de ocurrencia de un evento binario (éxito/fracaso, sí/no) en función de una serie de variables predictoras. 
#' 
#' Usaremos la base de datos simplificada creada anteriormente. 
#' 
## --------------------------------------------------------------------------------------------------
head(foessa2)

#' 
#' Tenemos una variable dicotómica "PE" que nos dice si cada individuo se encuentra en alguna de las situaciones de pobreza energética. 
#' 
#' Primero exploramos las variables en nuestra base de datos para seleccionar qué variables predictoras queremos usar: 
#' 
## --------------------------------------------------------------------------------------------------
names(foessa2)

#' 
#' Vamos a probar primero a relacionar la variable PE con ingresos por quintiles: 
#' 
## --------------------------------------------------------------------------------------------------
summary(foessa2$quintiles)

#' 
#' Vamos a aplicar una regresión logística: 
#' 
## --------------------------------------------------------------------------------------------------
modelo_barrio <- glm(PE ~ barrio, 
                        data = foessa2, 
                        family = binomial())

summary(modelo_barrio)

#' 
