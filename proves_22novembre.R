foessa <- read.spss("./dades/foessa18.sav",to.data.frame=TRUE) # Cargaremos la base de datos de EINSFOESSA 

as.numeric(foessa$E64_1) -> foessa$E64_1
as.numeric(foessa$E64_2) -> foessa$E64_2

foessa <- foessa %>% 
  replace_with_na_at(.vars = c("E64_2","E64_1"),
                     condition = ~.x == 1)

summary(foessa$E64_2)

levels(foessa$F77)

foessa <- foessa %>% mutate(renta_imputada = case_when(
  F77 == "Por compra, totalmente pagada" ~ 163.2, 
  F77 == "Por herencia o donación" ~ 163.2,
  TRUE ~ 0
))

foessa <- foessa %>% mutate(ingresos_EPOV = ifelse(
  foessa$renta_imputada > 0, 
  foessa$renta_imputada + foessa$INGR_SUMAFIABLE_HOGAR, 
  foessa$INGR_SUMAFIABLE_HOGAR))

foessa$ingresos_EPOV_eq <- foessa$ingresos_EPOV/foessa$UC_HOGAR
summary(foessa$ingresos_EPOV)
summary(foessa$ingresos_EPOV_eq)

summary(foessa$E64_3)

# Recodificación de variables 

foessa$E64_3 <- fct_recode(foessa$E64_3, "0" = "No tiene  gasto")#Recodificamos la variable sobre gasto de energia - que, inicialmente, es un factor - para convertir el nivel "No tiene gasto" en O

#Transformamos la variable gasto_energia en numérica 
foessa$E64_3<- as.numeric(as.character(foessa$E64_3))

summary(foessa$E64_3) # Con la función summary podemos ver las principales medidas de dispersión de la variable 

foessa$gasto_energia_eq <- foessa$E64_3/foessa$UC_HOGAR

foessa$HEP <- ifelse(foessa$gasto_energia_eq < 420/2, "Sí", "No")
foessa$HEP <- as.factor(foessa$HEP)

wtable(foessa$HEP, 
       weights = foessa$peso_corregido5, stat="prop")



foessa18 <- read.spss("./dades/foessa18.sav",to.data.frame=TRUE) # Cargaremos la base de datos de EINSFOESSA 

levels(foessa18$E64_3)

foessa18$gasto_energia <- fct_recode(foessa18$E64_3, "0" = "No tiene  gasto")#Recodificamos la variable sobre gasto de energia - que, inicialmente, es un factor - para convertir el nivel "No tiene gasto" en O

#Transformamos la variable gasto_energia en numérica 
foessa18$gasto_energia_num <- as.numeric(as.character(foessa18$gasto_energia))

#Con la siguiente función calculamos la mediana ponderada de la variable de gasto energético
weightedMedian(foessa18$gasto_energia_num, w= foessa18$peso_corregido5, na.rm = TRUE) 

foessa18 <- foessa18 %>% mutate(share_energy = gasto_energia_num/INGR_SUMAFIABLE_HOGAR*100)

weightedMedian(foessa18$share_energy, w= foessa18$peso_corregido5, na.rm = TRUE) 

foessa18 <- foessa18 %>% mutate("TWO_M" = case_when(
  share_energy > 3.86397*2 ~ "Sí", 
  TRUE ~ "No"))

wtable(foessa18$TWO_M, 
       weights = foessa18$peso_corregido5, 
       stat = "prop")


# BOXPLOT MINIMALISTA

# Generate plot
boxplot = ggplot(foessa2, aes(y = Vulnerabilidad_Energetica, x = ingresos_hogar))
#Stylized Boxplot
boxplot = boxplot + geom_boxplot(outlier.colour = NULL, aes(colour=Vulnerabilidad_Energetica, fill=Vulnerabilidad_Energetica)) + # geom_boxplot(notch=T) to compare groups
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) 
#Theme 
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
#No X Axis
theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x=element_blank())
#No Y Axis
theme = theme_update(axis.text.y=element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.title.y=element_blank())
#No Y Axis Label + Grey Axis Numbers
theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="#334756"), axis.ticks.y= element_line(colour="#334756"))
#Imprimir gráfico
boxplot


foessa2 %>% select(peso, retrasos1_Sí, temp_adecuada_No, TWO_M_Sí, HEP_Sí, def_hogar_Sí) %>% write_sav("foessa_mod2.sav")


foessa2$etnia_dummy <- recode(foessa2$etnia, 
                        "Todos españoles o UE15" = "Esp/UE15", 
                        "Extracomunitario/UE ampliación" = "Otros", 
                        "Gitanos" = "Otros")

crear_tabla("Vulnerabilidad_Energetica", "etnia_dummy", "Vulnerabilidad energética en hogares y colectivos minorizados", "Colectivos", 2)

t1 <- wtable(foessa2$Vulnerabilidad_Energetica,
             foessa2$etnia_dummy, 
             weights=foessa2$peso, mar = FALSE)
chisq1 <- chisq.test(t1)  #Relación significativa
chisq1 # 2.2e-16
assocstats(t1)


























