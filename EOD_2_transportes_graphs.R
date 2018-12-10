library(survey)
library(dplyr)
library(lubridate)
library(ggplot2)
library(foreign)
library(stringr)



rm(list=ls())

d <- read.csv(file = "transportes.csv")
names(d)

d$SEXO[d$SEXO == 1] <- "hombre"
d$SEXO[d$SEXO == 2] <- "mujer"

d$EDAD2 <- d$EDAD
d$EDAD2[d$EDAD2 %in% 6:16] <- "06a16"
d$EDAD2[d$EDAD2 %in% 17:29] <- "17a29"
d$EDAD2[d$EDAD2 %in% 30:39] <- "30a39"
d$EDAD2[d$EDAD2 %in% 40:49] <- "40a49"
d$EDAD2[d$EDAD2 %in% 50:60] <- "50a59"
d$EDAD2[d$EDAD2 %in% 61:97] <- "61a97"






#GRAPH CANTIDAD DE PERSONAS EN TRÁNSITO POR DIA

design_d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FACTOR, 
                      data = d)
options(survey.lonely.psu="remove")


varstime <- names(d)[29:172]
table7 <- lapply(varstime, function(x){
  form <- make.formula(c(x, "P5_3"))
  d <- data.frame(x, svytable(formula = form, design = design_d))
  names(d) <- c("tiempo", "en_transito", "dia", "freq")
  return(d)
}) ; names(table7) <- varstime
table7_2 <- do.call(what = "rbind", args = table7)


table7_2$tiempo2 <- ymd_hms(
  paste(
    today(), 
    do.call(what = "rbind", 
            args = strsplit(x = as.character(table7_2$tiempo), split = "tviaje_"))[,2],
    sep = "_"), 
  tz = "Mexico/General")
table7_2$dia <- as.character(table7_2$dia)
table7_2$dia[table7_2$dia == "entre_semana"] <- "Entre semana"
table7_2$dia[table7_2$dia == "sabado"] <- "Sabado"

graph_transito_pordia <- table7_2 %>% 
  filter(en_transito == TRUE) %>% 
  
  ggplot() +
  geom_bar(mapping = aes(x = tiempo2, y = freq), stat = "identity", width=120) +
  scale_x_datetime(date_breaks = "hours" , date_labels = "%H-%M") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1, size = 7)) +
  facet_wrap(facets = ~dia, nrow = 1) + 
  xlab(label = "Lapsos de 10 minutos") + 
  ylab(label = "Cantidad de personas en tránsito") +
  ggtitle(label = "Cantidad de personas en tránisto por lapsos de 10 minutos en la ZMVM",
          subtitle = "Segun reportado por los entrevistados en la EOD 2017")






#GRAPH CANTIDAD DE PERSONAS EN TRÁNSITO POR TRANSPORTE

design_d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FACTOR, 
                       data = d)
options(survey.lonely.psu="remove")


varstime <- names(d)[325:468]
table1 <- lapply(varstime, function(x){
  form <- make.formula(c(x, "P5_3", "SEXO", "EDAD2"))
  d <- data.frame(x, svytable(formula = form, design = design_d))
  names(d) <- c("tiempo", "en_transito", "dia", "sexo", "edad2", "freq")
  return(d)
}) ; names(table1) <- varstime
table2 <- do.call(what = "rbind", args = table1)


table2$tiempo2 <- ymd_hms(paste(today(), (do.call("rbind", strsplit(x = as.character(table2$tiempo), split = "ttransp_medio_"))[,2]), sep = " "), tz = "Mexico/General")
table2$dia <- as.character(table2$dia)
table2$dia[table2$dia == "entre_semana"] <- "Entre semana"
table2$dia[table2$dia == "sabado"] <- "Sabado"









transportes <- c("Automovil ", "Caminar", 
                 "Colectivo_Micro ", "Taxi_App_internet", "Taxi_sitio_calle_otro", 
                 "Metrobus_o_Mexibus ", "Transporte_de_personal ", "Moto", "Bicicleta", "Metro", "Autobus", "Autobus_RTP_ o_ M1")

graph3_semana <- table2 %>% 
  filter(en_transito != "FALSE") %>% 
  filter(dia == "Entre semana") %>%
  filter(en_transito %in% transportes) %>%
  
  ggplot() +
  geom_bar(mapping = aes(x = tiempo2, y = freq), stat = "identity", width=250) +
  scale_x_datetime(date_breaks = "hours" , date_labels = "%H-%M") +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0, size = 5)) +
  theme(axis.text.y = element_text(size = 7)) +
  facet_wrap(facets = ~en_transito, scales = "free")  + 
  xlab(label = "Lapsos de 10 minutos") + 
  ylab(label = "Cantidad de personas en tránsito") +
  ggtitle(label = "Cantidad de personas en tránisto por lapsos de 10 minutos en la ZMVM que utilizan el medio ENTRE SEMANA",
          subtitle = "Segun reportado por los entrevistados en la EOD 2017")



graph3_sabado <- table2 %>% 
  filter(en_transito != "FALSE") %>% 
  filter(dia == "Sabado") %>%
  filter(en_transito %in% transportes) %>%
  
  ggplot() +
  geom_bar(mapping = aes(x = tiempo2, y = freq), stat = "identity", width=250) +
  scale_x_datetime(date_breaks = "hours" , date_labels = "%H-%M") +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0, size = 5)) +
  theme(axis.text.y = element_text(size = 7)) +
  facet_wrap(facets = ~en_transito, scales = "free")  + 
  xlab(label = "Lapsos de 10 minutos") + 
  ylab(label = "Cantidad de personas en tránsito") +
  ggtitle(label = "Cantidad de personas en tránisto por lapsos de 10 minutos en la ZMVM que utilizan el medio SABADOS",
          subtitle = "Segun reportado por los entrevistados en la EOD 2017")





#GRAPH TIEMPOS DE TRASLADO POR TRANSPORTE

d$transp_length2_min <- d$transp_length2/60
d$transp_length2_min_intervals <- cut(x = d$transp_length2_min, breaks = c(seq(from = 0, to = 120, by = 10), 180, 240, 300))

design_d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FACTOR, 
                      data = d)
options(survey.lonely.psu="remove")

table3 <- data.frame(svytable(formula = ~transp_length2_min+P5_14+P5_3, design = design_d) )
names(table3) <- c("mins", "transporte", "dia", "freq")

table3$mins_intervals <- cut(x = as.numeric(table3$mins), breaks = 20)



graph_tiempo_traslados <- table3 %>% 
  filter(dia == "entre_semana") %>%
  filter(transporte %in% transportes) %>%
  
  ggplot() +
  geom_bar(mapping = aes(x = mins_intervals, y = freq), stat = "identity", width = .7) +
  #scale_x_datetime(date_breaks = "hours" , date_labels = "%H-%M") +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=0, size = 6)) +
  theme(axis.text.y = element_text(size = 7)) + 
  facet_wrap(facets = ~transporte, scales = "free") + 
  xlab(label = "Tiempo de traslado (intervalos de minutos)") + 
  ylab(label = "Cantidad de viajes") +
  ggtitle(label = "Tiempos de traslados en la ZMVM por transporte ENTRE SEMANA",
          subtitle = "Segun reportado por los entrevistados en la EOD 2017")





path_graphs <- here::here("GRAPHS")
#ggsave(filename = "graph3_semana.jpeg", plot = graph3_semana, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph3_sabado.jpeg", plot = graph3_sabado, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph_tiempo_traslados.jpeg", plot = graph_tiempo_traslados, device = "jpeg", path = path_graphs,width = 12, height = 6)




#TRAYECTOS MAS FRECUENTES

catest <- read.dbf(file = "TCAT_ESTACIONES.dbf")

catest$TRAS <- as.numeric(as.character(catest$TRAS))
catest$TRAS <- factor(x = catest$TRAS, levels = 1:20, 
       labels = c("Automovil ", "Colectivo_Micro ", "Taxi_App_internet", "Taxi_sitio_calle_otro", "Metro", "Autobus_RTP_ o_ M1", "Bicicleta", "Autobus", "Moto", "Trolebus ", "Metrobus_o_Mexibus ", "Tren_ligero ", "Tren_suburbano ", "Caminar", "Mexicable", "Bicitaxi ", "Mototaxi ", "Transporte_escolar", "Transporte_de_personal ", "Otro "))
catest$TRAS <- droplevels(catest$TRAS)

catest$EST <- as.numeric(catest$EST)

catest$DESC <- str_to_title(catest$DESC)
catest$SISTEMA <- str_to_title(catest$SISTEMA)

names(catest) <- c("P5_14", "P5_17", "estacion", "linea", "sistema")
catest$linea <- as.character(catest$linea)

catest_ini <- catest
names(catest_ini) <- c("P5_14", "P5_17_1C", "estacion_ini", "linea_ini", "sistema_ini")
catest_fin <- catest
names(catest_fin) <- c("P5_14", "P5_17_2C", "estacion_fin", "linea_fin", "sistema_fin")


d$P5_17_1C <- as.numeric(d$P5_17_1C)
d$P5_17_2C <- as.numeric(d$P5_17_2C)

d <- merge(x = d, y = catest_ini, all.x = TRUE) ; dim(d)
d <- merge(x = d, y = catest_fin, all.x = TRUE) ; dim(d)

########################################################
table(d$sistema_fin, d$sistema_ini) #agruparon metrobus y mexibus en p5_14
########################################################


d$linea_misma <- d$linea_ini == d$linea_fin

design_d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FACTOR, 
                      data = d)
options(survey.lonely.psu="remove")


table4 <- data.frame(svytable(formula = ~sistema_ini+sistema_fin+estacion_ini+estacion_fin, design = design_d) )
table4$ini_fin <- paste(table4$estacion_ini, table4$estacion_fin, sep = " - ")

table5 <- lapply(levels(table4$sistema_ini), function(x){
  table4 %>% 
    filter(sistema_ini == x) %>%
    filter(sistema_fin == x) %>%
    arrange(desc(Freq)) %>% 
    head(., 15) %>% 
    mutate(rank = 1:15)
  })
table5 <- do.call("rbind", table5)

table5$ini_fin <- factor(x = table5$ini_fin, levels = table5$ini_fin, labels = table5$ini_fin)

graph_trayectos <- ggplot(table5) +
  geom_bar(mapping = aes(x = ini_fin, y = Freq), stat = "identity") +
  facet_wrap(facets = ~sistema_ini, scales = "free") +
  theme(axis.text.x = element_text(angle=30, hjust=.85, vjust=.85, size = 6.5), 
        axis.text.y = element_text(size = 7, 
                                   margin = margin(l = 15, r = 4, unit = "pt"))) +
  xlab(label = "Trayecto (inicio - fin)") + 
  ylab(label = "Frecuencia") +
  ggtitle(label = "Trayectos más frecuentes en la ZMVM por transporte (entre semanana y sábados)", subtitle = "Segun reportado por los entrevistados en la EOD 2017")
  






path_graphs <- here::here("GRAPHS")
#ggsave(filename = "graph_transito_pordia.jpeg", plot = graph_transito_pordia, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph3_transito_semana.jpeg", plot = graph3_semana, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph3_transito_sabado.jpeg", plot = graph3_sabado, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph_tiempo_traslados.jpeg", plot = graph_tiempo_traslados, device = "jpeg", path = path_graphs,width = 12, height = 6)
#ggsave(filename = "graph_trayectos.jpeg", plot = graph_trayectos, device = "jpeg", path = path_graphs,width = 12, height = 6)