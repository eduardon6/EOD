library(foreign)
library(survey)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)
library(openxlsx)


rm(list=ls())
dir()

files_names <- c("TVIAJE.DBF", "TTRANSPORTE.DBF")
names(files_names) <- files_names

d <- lapply(files_names, function(x){
  read.dbf(file = x, as.is = TRUE)
})




d$TVIAJE.DBF$P5_3[d$TVIAJE.DBF$P5_3 == "1"] <- "entre_semana"
d$TVIAJE.DBF$P5_3[d$TVIAJE.DBF$P5_3 == "2"] <- "sabado"

d$TVIAJE.DBF <- d$TVIAJE.DBF[d$TVIAJE.DBF$P5_9_1 != 99,]
d$TVIAJE.DBF <- d$TVIAJE.DBF[d$TVIAJE.DBF$P5_10_1 != 99,]



d$TVIAJE.DBF$hr_ini_1 <- paste(as.numeric(d$TVIAJE.DBF$P5_9_1), d$TVIAJE.DBF$P5_9_2, "00", sep = ":")
d$TVIAJE.DBF$hr_fin_1 <- paste(as.numeric(d$TVIAJE.DBF$P5_10_1), d$TVIAJE.DBF$P5_10_2, "00", sep = ":")

d$TVIAJE.DBF$hr_ini_viaje <- ymd_hms(paste(today(), d$TVIAJE.DBF$hr_ini_1, sep = " "), tz = "Mexico/General")
d$TVIAJE.DBF$hr_fin_viaje <- ymd_hms(paste(today(), d$TVIAJE.DBF$hr_fin_1, sep = " "), tz = "Mexico/General")

d$TVIAJE.DBF$interval <- interval(start = d$TVIAJE.DBF$hr_ini_viaje, 
                                  end = d$TVIAJE.DBF$hr_fin_viaje, tzone = "Mexico/General")
d$TVIAJE.DBF$length <- d$TVIAJE.DBF$hr_fin_viaje - d$TVIAJE.DBF$hr_ini_viaje

d$TVIAJE.DBF[1:5,c("hr_ini_viaje", "hr_fin_viaje", "interval", "length")]




hourcat <- lapply(0:23, function(x){
  lapply(seq(from = 0, to = 50, by = 10), function(y){
    paste(x, y, "00", sep = ":")
  })
})
hourcat <- unlist(hourcat)
hourcat2 <- ymd_hms(paste(today(), hourcat, sep = " "), tz = "Mexico/General")
names(hourcat2) <- hourcat


inbetween <- lapply(seq_along(d$TVIAJE.DBF$hr_ini_viaje), function(x){
  between(hourcat2, d$TVIAJE.DBF$hr_ini_viaje[x], d$TVIAJE.DBF$hr_fin_viaje[x])
})

inbetween <- data.frame(do.call(what = "rbind", inbetween))
names(inbetween) <- paste("tviaje", hourcat, sep = "_")

d$TVIAJE.DBF <- data.frame(d$TVIAJE.DBF, inbetween)
d$TVIAJE.DBF <- d$TVIAJE.DBF[,c(1:8, 14:16, 76:82, 83:length(names(d$TVIAJE.DBF)))]

head(d$TVIAJE.DBF[,c(21, 22, 25:168)])



d$TTRANSPORTE.DBF$P5_3[d$TTRANSPORTE.DBF$P5_3 == "1"] <- "entre_semana"
d$TTRANSPORTE.DBF$P5_3[d$TTRANSPORTE.DBF$P5_3 == "2"] <- "sabado"


d2 <- reduce(.x = d, .f = merge, by = c("ID_VIA", "N_VIA", "P5_3", "ESTRATO", "FACTOR", "UPM_DIS", "EST_DIS", "TLOC", "SEXO", "EDAD"))
d2 <- d2 %>% 
  arrange(ID_VIA, N_VIA, P5_16)
names(d2)


d2 <- d2[d2$P5_16_1_1 != 99,] ####

prob <- d2 %>% 
  group_by(ID_VIA, P5_16) %>% 
  summarise("count" = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 1)
d2 <- d2[!(d2$ID_VIA %in% prob$ID_VIA),] ####




########################################################
#d2 <- d2[1:1000, ]
#######################################################


d2$P5_16 <- as.numeric(d2$P5_16)
d2$P5_14 <- as.numeric(d2$P5_14)
d2$P5_14 <- factor(x = d2$P5_14, levels = 1:20, 
                   labels = c("Automovil ", "Colectivo_Micro ", "Taxi_App_internet", "Taxi_sitio_calle_otro", "Metro", "Autobus_RTP_ o_ M1", "Bicicleta", "Autobus", "Moto", "Trolebus ", "Metrobus_o_Mexibus ", "Tren_ligero ", "Tren_suburbano ", "Caminar", "Mexicable", "Bicitaxi ", "Mototaxi ", "Transporte_escolar", "Transporte_de_personal ", "Otro "))





d2$P5_16_1_1 <- as.numeric(d2$P5_16_1_1)
d2$P5_16_1_2 <- as.numeric(d2$P5_16_1_2)

d2$transp_length <- hms(paste(d2$P5_16_1_1, d2$P5_16_1_2, "0", sep = ":"))
d2$transp_length2 <- as.difftime(d2$transp_length)





d2 <- d2 %>% 
  arrange(ID_VIA, N_VIA, P5_16)

d2$hr_ini_transp <- d2$hr_ini_viaje
d2$hr_fin_transp <- d2$hr_ini_viaje + d2$transp_length2

d2$hr_ini_transp[d2$P5_16 != 1] <- NA
d2$hr_fin_transp[d2$P5_16 != 1] <- NA

for(i in which(d2$P5_16 > 1)){
  d2$hr_ini_transp[i] <- d2$hr_fin_transp[i - 1]
  d2$hr_fin_transp[i] <- d2$hr_ini_transp[i] + d2$transp_length2[i]
}

d2[c(35:38, 57:59),c("ID_VIA", "P5_16", "hr_ini_viaje", "hr_fin_viaje", "P5_3", "P5_14", "transp_length", "hr_ini_transp", "hr_fin_transp")]


inbetween2 <- lapply(seq_along(d2$hr_ini_viaje), function(x){
  between(hourcat2, d2$hr_ini_transp[x], d2$hr_fin_transp[x])
})
inbetween2 <- data.frame(do.call(what = "rbind", inbetween2))
names(inbetween2) <- paste("ttransp", hourcat, sep = "_")
d2 <- data.frame(d2, inbetween2)



d3 <- lapply(levels(d2$P5_14), function(x){
  d <- d2[as.character(d2$P5_14) == x,]
  
  index <- grep(pattern = "ttransp_", names(d))
  
  d[,index] <- lapply(index, function(y){
    d3 <- d
    d3[,y][d3[,y] == TRUE] <- x
    return(d3[,y])
  })
  
  return(d)
  
})
d3 <- do.call("rbind", d3)

indexd3 <- grep(pattern = "ttransp_", names(d3))
names(d3)[indexd3] <- paste("ttransp_medio_", names(hourcat2), sep = "")


d2 <- d2 %>% 
  arrange(ID_VIA, N_VIA, P5_16)
d3 <- d3 %>% 
  arrange(ID_VIA, N_VIA, P5_16)

d4 <- cbind(d2, d3[,181:324])
#d4 <- merge(x = d2, y = d3, by = names(d2[1:180]))



names(d4)[325:468] <- str_replace_all(string = names(d4)[325:468], pattern = ":", replacement = ".")
head(d4[,grep(pattern = "ID_VIA|P5_16$|P5_14|transp|hora", x = names(d4), value = TRUE)])
d4[35:38,c(1:12, 171, 179, 180, 177, 181:324, 325:468)]


#write.csv(x = d4, file = "transportes.csv")




