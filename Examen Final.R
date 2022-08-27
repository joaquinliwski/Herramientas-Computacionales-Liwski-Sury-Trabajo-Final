setwd("C:/Users/Joaquin/Desktop/UdeSA/Maestría en Economía/Herramientas Computacionales Para Investigación/Herramientas-Computacionales-Liwski-Sury-Trabajo-Final")
#Cargo Paquetes
suppressMessages({
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load("tidyverse","readxl","haven","sf","rgdal","RecordLinkage",
                 "rmapshaper","geojsonio")
})
rm(list=ls())

#cargo los dta
inputfolder<-"Data, figuras y do originales/Data-Original/"
dtas<-list.files(path= inputfolder, recursive = TRUE, include.dirs = TRUE)
for(file in dtas) {
  # nombre al cual le asigno cada dta
  obj_name <- gsub(".dta", "", file)
  #open file
  dta_file <- read_dta(paste0(inputfolder,file))
  # Assign
  assign(obj_name, dta_file)
}
#Borro lo que este de mas
rm(dta_file,dtas,file,obj_name)

#Limpio nombres de merge
merge_ss_eneu_final$descripcion_municipio[which(merge_ss_eneu_final$descripcion_municipio=="")]<-merge_ss_eneu_final$name_ss[which(merge_ss_eneu_final$descripcion_municipio=="")]
#merge_ss_eneu_final$descripcion_municipio<-str_to_title(iconv(merge_ss_eneu_final$descripcion_municipio, to = 'Latin1'))
merge_ss_eneu_final$descripcion_municipio<-str_to_title(merge_ss_eneu_final$descripcion_municipio)


#Inporto SHP
#municipioshp <- read_sf("Data, figuras y do originales/municipios_mx_feb2018/municipios_mx_feb2018.shp") #faster
municipio <- st_read("Data, figuras y do originales/municipios_mx_feb2018/municipios_mx_feb2018.shp", options = "ENCODING=WINDOWS-1252")
municipio <- ms_simplify(municipio) #para que no sea tan pesado
municipio$CVEGEO<-as.numeric(municipio$CVEGEO)
municipio<-municipio%>%left_join(merge_ss_eneu_final, by=c("cvemun"="municipality"))


municipiocaract<-municipio%>%left_join(caract_muni, by=c("cvemun"))

ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=as.factor(urban),color=as.factor(urban)))
  

#Histograma informalidad 2000 
hist(x = caract_muni$inf, prob = TRUE, main = "Informalidad mexicana por municipio en el 2000", 
     xlab = "Informalidad en %", ylab = "Densidad",
     col = "#AFEEEE", 
     breaks = 20)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(x = caract_muni$inf, prob = TRUE, main = "Informalidad mexicana por municipio en el 2000", 
     xlab = "Informalidad en %", ylab = "Densidad",
     add = TRUE, 
     col = "#AFEEEE", 
     breaks = 20)
lines(density(caract_muni$inf), lwd = 2, col = '#6A5ACD')

#Histograma insured 2000
hist(x = caract_muni$insured, prob = TRUE, main = "Población mexicana asegurada por municipio en el 2000", 
     xlab = "Asegurados en %", ylab = "Densidad",
     col = "#FF7F50", 
     breaks = 20)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(x = caract_muni$insured, prob = TRUE, main = "Poblacion mexicana asegurada por municipio en el2000", 
     xlab = "Asegurados en %", ylab = "Densidad",
     col = "#FF7F50", 
     breaks = 20, 
     add = TRUE)
lines(density(caract_muni$insured), lwd = 2, col = '#8B1A1A')






