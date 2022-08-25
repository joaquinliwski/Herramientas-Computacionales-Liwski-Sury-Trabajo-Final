setwd("C:/Users/Joaquin/Desktop/UdeSA/Maestría en Economía/Herramientas Computacionales Para Investigación/Herramientas-Computacionales-Liwski-Sury-Trabajo-Final")
#Cargo Paquetes
suppressMessages({
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load("tidyverse","readxl","haven","sf","rgdal","RecordLinkage")
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
municipio <- readOGR("Data, figuras y do originales/municipios_mx_feb2018/municipios_mx_feb2018.shp")
#plot(municipioshp)
#Names
municipio@data$NOMGEO<-str_to_title(iconv(municipio@data$NOMGEO, to = 'Latin1'))
#Paso a numeric la variable en base a la cual voy a unir.
municipio@data$CVEGEO<-as.numeric(municipio@data$CVEGEO)
#Agrego al poligono la data para unir a los demas
municipio@data<-municipio@data%>%left_join(merge_ss_eneu_final,by=c("CVEGEO"="municipality"))


