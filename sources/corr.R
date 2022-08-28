municipiosp<-municipio%>%left_join(benef_SP_2002_2009, by=c("cvemun")) #join con caracteristicas de municipio
#armamos date variables
municipiosp$month<-3*municipiosp$quarter
municipiosp$date<-as.Date(paste0(municipiosp$year,"-",municipiosp$month,"-01"))
options(scipen=999)#para que no aparezca con scientific notation

#date variable NA si ind<10 o NA (con replace porque ifelse rompe el formato)
municipiosp$date<-replace(municipiosp$date,which(municipiosp$ind<10),NA)
municipiosp$date<-replace(municipiosp$date,which(is.na(municipiosp$ind)),NA)

#primer fecha donde ind es mayor a diez como entrada al programa de un municipio.
entrydate<-municipiosp%>%group_by(cvemun)%>%slice(which.min(date))
entrydate<-entrydate%>%left_join(caract_muni, by=c("cvemun"))

ggplot(entrydate,aes(x=date,y=log(populationb)))+
  geom_point(alpha=.3,color="#b8d8be")+
  geom_smooth(method = lm, se = FALSE,color="black")+ #para ver direccion
  annotate("text", x = as.Date("2008-06-01"), y = 13,
           label = expression(beta*"=-40.14"), parse = TRUE)+
  labs(x = "Fecha de Entrada", y = "Poblacion",   #cambio detalles
       fill = "",
       title = "Fecha de Adopcion SP vs Poblacion",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000 - IMSS") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )
ggsave("Correlacion Cami.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps
rm(list = c("municipiosp","entrydate"))
