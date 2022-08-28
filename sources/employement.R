#creamos employement agregado
employement<-employees%>%full_join(employers,by=c("municipio","year","quarter"))

employementfull<-employement%>%group_by(year,quarter)%>%
  summarise("employees"=sum(emp_t,na.rm=T),"employers"=sum(pat_t,na.rm=T))

#agregamos date variable
employementfull$month<-3*employementfull$quarter
employementfull$date<-as.Date(paste0(employementfull$year,"-",employementfull$month,"-01"))


#Plot trabajo en millions

#axis plot
ylim.prim <- c(12, 16)   
ylim.sec <- c(0.75, 0.84) 
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


ggplot(employementfull%>%filter(employers>0),aes(x=date))+
  geom_line(aes(y=employees/1000000,color="Empleados (en millones)"),linetype = "dashed")+
  geom_line(aes(y = (a + employers*b/1000000),color="Empleadores (en millones)"))+
  scale_y_continuous(
    # Primer Eje
    name = "Empleadores (en millones)",
    #segundo eje
    sec.axis = sec_axis(trans =~ (. - a)/b, name="Empleados (en millones)")
  ) +labs(x = "Fecha",   #cambio detalles
          color = "",
          title = "Número de Empleadores y Empleados Afiliados al Seguro Social Mexicano",
          subtitle = "Figura 1",
          caption = "Fuente: IMSS") +
  theme_minimal() +#lo pongo bonito
  theme(legend.position="bottom")
ggsave("employement.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps
  

#Empleo por lo general no varia tanto, mas estático.
#Junto datos, por municipio empleados y empleadores promedio para el periodo por municipio
empcaract<-employement%>%left_join(municipio)%>%left_join(caract_muni, by=c("cvemun"))%>%
  select(emp_t,pat_t,populationb,year,quarter,municipio)%>%group_by(municipio)%>%
  summarise("employees"=mean(emp_t/populationb,na.rm=T),"employers"=mean(pat_t/populationb,na.rm=T))%>%
  right_join(municipio) #le devolvemos la hermosa geometria
empcaract<-st_as_sf(empcaract) #SF once again

#Mapa de empleados
ggplot() + 
  geom_sf(data=empcaract,colour='black', aes(fill=100*employees), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "Porcentaje",
       title = "Promedio Empleados (IMSS) sobre Poblacion",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000 - IMSS") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low = "#FF6962", mid="#FDFD96", high = "#77DD77",midpoint = 25 ,limits=c(0,60))
ggsave("employeespob.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps
#Hay tres que tienen mas empleados que poblacion


ggplot() + 
  geom_sf(data=empcaract,colour='black', aes(fill=100*employers), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "Porcentaje",
       title = "Promedio Empleadores (IMSS) sobre Poblacion",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000 - IMSS") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low = "#FF6962", mid="#FDFD96", high = "#77DD77",midpoint = 1.5 ,limits=c(0,3.5))
ggsave("employerspob.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps
#28 porciento de empleadores algunos

rm(list=c('empcaract','employementfull','employement',"a","b",'ylim.prim',"ylim.sec"))
