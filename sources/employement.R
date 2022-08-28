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
    name = "Empleados (en millones)",
    #segundo eje
    sec.axis = sec_axis(trans =~ (. - a)/b, name="Empleadores (en millones)")
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
  scale_fill_gradient2(low = "#FF6962", mid="#FDFD96", high = "#77DD77",midpoint = 20 ,limits=c(0,50))
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


#Tabla con proporcion de cada tamaño empleados
propemployees<-employees%>%filter(quarter==4)%>% #filtro solo cuarto trimestre
  group_by(year)%>%summarise( #agrupo por anio y calculo
  "1 Empleado"=sum(emp_size_1,na.rm = T)/sum(emp_t,na.rm=T),
  "2 a 5 Empleados"=sum(emp_size_2_5,na.rm = T)/sum(emp_t,na.rm=T),
  "6 a 50 Empleados"=sum(emp_size_6_50,na.rm = T)/sum(emp_t,na.rm=T),
  "51 a 250 Empleados"=sum(emp_size_51_250,na.rm = T)/sum(emp_t,na.rm=T),
  "251 a 500 Empleados"=sum(emp_size_251_500,na.rm = T)/sum(emp_t,na.rm=T),
  "501 a 1000 Empleados"=sum(emp_size_501_1000,na.rm = T)/sum(emp_t,na.rm=T),
  "Mas de 1000 Empleados"=sum(emp_size_1000,na.rm = T)/sum(emp_t,na.rm=T)
)
#paso a porcentaje
propemployees[,2:ncol(propemployees)]<-round(propemployees[,2:ncol(propemployees)] * 100,digits=1)
#guardo la tabla
write(print(xtable(propemployees,digits=c(0,0,2,2,2,2,2,2,2)
                   , caption="Porcentaje de Empleados en firmas por tamaño y año (cuarto trimestre).")),
      file="outputs/propemployees.tex")

#Tabla con proporcion de cada tamaño empleados 
propemployers<-employers%>%filter(quarter==4)%>% #filtro cuarto trimestre
  group_by(year)%>%summarise( #agrupo por anio y calculo
    "1 Empleado"=sum(pat_size_1,na.rm = T)/sum(pat_t,na.rm=T),
    "2 a 5 Empleados"=sum(pat_size_2_5,na.rm = T)/sum(pat_t,na.rm=T),
    "6 a 50 Empleados"=sum(pat_size_6_50,na.rm = T)/sum(pat_t,na.rm=T),
    "51 a 250 Empleados"=sum(pat_size_51_250,na.rm = T)/sum(pat_t,na.rm=T),
    "251 a 500 Empleados"=sum(pat_size_251_500,na.rm = T)/sum(pat_t,na.rm=T),
    "501 a 1000 Empleados"=sum(pat_size_501_1000,na.rm = T)/sum(pat_t,na.rm=T),
    "Mas de 1000 Empleados"=sum(pat_size_1000,na.rm = T)/sum(pat_t,na.rm=T)
  )
#paso a porcentaje
propemployers[,2:ncol(propemployers)]<-round(propemployers[,2:ncol(propemployers)] * 100,digits=1)
#guardo la tabla
write(print(xtable(propemployers,digits=c(0,0,2,2,2,2,2,2,2)
                   , caption="Porcentaje de Empleadores en firmas por tamaño y año (cuarto trimestre).")),
      file="outputs/propemployers.tex")

#Creo proporcion de empleo en firmas chicas
employement$emp_small<-(employement$emp_size_1+employement$emp_size_2_5+employement$emp_size_6_50)
employement$pat_small<-(employement$pat_size_1+employement$pat_size_2_5+employement$pat_size_6_50)

employementprop<-employement%>%group_by(year,quarter)%>%
  summarise("emp_small_prop"=sum(emp_small,na.rm=T)/sum(emp_t,na.rm=T),"pat_small_prop"=sum(pat_small,na.rm=T)/sum(pat_t,na.rm=T))

#agregamos date variable
employementprop$month<-3*employementprop$quarter
employementprop$date<-as.Date(paste0(employementprop$year,"-",employementprop$month,"-01"))



#Plot trabajo en Firmas Chicas proporcion
#axis plot
ylim.prim <- c(0.3059, 0.353)   
ylim.sec <- c(0.945, 0.96) 
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]


ggplot(employementprop%>%filter(emp_small_prop>0),aes(x=date))+
  geom_line(aes(y=emp_small_prop,color="Proporcion Empleados"),linetype = "dashed")+
  geom_line(aes(y = (a + pat_small_prop*b),color="Proporcion Empleadores"))+
  geom_vline(xintercept=as.Date("2003-01-01"), colour="black",linetype="dotted",size=1.3) +
  geom_text(aes(x=as.Date("2002-11-01"), label="2003", y=0.3175), colour="black", angle=90) +
  scale_y_continuous(
    # Primer Eje
    name = "Proporcion Empleados",
    #segundo eje
    sec.axis = sec_axis(trans =~ (. - a)/b, name="Proporcion Empleadores")
  ) +labs(x = "Fecha",   #cambio detalles
          color = "",
          title = "Evolucion de Proporcion Empleados y Empleadores Firmas Chicas",
          subtitle = "Firmas  mas afectadas por Seguro Popular",
          caption = "Fuente: IMSS y Bosch 2014") +
  theme_minimal() +#lo pongo bonito
  theme(legend.position="bottom")
ggsave("smallfirms.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps



rm(list=c('empcaract','employementfull','employement',"a","b",
          'ylim.prim',"ylim.sec","propemployees","propemployers",
          "employementprop"))
