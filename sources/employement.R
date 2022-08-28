#creamos employement agregado
employement<-employees%>%full_join(employers,by=c("municipio","year","quarter"))

employementfull<-employement%>%group_by(year,quarter)%>%
  summarise("employees"=sum(emp_t,na.rm=T),"employers"=sum(pat_t,na.rm=T))

#agregamos date variable
employementfull$month<-3*employementfull$quarter
employementfull$date<-as.Date(paste0(employementfull$year,"-",employementfull$month,"-01"))

#axis plot
ylim.prim <- c(12, 16)   
ylim.sec <- c(0.75, 0.84) 
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

#Plot trabajo en millions ()
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
          title = "Número de Empleadores y Empleados Afiliados
al Seguro Social Mexicano",
          subtitle = "Figura 1",
          caption = "Fuente: IMSS") +
  theme_minimal() +#lo pongo bonito
  theme(legend.position="bottom")
ggsave("employement.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps
  