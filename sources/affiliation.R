#Junto demas en other
affiliated<-affiliated%>%group_by(year)%>%
  mutate("other"=sum(ISSSTE,SEDENA,SEMAR,PEMEX,na.rm=T))

ggplot(affiliated,aes(x=year))+
  geom_line(aes(y=SeguroPopular,color="Seguro Popular"))+
  geom_line(aes(y=IMSS,color="IMSS"),linetype = "dashed")+
  geom_line(aes(y = other,color="Otros"),linetype = 3,size=1)+
  labs(x = "Fecha",   #cambio detalles
          color = "",
          title = "Número de Afiliados al Seguro Popular, IMSS y otros proveedores de salud con SP: 2000-2011",
          subtitle = "Figura 3",
          caption = "Fuente: IMSS",
       y="Afiliados a servicios de salud (en millones)") +
  theme_minimal() +#lo pongo bonito
  theme(legend.position="bottom")
ggsave("figura3.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps