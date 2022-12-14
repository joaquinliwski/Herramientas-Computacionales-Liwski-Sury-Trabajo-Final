municipiosp<-municipio%>%left_join(benef_SP_2002_2009, by=c("cvemun")) #join con caracteristicas de municipio
#armamos date variables
municipiosp$month<-3*municipiosp$quarter
municipiosp$ind<-ifelse(municipiosp$ind>10,municipiosp$ind,NA)
municipiosp$date<-as.Date(paste0(municipiosp$year,"-",municipiosp$month,"-01"))
options(scipen=999)#para que no aparezca con scientific notation

#GIF SP
mapagif<- ggplot(municipiosp%>%filter(!is.na(date))) + 
  geom_sf(colour='black', aes(fill=ind), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "Cantidad de Adheridos",
       title = "Seguro Popular - M?xico",
       subtitle = "Individuos Adheridos Por Municipio\n{frame_time}",
       caption = "Fuente:IMSS") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+scale_fill_gradient2(low="white",high="black",limits=c(1,286144))+#revertimos el original pattern
transition_time(date) 
num_frames <- 29
animate(mapagif,nframes=num_frames,fps=2,duration=15,height = 732.8, width = 1061.04)
anim_save("outputs/SP.gif")

#elimino todo lo que aca se agrega
rm(list = c("num_frames","municipiosp"))

