municipiopoverty<-municipio%>%left_join(Population_Poverty, by=c("cvemun")) #join con caracteristicas de municipio


#PLOT poverty alimentaria
ggplot(municipiopoverty) + 
  geom_sf(colour='black', aes(fill=pobreza_alim2000), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "Porcentaje",
       title = "Porcentaje de Poblacion en Pobreza Alimentaria (Indigencia)",
       subtitle = "por Municipio, Mexico 2000",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(high = "#FF6962", mid="#FDFD96", low = "#77DD77",midpoint = 40 ,limits=c(0,100))
ggsave("povertyalim.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps

#PLOT poverty INGRESOS
ggplot(municipiopoverty) + 
  geom_sf(colour='black', aes(fill=pobreza_pat2000), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "Porcentaje",
       title = "Porcentaje de Poblacion en Pobreza por Ingresos",
       subtitle = "por Municipio, Mexico 2000",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(high = "#FF6962", mid="#FDFD96", low = "#77DD77",midpoint = 40 ,limits=c(0,100))
ggsave("povertypat.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps


#borramos todo lo aca creado
rm(list= "municipiopoverty")