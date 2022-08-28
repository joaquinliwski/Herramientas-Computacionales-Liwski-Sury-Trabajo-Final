municipiocaract<-municipio%>%left_join(caract_muni, by=c("cvemun")) #join con caracteristicas de municipio

#PLOT URBANOS VS RURALES
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=as.factor(urban),color=as.factor(urban)), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Municipios Urbanos y Rurales",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
        )+
  scale_fill_manual(labels=c("Rural","Urbano","Sin Datos"),
                    values=c("#BDE7BD", "#FF6962","#EBEBE3")#ponemos colores de cornuda (colores pastel <3)
                    )
ggsave("Urbanos y Rurales.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps


#PLOT Share Insured
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=insured), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Fraccion de Poblacion Asegurada",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low = "#FF6962", mid="#FDFD96", high = "#77DD77",midpoint = 0.4 ,limits=c(0,0.8))
ggsave("insured.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps

#PLOT Poblacion
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=populationb), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Poblacion",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low="#add8e6", high="#6777B8")#revertimos el original pattern
ggsave("populationb.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps

#PLOT INFORMALIDAD
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=inf), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Informalidad Promedio",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Bosch 2014") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(high = "#FF6962", mid="#FDFD96", low = "#77DD77",
                       midpoint = 0.55 ,limits=c(0.1,1)) 

ggsave("inf.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps


#Histograma informalidad 2000 
setEPS() 
postscript(file="outputs/histinf.eps",width =  7.87402, height = 5.90551)
hist(x = caract_muni$inf, prob = TRUE, main = "Informalidad mexicana por municipio en el 2000", 
     xlab = "Informalidad en %", ylab = "Densidad",
     col = "grey95", 
     breaks = 20)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(x = caract_muni$inf, prob = TRUE, main = "Informalidad mexicana por municipio en el 2000", 
     xlab = "Informalidad en %", ylab = "Densidad",
     add = TRUE, 
     col = "grey95", 
     breaks = 20)
lines(density(caract_muni$inf), lwd = 2, col = 'black')
dev.off()


#Histograma insured 2000
setEPS() 
postscript(file="outputs/histinsured.eps",width = 7.87402, height = 5.90551)
hist(x = caract_muni$insured, prob = TRUE, main = "Población mexicana asegurada por municipio en el 2000", 
     xlab = "Asegurados en %", ylab = "Densidad",
     col = "grey95", 
     breaks = 20)
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(x = caract_muni$insured, prob = TRUE, main = "Poblacion mexicana asegurada por municipio en el2000", 
     xlab = "Asegurados en %", ylab = "Densidad",
     col = "#grey95", 
     breaks = 20, 
     add = TRUE)
lines(density(caract_muni$insured), lwd = 2, col = 'black')
dev.off()

#PLOT Desempleo
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=unm), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Tasa de Desempleo",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low="#77dd77", high="#ff6961")#revertimos el original pattern
ggsave("unemployement.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps

#PLOT Escolaridad
ggplot(municipiocaract) + 
  geom_sf(colour='black', aes(fill=yrschl), size = 0.000005)+ #tipo de figura y aes
  labs(x = "Longitud", y = "Latitud",   #cambio detalles
       fill = "",
       title = "Años Promedio de Escolariddad",
       subtitle = "por Municipio, Mexico",
       caption = "Fuente: Censo General de Población y Vivienda 2000") +
  theme_minimal()+ #lo pongo bonito
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(.8,.8),
        legend.background = element_blank()
  )+
  scale_fill_gradient2(low="#A1F791", mid= "#F8F1AE", high="red", midpoint=6.5, limits=c(1,14))#revertimos el original pattern
ggsave("schooling.eps", plot = last_plot(), 
       path = "outputs", 
       width = 200, height = 135, units = "mm") #lo guardo como eps

#borramos todo lo aca creado
rm(list= "municipiocaract")
