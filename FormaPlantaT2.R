### --------------------------------------------------------------###
### An?lisis de datos para informe final Proyecto Fondef D10i1149 ###
### --------------------------------------------------------------###

# cargar los paquetes a utilizar #
library(ggplot2) # ggplot2 para la generaci?n de gr?ficos avanzados
library(reshape2) # cambiar formato wide to long
library(plyr) # agregaci?n y resumen de datos
library(xlsx) # formato excel
library(agricolae) # paquete estad?stico aplicado a RRNN
library(car) # prueba LeveneTest

# Fijar el directorio de trabajo
setwd("C:/Users/manufore/Documents/Analisis Estadistico Macropropagaci?n")

# Leer datos desde el archivo principal propagacion_mod2.csv
datos <- read.csv("propagacion_mod4.csv", sep=";")
# Extrae el espacio entre palabras
datos$medicion <- gsub(" ", "", datos$medicion)
datos$setos <- as.factor(datos$setos)

#---------------------------------------------------------------------------------------#
# Ensayos de Formaci?n de plantas madres seguimiento (Referencia Temporada T2)
#---------------------------------------------------------------------------------------#

# Agrupaci?n de dos tratamientos de las mismas caracter?sticas
datos$id[datos$id=="E2T1"] <- "E1T2"

# Definici?n de ensayo Tipo de contenedor (Cruzas 2013)
datos$id2 <- ifelse(datos$setos=="2013" & datos$origen=="CC" & datos$contenedor=="Canalet?n" & datos$medicion=="abril2013","E5T2",
                    ifelse(datos$setos=="2013" & datos$origen=="CC" & datos$contenedor=="Bolsa 2,5 litros" & datos$medicion=="abril2013","E5T1",as.character(datos$id)))

datos$id2 <- as.factor(datos$id2)

# definici?n de filtro de selecci?n Productividad y Enraizamiento
sele <- datos$select1
# Ensayo N1 : Altura de corte #
Alturacorte <- subset(datos, (id2=="E1T1" | id2=="E1T2" | id2=="E1T3") & sele=="SELECCI?N" & !is.na(produce2) & produce1>=5 & produce2>=5 & enraiza1>=50 & enraiza2>=50)
# Ensayo N2 : Fertilizaci?n al sustrato #
Fertilizacion <- subset(datos, (id2=="E1T2" | id2=="E2T2") & sele=="SELECCI?N" & !is.na(produce2) & produce1>=5 & produce2>=5 & enraiza1>=50 & enraiza2>=50)
# Ensayo N3 : Tipo de contenedor #
Tipocontenedor <- subset(datos, (id2=="E5T1" | id2=="E5T2") & select2=="SELECCI?N" & !is.na(produce3) & produce2>=5 & produce3>=5 & enraiza2>=50 & enraiza3>=50)
# Ensayo N4 : Tipo de sustrato #
Tiposustrato <- subset(datos, (id2=="E1T2" | id2=="E4T1") & sele=="SELECCI?N" & !is.na(produce2) & produce1>=5 & produce2>=5 & enraiza1>=50 & enraiza2>=50)

# definici?n de filtro de selecci?n para an?lisis de sobrevivencia
# Ensayo N1 : Altura de corte #
AlturacorteV <- subset(datos, (id2=="E1T1" | id2=="E1T2" | id2=="E1T3"))
# Ensayo N2 : Fertilizaci?n al sustrato #
FertilizacionV <- subset(datos, (id2=="E1T2" | id2=="E2T2"))
# Ensayo N3 : Tipo de contenedor #
TipocontenedorV <- subset(datos, (id2=="E5T1" | id2=="E5T2"))
# Ensayo N4 : Tipo de sustrato #
TiposustratoV <- subset(datos, (id2=="E1T2" | id2=="E4T1"))

# Variables a utilizar (producci?n de miniestacas)
producevars <- c("id2","produce1","produce2")
enraizavars <- c("id2","enraiza1","enraiza2")
producevarsTC <- c("id2","produce2","produce3")
enraizavarsTC <- c("id2","enraiza2","enraiza3")
svivevars <- c("id2","svive1","svive2")
svivevarsTC <- c("id2","svive2","svive3")

# cambio de formato de las tablas wide to long
FormaplantaP <- list (melt(Alturacorte[producevars],id.vars = c("id2")),
                      melt(Fertilizacion[producevars],id.vars = c("id2")),
                      melt(Tipocontenedor[producevarsTC],id.vars = c("id2")),
                      melt(Tiposustrato[producevars],id.vars = c("id2")))

FormaplantaE <- list (melt(Alturacorte[enraizavars],id.vars = c("id2")),
                      melt(Fertilizacion[enraizavars],id.vars = c("id2")),
                      melt(Tipocontenedor[enraizavarsTC],id.vars = c("id2")),
                      melt(Tiposustrato[enraizavars],id.vars = c("id2")))

FormaplantaV <- list (melt(AlturacorteV[svivevars],id.vars = c("id2")),
                      melt(FertilizacionV[svivevars],id.vars = c("id2")),
                      melt(TipocontenedorV[svivevarsTC],id.vars = c("id2")),
                      melt(TiposustratoV[svivevars],id.vars = c("id2")))

# T?tulo de los dataframes de cada ensayo
attr(FormaplantaP[[1]], "titulo") <- "Altura de corte"
attr(FormaplantaP[[2]], "titulo") <- "Fertilizaci?n al sustrato"
attr(FormaplantaP[[3]], "titulo") <- "Tipo de contenedor"
attr(FormaplantaP[[4]], "titulo") <- "Tipo de sustrato"

# Reetiquetado para Temporadas de producci?n-enraiza-svive
for (i in 1:4) {
  FormaplantaP[[i]]$variable <- factor(FormaplantaP[[i]]$variable,
                                       levels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("produce2","produce3") } else { c("produce1","produce2","produce3","produce4")},
                                       labels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("A?o 1","A?o 2") } else { c("A?o 1","A?o 2","A?o 3","A?o 4")})[, drop=TRUE]
  
  FormaplantaE[[i]]$variable <- factor(FormaplantaE[[i]]$variable,
                                       levels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("enraiza2","enraiza3") } else { c("enraiza1","enraiza2","enraiza3","enraiza4")},
                                       labels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("A?o 1","A?o 2") } else { c("A?o 1","A?o 2","A?o 3","A?o 4") })[, drop=TRUE]
  
  FormaplantaV[[i]]$variable <- factor(FormaplantaV[[i]]$variable,
                                       levels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("svive2","svive3") } else { c("svive1","svive2","svive3","svive4")},
                                       labels = if  (attr(FormaplantaP[[i]], "titulo")=="Tipo de contenedor") { c("A?o 1","A?o 2") } else { c("A?o 1","A?o 2","A?o 3","A?o 4") })[, drop=TRUE]
  
  }

# Definici?n de etiquetas para tratamientos
etiquetaAC <- c("1cm","4cm","8cm","6 g/L Basacote 9M","Bolsa 2,5 L","Sust. continuo","Turba30%+Perlita70%")
etiquetaFS <- c("1cm","3 g/L Basacote 9M","8cm","6 g/L Basacote 9M","Bolsa 2,5 L","Sust. continuo","Turba30%+Perlita70%")
etiquetaTC <- c("1cm","4cm","8cm","6 g/L Basacote 9M","Bolsa 2,5 L","Sust. continuo","Turba30%+Perlita70%")
etiquetaTS <- c("1cm","T15%+A15%+P70%","8cm","6 g/L Basacote 9M","Bolsa 2,5 L","Sust. continuo","T30%+P70%")
etiquetas <- list(etiquetaAC,etiquetaFS,etiquetaTC,etiquetaTS)

# Reetiquetado para Tratamientos
for (i in 1:4) {
  FormaplantaP[[i]]$id2 <- factor(FormaplantaP[[i]]$id2,
                                       levels = c("E1T1","E1T2","E1T3","E2T2","E5T1","E5T2","E4T1"),
                                       labels = etiquetas[[i]])[, drop=TRUE]

  FormaplantaE[[i]]$id2 <- factor(FormaplantaE[[i]]$id2,
                                       levels = c("E1T1","E1T2","E1T3","E2T2","E5T1","E5T2","E4T1"),
                                       labels = etiquetas[[i]])[, drop=TRUE]
  FormaplantaV[[i]]$id2 <- factor(FormaplantaV[[i]]$id2,
                                       levels = c("E1T1","E1T2","E1T3","E2T2","E5T1","E5T2","E4T1"),
                                       labels = etiquetas[[i]])[, drop=TRUE]
                                    }

# Generaci?n de graficos de barra por cada tratamiento
for (i in 1:4) {
  
  grafPA <- ddply(subset(FormaplantaP[[i]], !is.na(value)), .(variable), summarize,
                  promedio = round(mean(value), 0))
                    
  grafEA <- ddply(subset(FormaplantaE[[i]], !is.na(value)), .(variable), summarize,
                  promedio = round(mean(value), 0))
  
  grafP <- ddply(subset(FormaplantaP[[i]], !is.na(value)), .(id2,variable), summarize,
                 cuenta = length(value),
                 min = min(value),
                 promedio = round(mean(value), 0),
                 suma = sum(value),
                 max = max(value),
                 sd = round(sd(value), 1),
                 cv = round(sd(value)/mean(value)*100,0),
                 se = round(sd / sqrt(cuenta),1),
                 em = round(1.96*se,1),
                 emP= round(em/promedio*100,0),
                 li = round(promedio - 1.96*se,0),
                 ls = round(promedio + 1.96*se,0),
                 Q1 = round(quantile(value, probs = 0.2),0),
                 Q2 = round(quantile(value, probs = 0.4),0),
                 Q3 = round(quantile(value, probs = 0.6),0),
                 Q4 = round(quantile(value, probs = 0.8),0),
                 Q5 = round(quantile(value, probs = 1),0),
                 mQ1 = round(mean(value[value<Q1]),0),
                 mQ2 = round(mean(value[value>=Q1 & value<Q2]),0),
                 mQ3 = round(mean(value[value>=Q2 & value<Q3]),0),
                 mQ4 = round(mean(value[value>=Q3 & value<Q4]),0),
                 mQ5 = round(mean(value[value>=Q4 & value<=Q5]),0))
  
  grafE <- ddply(subset(FormaplantaE[[i]], !is.na(value)), .(id2,variable), summarize,
                 cuenta = length(value),
                 min = min(value),
                 promedio = round(mean(value), 0),
                 max = max(value),
                 sd = round(sd(value), 1),
                 cv = round(sd(value)/mean(value)*100,0),
                 se = round(sd / sqrt(cuenta),1),
                 em = round(1.96*se,1),
                 emP= round(em/promedio*100,0),
                 li = round(promedio - 1.96*se,0),
                 ls = round(promedio + 1.96*se,0),
                 Q1 = round(quantile(value, probs = 0.2),0),
                 Q2 = round(quantile(value, probs = 0.4),0),
                 Q3 = round(quantile(value, probs = 0.6),0),
                 Q4 = round(quantile(value, probs = 0.8),0),
                 Q5 = round(quantile(value, probs = 1),0),
                 mQ1 = round(mean(value[value<Q1]),0),
                 mQ2 = round(mean(value[value>=Q1 & value<Q2]),0),
                 mQ3 = round(mean(value[value>=Q2 & value<Q3]),0),
                 mQ4 = round(mean(value[value>=Q3 & value<Q4]),0),
                 mQ5 = round(mean(value[value>=Q4 & value<=Q5]),0))
  
  grafT <- ddply(subset(FormaplantaV[[i]]), .(id2,variable), summarize,
                 cuenta = length(id2))
  
  grafV <- ddply(subset(FormaplantaV[[i]]), .(id2,variable,value), summarize,
                 cuenta = length(value))
  
  for (j in 1:nrow(grafV)) { grafV$P[j]<- round(grafV$cuenta[j]/(grafT[grafT$id2==grafV$id2[j] & grafT$variable==grafV$variable[j],]$cuenta)*100,0) }
  
  # Edici?n final para tablas en Excel
  TablaProductividad <- data.frame(cbind(grafP[c("id2","variable","cuenta")],Ninicial=grafT$cuenta,svive=grafV[grafV$value=="viva",]$P,grafP[c("min","promedio","max","cv","emP","li","ls")]))
  TablaEnraizamiento <- data.frame(cbind(grafE[c("id2","variable","cuenta")],Ninicial=grafT$cuenta,svive=grafV[grafV$value=="viva",]$P,grafE[c("min","promedio","max","cv","emP","li","ls")]))
  TablaFinalP <- cbind(TablaProductividad[TablaProductividad$variable=="A?o 1",],TablaProductividad[TablaProductividad$variable=="A?o 2",][-c(1,3,4,5)])
  TablaFinalE <- cbind(TablaEnraizamiento[TablaEnraizamiento$variable=="A?o 1",],TablaEnraizamiento[TablaEnraizamiento$variable=="A?o 2",][-c(1,3,4,5)])
  
  # Tabla resumenes para cada ensayo
  resumenes <- list(grafP,grafE,grafT,grafV,TablaFinalP,TablaFinalE)
  nombreres <- c("grafP","grafE","grafT","grafV","TablaFinalP","TablaFinalE")
  for (k in 1:6) { if (k==1) {write.xlsx(resumenes[[k]], paste("./Resumen/Seguimiento/",attr(FormaplantaP[[i]], "titulo"),"T2.xlsx",sep = ""), sheetName = nombreres[k], row.names = FALSE)
  } else {write.xlsx(resumenes[[k]], paste("./Resumen/Seguimiento/",attr(FormaplantaP[[i]], "titulo"),"T2.xlsx",sep = ""), sheetName = nombreres[k], append = TRUE, row.names = FALSE)}}
  
  Colores <- c("#339900","#FFCC00","#D55E00","#336699")
  
  if (nrow(FormaplantaP[[i]])>0) {
    
    
    g1 <- ggplot(data=grafP, aes(x=id2, y= promedio, fill=variable))+
      geom_bar(stat = "identity", position = position_dodge(width=0.92))+
      geom_errorbar(aes(ymin=promedio-se,ymax=promedio+se),position=position_dodge(0.9), width=0.2)+
      geom_text(aes(y=9,label=paste("P=",promedio,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=7,label=paste("E=",grafE$promedio,"%",sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=3,label=paste("n=",cuenta,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=5,label=paste("S=",grafV[grafV$value=="viva",]$P,"%",sep="")), position=position_dodge(0.9))+
      scale_x_discrete(breaks=unique(FormaplantaP[[i]]$id2))+
      ylab("N? de Miniestacas por seto")+
      scale_y_continuous(breaks=seq(0,35,by=5), limits=c(0,35), expand = c(0,0))+
      xlab(attr(FormaplantaP[[i]], "titulo"))+
      scale_fill_manual(  values= Colores ,
                          name="Edad",
                          labels=unique(FormaplantaP[[i]]$variable))+
      theme(axis.line.x = element_line(colour = "grey"), axis.line.y = element_line(colour = "grey"))
    
    print(g1)
    
    
    g2 <- ggplot(data=grafP, aes(x=variable, y= promedio, fill=id2))+
      geom_bar(stat = "identity", position = position_dodge(width=0.92))+
      geom_errorbar(aes(ymin=promedio-se,ymax=promedio+se),position=position_dodge(0.9), width=0.2)+
      geom_text(aes(y=9,label=paste("P=",promedio,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=7,label=paste("E=",grafE$promedio,"%",sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=3,label=paste("n=",cuenta,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=5,label=paste("S=",grafV[grafV$value=="viva",]$P,"%",sep="")), position=position_dodge(0.9))+
      scale_x_discrete(breaks=unique(FormaplantaP[[i]]$variable))+
      ylab("N? de Miniestacas por seto")+
      scale_y_continuous(breaks=seq(0,35,by=5), limits=c(0,35), expand = c(0,0))+
      xlab("Edad de seto")+
      scale_fill_manual(  values= Colores ,
                          name=attr(FormaplantaP[[i]], "titulo"),
                          labels=unique(FormaplantaP[[i]]$id2))+
      theme(axis.line.x = element_line(colour = "grey"), axis.line.y = element_line(colour = "grey"))
    
    print(g2)
    
    g3 <- ggplot(data=grafE, aes(x=id2, y= promedio, fill=variable))+
      geom_bar(stat = "identity", position = position_dodge(width=0.92))+
      geom_errorbar(aes(ymin=promedio-se,ymax=promedio+se),position=position_dodge(0.9), width=0.2)+
      geom_text(aes(y=20,label=paste("P=",grafP$promedio,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=25,label=paste("E=",promedio,"%",sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=10,label=paste("n=",cuenta,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=15,label=paste("S=",grafV[grafV$value=="viva",]$P,"%",sep="")), position=position_dodge(0.9))+
      scale_x_discrete(breaks=unique(FormaplantaE[[i]]$id2))+
      ylab("Porcentaje de enraizamiento (%)")+
      scale_y_continuous(breaks=seq(0,100,by=10), limits=c(0,100), expand = c(0,0))+
      xlab(attr(FormaplantaP[[i]], "titulo"))+
      scale_fill_manual(  values= Colores ,
                          name="Edad",
                          labels=unique(FormaplantaE[[i]]$variable))+
      theme(axis.line.x = element_line(colour = "grey"), axis.line.y = element_line(colour = "grey"))
    
    print(g3)
    
    
    g4 <- ggplot(data=grafE, aes(x=variable, y= promedio, fill=id2))+
      geom_bar(stat = "identity", position = position_dodge(width=0.92))+
      geom_errorbar(aes(ymin=promedio-se,ymax=promedio+se),position=position_dodge(0.9), width=0.2)+
      geom_text(aes(y=20,label=paste("P=",grafP$promedio,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=25,label=paste("E=",promedio,"%",sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=10,label=paste("n=",cuenta,sep="")), position=position_dodge(0.9))+
      geom_text(aes(y=15,label=paste("S=",grafV[grafV$value=="viva",]$P,"%",sep="")), position=position_dodge(0.9))+
      scale_x_discrete(breaks=unique(FormaplantaE[[i]]$variable))+
      ylab("Porcentaje de enraizamiento (%)")+
      scale_y_continuous(breaks=seq(0,100,by=10), limits=c(0,100), expand = c(0,0))+
      xlab("Edad de seto")+
      scale_fill_manual(  values= Colores ,
                          name=attr(FormaplantaP[[i]], "titulo"),
                          labels=unique(FormaplantaE[[i]]$id2))+
      theme(axis.line.x = element_line(colour = "grey"), axis.line.y = element_line(colour = "grey"))
    
    print(g4)
    
    g5 <- ggplot(data=subset(FormaplantaP[[i]], !is.na(value)), aes(x=value))+
      geom_histogram(bins = 20, col="black",fill="#339900", position = "identity", alpha=0.8)+
      ylab("N? de setos") +
      xlab("N? de miniestacas")+
      #geom_label(data = grafP, mapping = aes(x=50, y=10, label=paste("media=",promedio,sep=""), Fill="white", hjust=1, vjust=1))+
      geom_vline(aes(xintercept=promedio), data=grafP,
                 color="blue", linetype="dashed", size=1)+
      facet_grid(variable~id2)+
      
      theme(strip.text.x = element_text(size = 11),
            strip.text.y = element_text(size = 11))+
      theme(legend.position="none")
    
    print(g5)
    
    g6 <- ggplot(data=subset(FormaplantaP[[i]], !is.na(value)), aes(x=value))+
      geom_histogram(bins = 20, col="black",fill="#339900", position = "identity", alpha=0.8)+
      ylab("N? de setos") +
      xlab("N? de miniestacas")+
      #geom_label(data = grafP, mapping = aes(x=50, y=10, label=paste("media=",promedio,sep=""), Fill="white", hjust=1, vjust=1))+
      geom_vline(aes(xintercept=promedio), data=grafP,
                 color="blue", linetype="dashed", size=1)+
      facet_grid(id2~variable)+
      
      theme(strip.text.x = element_text(size = 11),
            strip.text.y = element_text(size = 11))+
      theme(legend.position="none")
    
    print(g6)
    
   g7 <- ggplot(data=subset(FormaplantaE[[i]], !is.na(value)), aes(x=value))+
      geom_histogram(bins = 20, col="black",fill="#339900", position = "identity", alpha=0.8)+
      ylab("N? de setos") +
      xlab("Porcentaje de Enraizamiento (%)")+
      #geom_label(data = grafE, mapping = aes(x=70, y=10, label=paste("media=",promedio,sep=""), Fill="white", hjust=1, vjust=1))+
      geom_vline(aes(xintercept=promedio), data=grafE,
                 color="blue", linetype="dashed", size=1)+
      facet_grid(variable~id2)+
      
      theme(strip.text.x = element_text(size = 11),
            strip.text.y = element_text(size = 11))+
      theme(legend.position="none")
    
    print(g7)
    
    g8 <- ggplot(data=subset(FormaplantaE[[i]], !is.na(value)), aes(x=value))+
      geom_histogram(bins = 20, col="black",fill="#339900", position = "identity", alpha=0.8)+
      ylab("N? de setos") +
      xlab("Porcentaje de Enraizamiento (%)")+
      #geom_label(data = grafE, mapping = aes(x=70, y=10, label=paste("media=",promedio,sep=""), Fill="white", hjust=1, vjust=1))+
      geom_vline(aes(xintercept=promedio), data=grafE,
                 color="blue", linetype="dashed", size=1)+
      facet_grid(id2~variable)+
      
      theme(strip.text.x = element_text(size = 11),
            strip.text.y = element_text(size = 11))+
      theme(legend.position="none")
    
    print(g8)
    
    
    g9 <- ggplot(data=subset(FormaplantaP[[i]], !is.na(value)), aes(x=id2, y=value))+
      geom_boxplot(fill="green")+
      ylab("N? de miniestacas")+
      xlab("Tratamiento")+
      theme(strip.text.x = element_text(size = 11))+
      geom_hline(aes(yintercept=promedio), data=grafPA, color="blue", linetype="twodash", size=1)+
      stat_summary(fun.y=mean, colour="#CC0033", geom="point", 
                   shape=16, size=3)+
      facet_grid(.~variable)
    
    print(g9)
    
    g10 <- ggplot(data=subset(FormaplantaE[[i]], !is.na(value)), aes(x=id2, y=value))+
      geom_boxplot(fill="green")+
      ylab("Enraizamiento de miniestacas (%)")+
      xlab("Tratamiento")+
      theme(strip.text.x = element_text(size = 11))+
      geom_hline(aes(yintercept=promedio), data=grafEA, color="blue", linetype="twodash", size=1)+
      stat_summary(fun.y=mean, colour="#CC0033", geom="point", 
                   shape=16, size=3)+
      facet_grid(.~variable)
    
    print(g10)
    
    
    anova1 <- aov(formula = value ~ id2*variable, data = FormaplantaP[[i]])
    anovaR <- summary(anova1)
    write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ ",attr(FormaplantaP[[i]], "titulo")," ANOVAP.xlsx",sep = ""), sheetName = paste("ANOVA"), append = TRUE, row.names = TRUE)
    
    anova1 <- aov(formula = value ~ id2*variable, data = FormaplantaE[[i]])
    anovaR <- summary(anova1)
    write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ ",attr(FormaplantaP[[i]], "titulo")," ANOVAE.xlsx",sep = ""), sheetName = paste("ANOVA"), append = TRUE, row.names = TRUE)
    
    
  } else {print("Ensayo Sin datos")}
  
}

#--------------------------------------------------------------------------#
# Comparaci?n de medias Entre tratamientos en temporada respectiva
#--------------------------------------------------------------------------#
for (i in 1:4) {
  
  temporadas <- unique(FormaplantaP[[i]]$variable)
  write.xlsx(temporadas, paste("./Resumen/Seguimiento/ComparacionP/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = "temporadas", row.names = FALSE)
  for (j in 1:length(temporadas)) {
    
    Grupo <- subset(FormaplantaP[[i]], variable==temporadas[j])
    attr(Grupo, "titulo") <- paste(as.character(attr(FormaplantaP[[i]], "titulo")),as.character(temporadas[j]))
    Niveles <- unique(Grupo$id2)
    LeveneT <- leveneTest(value ~ id2, data=Grupo)[[3]][[1]]
    
    pShapiro <- c()
    for (k in 1:length(Niveles)){
      
      pShapiro[k] <- shapiro.test(Grupo[Grupo$id2==Niveles[k],]$value)[[1]][[1]]
      
    }
    
    Supuestos <- data.frame(c("Levene",paste("Shapiro",Niveles)),c(LeveneT,pShapiro))
    colnames(Supuestos) <- c("Prueba","P-valor")
    write.xlsx(Supuestos, paste("./Resumen/Seguimiento/ComparacionP/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("Supuestos",as.character(temporadas[j])), append = TRUE, row.names = FALSE)
    
    if (length(Niveles)==2) {
      
      ttest <- t.test(value~id2,data=Grupo)
      pttest <- data.frame(c("t.test"),c(ttest[[3]]))
      colnames(pttest) <- c("Prueba","P-valor")
      write.xlsx(pttest, paste("./Resumen/Seguimiento/ComparacionP/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("t.test",as.character(temporadas[j])), append = TRUE, row.names = FALSE)
    
      } else {
      
      anova1 <- aov(formula = value ~ id2, data = Grupo)
      anovaR <- summary(anova1)
      write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ComparacionP/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("ANOVA",as.character(temporadas[j])), append = TRUE, row.names = TRUE)
      
      if (anovaR[[1]]$`Pr(>F)`[1]<0.05) {
        
        HSDTest <- TukeyHSD(anova1)
        HSDTest2 <- HSD.test(anova1,"id2")
        write.xlsx(HSDTest2$groups, paste("./Resumen/Seguimiento/ComparacionP/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("TukeyHSD",as.character(temporadas[j])), append = TRUE, row.names = TRUE)  
      } else { Nodif <- print(paste("No hay diferencias significativas entre Grupos de",attr(Grupo, "titulo"))) }
      
      
    }
    
  }
    
}


#--------------------------------------------------------------------------------#
# Comparaci?n Productividad de medias Entre temporadas en tratamiento respectivo #
#--------------------------------------------------------------------------------#

for (i in 1:4) {
  
  tratamientos <- unique(FormaplantaP[[i]]$id2)
  write.xlsx(tratamientos, paste("./Resumen/Seguimiento/ComparacionP/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = "tratamientos", row.names = FALSE)
  
  for (j in 1:length(tratamientos)) {
    
    Grupo <- subset(FormaplantaP[[i]], id2==tratamientos[j])
    attr(Grupo, "titulo") <- paste(as.character(attr(FormaplantaP[[i]], "titulo")),as.character(tratamientos[j]))
    Niveles <- unique(Grupo$variable)
    LeveneT <- leveneTest(value ~ variable, data=Grupo)[[3]][[1]]
    
    pShapiro <- c()
    for (k in 1:length(Niveles)){
      
      pShapiro[k] <- shapiro.test(Grupo[Grupo$variable==Niveles[k],]$value)[[1]][[1]]
      
    }
    
    Supuestos <- data.frame(c("Levene",paste("Shapiro",Niveles)),c(LeveneT,pShapiro))
    colnames(Supuestos) <- c("Prueba","P-valor")
    write.xlsx(Supuestos, paste("./Resumen/Seguimiento/ComparacionP/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("Supuestos",as.character(tratamientos[j])), append = TRUE, row.names = FALSE)
    
    if (length(Niveles)==2) {
      
      ttest <- t.test(value~variable,data=Grupo)
      pttest <- data.frame(c("t.test"),c(ttest[[3]]))
      colnames(pttest) <- c("Prueba","P-valor")
      write.xlsx(pttest, paste("./Resumen/Seguimiento/ComparacionP/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("t.test",as.character(tratamientos[j])), append = TRUE, row.names = FALSE)
      
    } else {
      
      anova1 <- aov(formula = value ~ variable, data = Grupo)
      anovaR <- summary(anova1)
      write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ComparacionP/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("ANOVA",as.character(tratamientos[j])), append = TRUE, row.names = TRUE)
      
      if (anovaR[[1]]$`Pr(>F)`[1]<0.05) {
        
        HSDTest <- TukeyHSD(anova1)
        write.xlsx(HSDTest[[1]], paste("./Resumen/Seguimiento/ComparacionP/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("TukeyHSD",as.character(tratamientos[j])), append = TRUE, row.names = TRUE)  
      } else { Nodif <- print(paste("No hay diferencias significativas entre Grupos de",attr(Grupo, "titulo"))) }
      
      
    }
    
  }
  
}

#---------------------------------------------------------------------------------#
# Comparaci?n Enraizamiento de medias Entre tratamientos en temporada respectiva  #
#---------------------------------------------------------------------------------#

for (i in 1:4) {
  
  temporadas <- unique(FormaplantaE[[i]]$variable)
  write.xlsx(temporadas, paste("./Resumen/Seguimiento/ComparacionE/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = "temporadas", row.names = FALSE)
  for (j in 1:length(temporadas)) {
    
    Grupo <- subset(FormaplantaE[[i]], variable==temporadas[j])
    attr(Grupo, "titulo") <- paste(as.character(attr(FormaplantaP[[i]], "titulo")),as.character(temporadas[j]))
    Niveles <- unique(Grupo$id2)
    LeveneT <- leveneTest(value ~ id2, data=Grupo)[[3]][[1]]
    
    pShapiro <- c()
    for (k in 1:length(Niveles)){
      
      pShapiro[k] <- shapiro.test(Grupo[Grupo$id2==Niveles[k],]$value)[[1]][[1]]
      
    }
    
    Supuestos <- data.frame(c("Levene",paste("Shapiro",Niveles)),c(LeveneT,pShapiro))
    colnames(Supuestos) <- c("Prueba","P-valor")
    write.xlsx(Supuestos, paste("./Resumen/Seguimiento/ComparacionE/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("Supuestos",as.character(temporadas[j])), append = TRUE, row.names = FALSE)
    
    if (length(Niveles)==2) {
      
      ttest <- t.test(value~id2,data=Grupo)
      pttest <- data.frame(c("t.test"),c(ttest[[3]]))
      colnames(pttest) <- c("Prueba","P-valor")
      write.xlsx(pttest, paste("./Resumen/Seguimiento/ComparacionE/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("t.test",as.character(temporadas[j])), append = TRUE, row.names = FALSE)
      
    } else {
      
      anova1 <- aov(formula = value ~ id2, data = Grupo)
      anovaR <- summary(anova1)
      write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ComparacionE/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("ANOVA",as.character(temporadas[j])), append = TRUE, row.names = TRUE)
      
      if (anovaR[[1]]$`Pr(>F)`[1]<0.05) {
        
        HSDTest <- TukeyHSD(anova1)
        HSDTest2 <- HSD.test(anova1,"id2")
        write.xlsx(HSDTest2$groups, paste("./Resumen/Seguimiento/ComparacionE/ComparaTratamientos ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("TukeyHSD",as.character(temporadas[j])), append = TRUE, row.names = TRUE)  
      } else { Nodif <- print(paste("No hay diferencias significativas entre Grupos de",attr(Grupo, "titulo"))) }
      
      
    }
    
  }
  
}

#--------------------------------------------------------------------------------#
# Comparaci?n Enraizamiento de medias Entre temporadas en tratamiento respectivo #
#--------------------------------------------------------------------------------#

for (i in 1:4) {
  
  tratamientos <- unique(FormaplantaE[[i]]$id2)
  write.xlsx(tratamientos, paste("./Resumen/Seguimiento/ComparacionE/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = "tratamientos", row.names = FALSE)
  
  for (j in 1:length(tratamientos)) {
    
    Grupo <- subset(FormaplantaE[[i]], id2==tratamientos[j])
    attr(Grupo, "titulo") <- paste(as.character(attr(FormaplantaP[[i]], "titulo")),as.character(tratamientos[j]))
    Niveles <- unique(Grupo$variable)
    LeveneT <- leveneTest(value ~ variable, data=Grupo)[[3]][[1]]
    
    pShapiro <- c()
    for (k in 1:length(Niveles)){
      
      pShapiro[k] <- shapiro.test(Grupo[Grupo$variable==Niveles[k],]$value)[[1]][[1]]
      
    }
    
    Supuestos <- data.frame(c("Levene",paste("Shapiro",Niveles)),c(LeveneT,pShapiro))
    colnames(Supuestos) <- c("Prueba","P-valor")
    write.xlsx(Supuestos, paste("./Resumen/Seguimiento/ComparacionE/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("Supuestos",as.character(tratamientos[j])), append = TRUE, row.names = FALSE)
    
    if (length(Niveles)==2) {
      
      ttest <- t.test(value~variable,data=Grupo)
      pttest <- data.frame(c("t.test"),c(ttest[[3]]))
      colnames(pttest) <- c("Prueba","P-valor")
      write.xlsx(pttest, paste("./Resumen/Seguimiento/ComparacionE/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("t.test",as.character(tratamientos[j])), append = TRUE, row.names = FALSE)
      
    } else {
      
      anova1 <- aov(formula = value ~ variable, data = Grupo)
      anovaR <- summary(anova1)
      write.xlsx(anovaR[[1]], paste("./Resumen/Seguimiento/ComparacionE/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("ANOVA",as.character(tratamientos[j])), append = TRUE, row.names = TRUE)
      
      if (anovaR[[1]]$`Pr(>F)`[1]<0.05) {
        
        HSDTest <- TukeyHSD(anova1)
        write.xlsx(HSDTest[[1]], paste("./Resumen/Seguimiento/ComparacionE/ComparaTemporadas ",attr(FormaplantaP[[i]], "titulo")," T2.xlsx",sep = ""), sheetName = paste("TukeyHSD",as.character(tratamientos[j])), append = TRUE, row.names = TRUE)  
      } else { Nodif <- print(paste("No hay diferencias significativas entre Grupos de",attr(Grupo, "titulo"))) }
      
      
    }
    
  }
  
}


