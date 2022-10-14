#Librerías necesarias
library(rio)
library(ggplot2)
library(lsr)
library(Rmisc)
library(stargazer)
library(jtools)
library(DescTools)
library(haven)
library(descr)
library(survey)
library(sf)
library(dplyr)

#Importar datos de Perú, de Perú 2006-2021 y de Ronda 2021

library(rio)
arg = import("ARG_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

lapop21 = import("lapop21.RData")
lapop18 = import("https://raw.github.com/lapop-central/materials_edu/main/lapop18.RData")
lapop18 = subset(lapop18, pais<=35)

#Recodificar una variable
table(arg$ing4)

library(car)
arg$ing4r = car::recode(arg$ing4, "1:4=0; 5:7=1")
table(arg$ing4r)
prop.table(table(arg$ing4r))*100

lapop18$ing4r = car::recode(lapop18$ing4, "1:4=0; 5:7=100")
table(lapop18$ing4r)

##Calcular una variable
table(arg$b2)
arg$b2r = ((arg$b2-1)/6)*100
table(arg$b2r)

lapop18$b2r = ((lapop18$b2-1)/6)*100

table(arg$b2r)
table(lapop18$b2r)

#Tipo de variable
arg$ing4r = as.factor(arg$ing4r)
levels(arg$ing4r) <- c("No", "Sí")

apoyo.arg = as.data.frame(round(prop.table(table(arg$ing4r))*100, 1))

lapop18$ing4r = as.factor(lapop18$ing4r)
levels(lapop18$ing4r) <- c("No", "Sí")

apoyo.al = as.data.frame(round(prop.table(table(lapop18$ing4r))*100, 1))

#Graficar
library(ggplot2)
ggplot(apoyo.arg, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")
#  labs(x="Apoyo a la democracia", y="Porcentaje", 
#       caption="Barómetro de las Américas por LAPOP, 2021")+
#  ylim(0,60)


library(ggplot2)
ggplot(apoyo.al, aes(x="", y=Freq, fill=Var1))+
  geom_bar(stat = "identity")
#  labs(x="Apoyo a la democracia", y="Porcentaje", 
#       caption="Barómetro de las Américas por LAPOP, 2021")+
#  ylim(0,80)+
#  geom_text(aes(label=paste(Freq, "%", sep="")), color="white", 
#            position=position_stack(vjust=0.5), size=3)+
#  coord_polar("y")

#Otra variable: los votos son contados correctamente

table(arg$countfair1)
arg$countfair1r = as.factor(arg$countfair1)
levels(arg$countfair1r) <- c("Siempre", "Algunas veces", "Nunca")
table(arg$countfair1r)
voto.arg = as.data.frame(round(prop.table(table(arg$countfair1r)), 3)*100)

ggplot(voto.arg, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")

ggplot(data=voto.arg, aes(fill=Var1, x=Freq, y=""))+
  geom_bar(stat="identity", width=0.3)

#table(lapop21$countfair1)
#lapop21$countfair1r = as.factor(lapop21$countfair1)
#levels(lapop21$countfair1r) <- c("Siempre", "Algunas veces", "Nunca")
#table(lapop21$countfair1r)

#voto.al = as.data.frame(round(prop.table(table(lapop21$countfair1r)), 3)*100)
#ggplot(voto.al, aes(x=Var1, y=Freq))+
#  geom_bar(stat = "identity")

table(lapop18$idio2)
lapop18$idio2r = as.factor(lapop18$idio2)
levels(lapop18$idio2r) <- c("Mejor", "Igual", "Peor")
table(lapop18$idio2r)

econ.al = as.data.frame(round(prop.table(table(lapop18$idio2r)), 3)*100)
ggplot(econ.al, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")



#Cruce de dos variables: los votos son contados correctamente por país
table(lapop18$pais, lapop18$idio2r) 

lapop18$pais2 = as.factor(lapop18$pais)
levels(lapop18$pais2) <- c("México", "Guatemala", "El Salvador", "Honduras",
                           "Nicaragua","Costa Rica", "Panamá", "Colombia", 
                           "Ecuador", "Bolivia", "Perú", "Paraguay", 
                           "Chile", "Uruguay", "Brasil", "Argentina", 
                           "Rep. Dom.", "Jamaica")


table(lapop18$pais2, lapop18$idio2r)
round(prop.table(table(lapop18$pais2, lapop18$idio2r), 1)*100, 1)

econ.al = as.data.frame(round(prop.table(table(lapop18$pais2, lapop18$idio2r), 1)*100, 1))

ggplot(data=econ.al, aes(x=Freq, y=Var1, fill=Var2))+
  geom_col(position="fill")

# Intervalos de confianza: tolerancia a los golpes ejecutivos

table(arg$jc15a)
arg$jc15ar <- car::recode(arg$jc15a, "1=100; 2=0")
table(arg$jc15ar)
summary(arg$jc15ar)


lapop18$jc15ar <- car::recode(lapop18$jc15a, "1=100; 2=0")
table(lapop18$jc15ar)
summary(lapop18$jc15ar)

library(lsr)
ciMean(as.numeric(lapop18$jc15ar), na.rm=T)

library(Rmisc)
golpe <- group.CI(jc15ar~pais2, lapop18)
golpe

ggplot(golpe, aes(x=reorder(pais2, -jc15ar.mean), y=jc15ar.mean))+
  geom_bar(width=0.5, fill="powder blue", colour="#69b3a2", stat="identity")+
  geom_errorbar(aes(ymin=jc15ar.lower, ymax=jc15ar.upper), width=0.4, 
                color="darkcyan", cex=0.4)+
  geom_text(aes(label=paste(round(jc15ar.mean, 1), "%")), hjust=-0.8, size=2.5)+
  xlab("País") + ylab("Tolerancia a golpes ejecutivos (%)")+
  ylim(0, 80)+
  coord_flip()

#Victimización por el crimen

table(arg$vic1ext)
arg$crimen <- car::recode(arg$vic1ext, "1=100; 2=0")
mean(arg$crimen, na.rm=T)

table(arg$q1tb)
arg$genero = car::recode(arg$q1tb, "1=2; 2=1; 3=1")
arg$genero = as.factor(arg$genero)
levels(arg$genero) = c("Mujer", "Hombre")
table(arg$genero)

mean(arg$crimen[arg$genero=="Hombre"], na.rm=T)
mean(arg$crimen[arg$genero=="Mujer"], na.rm=T)

library(Rmisc)
crxgen <- group.CI(crimen~genero, arg)

library(ggplot2)
ggplot(crxgen, aes(x=genero, y=crimen.mean))+
  geom_bar(width=0.5, fill="darkcyan", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=crimen.lower, ymax=crimen.upper), width=0.2)+
  geom_text(aes(label=paste(round(crimen.mean, 1), "%")), vjust=-4, size=3)+
  xlab("Género") + ylab("Victimización por crimen (%)")+
  ylim(0, 40)

table(arg$q2) #Transformar a grupos de edad
table(arg$edr) #Convertir a factor y etiquetar niveles educativos
#Replicar con datos lapop21

#Apoyo a la democracia por país
library(Rmisc)
lapop18$ing4r = car::recode(lapop18$ing4, "1:4=0; 5:7=100")
df <- summarySE(data=lapop18, measurevar="ing4r", groupvar="pais2", na.rm=T)
df

ggplot(df, aes(x=reorder(pais2, -ing4r), y=ing4r))+
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=ing4r-ci, ymax=ing4r+ci), width=0.2)+
  geom_text(aes(label=paste(round(ing4r, 1), "%")), vjust=-1.5, size=2)+
  xlab("País")+
  ylab("Apoyo a la democracia")

#Correlaciones
vdem = import("https://raw.github.com/lapop-central/materials_edu/main/vdem.xlsx")

df = cbind(df, vdem$vdem2019)
colnames(df)[7] = "vdem2019"

ggplot(df, aes(x=ing4r, y=vdem2019))+
  geom_point()+
  geom_smooth(method=lm, se=F)+ #agregar línea de tendencia
  geom_text(data=df, aes(label=pais2), cex=2.5, check_overlap = T)+ 
  #Para etiquetar los puntos, darles un tamaño, ubicación y prevenir que se       sobrepongan
  labs(x="Apoyo a la democracia", y="Índice de Democracia Electoral V-Dem ")+
  #para etiquetar los ejes
  theme_light()+
  xlim(40, 100)+
  ylim(0.2, 1)

cor.test(x = df$ing4r, y = df$vdem2019)

#Modelamiento

#Modelos de regresión lineal

lapop18$ejec = ((lapop18$b21a-1)/6)*100
summary(lapop18$ejec)

modelo1 = lm(psar ~ eff1r + ejec + tolr + it1r + b32r, data=lapop18)
summary(modelo1)

library(stargazer)
stargazer(modelo1, align=T, type = 'text')

library(jtools)
plot_summs(modelo1)

library(DescTools)
library(haven)
lapop18$mujer = as_factor(lapop18$mujer)
LeveneTest(lapop18$psar, lapop18$mujer)
t.test(psar ~ mujer, data = lapop18, var.equal=T)

modelo2 <- lm(psar ~ mujer, data=lapop18)
summary(modelo2)

lapop18$mexico = ifelse(lapop18$pais==1, 1, 0)
t.test(psar ~ mexico, data = lapop18, var.equal=F)
modelo3 <- lm(psar ~ mexico, data=lapop18)
summary(modelo3)

modelo4 <- lm(psar ~ factor(pais), data=lapop18)
summary(modelo4)

modelo5 <- lm(psar ~ eff1r + factor(pais), data=lapop18)
summary(modelo5)

modelo6 <- lm(psar ~ eff1r + ejec + tolr + it1r + b32r + factor(pais), data=lapop18)
summary(modelo6)

modelo7 <- lm(psar ~ eff1r + ejec + tolr + it1r + b32r + factor(pais)
              + factor(edr) + factor(quintall) + factor(urban) + factor(mujer)
              + factor(edad), data=lapop18)
summary(modelo7)

plot_summs(modelo7, coefs=c("Eficacia externa"="eff1r", "Confianza en el ejecutivo"= "ejec",
                            "Tolerancia política"="tolr", "Confianza en la comunidad"="it1r",
                            "Confianza en el gobierno local"="b32r"))

export_summs(modelo7, modelo6, modelo1, to.file = "docx", file.name = "modelo.docx")

#Modelos de regresión logística
lapop18$fb_user = ifelse(lapop18$smedia1==1 & lapop18$smedia2<=4, 1, 0)
lapop18$tw_user = ifelse(lapop18$smedia4==1 & lapop18$smedia5<=4, 1, 0)
lapop18$wa_user = ifelse(lapop18$smedia7==1 & lapop18$smedia8<=4, 1, 0)

lapop18$user = ifelse(lapop18$fb_user==1 | lapop18$wa_user==1 | lapop18$tw_user ==1, 1, 0)
table(lapop18$user)

#Recodificando entre 0 y 1
lapop18$riqueza = (lapop18$quintall - 1)/4
lapop18$educ = (lapop18$ed)/18
lapop18$edad = (lapop18$q2 - 16)/83

modelo.log1 <- glm(user ~ riqueza + educ + edad + mujer + urban + factor(pais), family = binomial, data=lapop18)
summary(modelo.log1)
summ(modelo.log1)
plot_summs(modelo.log1, coefs=c("Nivel de riqueza"="riqueza", "Años de educación"="educ", 
                            "Edad"="edad", "Mujer"="mujerFemale", "Área urbana"="urban"))

#Mapas
tabla = as.data.frame(compmeans(lapop18$jc15ar, lapop18$pais2, 
                                lapop18$weight1500, plot=FALSE))

colnames(tabla) = c("media_golpe", "n_golpe", "sd_golpe")
tabla$pais = row.names(tabla)
tabla = tabla[-19, ]

tabla$OBJECTID = NA
tabla = within(tabla, {
  OBJECTID[pais=="Argentina"] <- 1
  # OBJECTID[pais=="Barbados"]<- 2
  # OBJECTID[pais=="Bahamas"]<- 3
  # OBJECTID[pais=="Belice"]<-4
  OBJECTID[pais=="Bolivia"]<-5
  OBJECTID[pais=="Brasil"]<-6
  #OBJECTID[pais=="Canadá"]<-7
  OBJECTID[pais=="Chile"]<-8
  OBJECTID[pais=="Colombia"]<-9
  OBJECTID[pais=="Costa Rica"]<-10
  #OBJECTID[pais=="Dominica"]<-11
  OBJECTID[pais=="Rep. Dom."]<-12
  OBJECTID[pais=="Ecuador"]<-13
  OBJECTID[pais=="El Salvador"]<-14
  # OBJECTID[pais=="Granada"]<-15
  OBJECTID[pais=="Guatemala"]<-16
  #OBJECTID[pais=="Guyana"]<-17
  #OBJECTID[pais=="Haití"]<-18
  OBJECTID[pais=="Honduras"]<-19
  OBJECTID[pais=="Jamaica"]<-20
  OBJECTID[pais=="México"]<-21
  #OBJECTID[pais=="Surinam"]<-22
  OBJECTID[pais=="Nicaragua"]<-23
  OBJECTID[pais=="Paraguay"]<-24
  OBJECTID[pais=="Perú"]<-25
  OBJECTID[pais=="Panamá"]<-26
  #OBJECTID[pais=="San Cristobal y Nieves"]<-27
  #OBJECTID[pais=="Santa Lucía"]<-28
  #OBJECTID[pais=="Trinidad y Tobago"]<-29
  OBJECTID[pais=="Uruguay"]<-30
  #OBJECTID[pais=="San Vicente y las Granadinas"]<-31
  #OBJECTID[pais=="Venezuela"]<-32
  #OBJECTID[pais=="Estados Unidos"]<-33
})
tabla <- tabla[order(tabla$OBJECTID),]

library(sf)
al = st_read("América.shp")
al = al[-c(1, 2, 4, 5, 6, 7, 8, 11, 13, 16, 17, 21, 22, 23, 24, 25, 27, 28, 31, 32, 34, 39, 40, 41, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53), ]

al$OBJECTID = NA
al = within(al, {
  OBJECTID[PAÍS=="Argentina"] <- 1
  # OBJECTID[PAÍS=="Barbados"]<- 2
  # OBJECTID[PAÍS=="Bahamas"]<- 3
  # OBJECTID[PAÍS=="Belice"]<-4
  OBJECTID[PAÍS=="Bolivia"]<-5
  OBJECTID[PAÍS=="Brasil"]<-6
  OBJECTID[PAÍS=="Canadá"]<-7
  OBJECTID[PAÍS=="Chile"]<-8
  OBJECTID[PAÍS=="Colombia"]<-9
  OBJECTID[PAÍS=="Costa Rica"]<-10
  # OBJECTID[PAÍS=="Dominica"]<-11
  OBJECTID[PAÍS=="República Dominicana"]<-12
  OBJECTID[PAÍS=="Ecuador"]<-13
  OBJECTID[PAÍS=="El Salvador"]<-14
  # OBJECTID[PAÍS=="Granada"]<-15
  OBJECTID[PAÍS=="Guatemala"]<-16
  #OBJECTID[PAÍS=="Guyana"]<-17
  #OBJECTID[PAÍS=="Haití"]<-18
  OBJECTID[PAÍS=="Honduras"]<-19
  #OBJECTID[PAÍS=="Jamaica"]<-20
  OBJECTID[PAÍS=="México"]<-21
  #OBJECTID[PAÍS=="Surinam"]<-22
  OBJECTID[PAÍS=="Nicaragua"]<-23
  OBJECTID[PAÍS=="Paraguay"]<-24
  OBJECTID[PAÍS=="Perú"]<-25
  OBJECTID[PAÍS=="Panamá"]<-26
  #OBJECTID[PAÍS=="San Cristobal y Nieves"]<-27
  #OBJECTID[PAÍS=="Santa Lucía"]<-28
  #OBJECTID[PAÍS=="Trinidad y Tobago"]<-29
  OBJECTID[PAÍS=="Uruguay"]<-30
  #OBJECTID[PAÍS=="San Vicente y las Granadinas"]<-31
  #OBJECTID[PAÍS=="Venezuela"]<-32
  #OBJECTID[PAÍS=="Estados Unidos"]<-33
})
al = al[order(al$OBJECTID),]

library(dplyr)
al_datos <- al %>%
  left_join(tabla)

ggplot(data=al_datos) +
  geom_sf(fill="skyblue3", color="black")

ggplot(al_datos) +
  geom_sf(aes(fill = media_golpe))+
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_sf_text(aes(label=pais), size=2, check_overlap = T)+
  labs(title = "Tolerancia a golpes ejecutivos en América Latina",
       caption = "Fuente: Barómetro de las Américas 2021",
       x="Longitud",
       fill = "%")+
  theme_bw()


#Efecto de diseño o factor de expansión

#Apoyo a la democracia por país
library(descr)
compmeans(arg$ing4r, arg$pais, arg$weight1500, plot=F)

#Calculando efecto de diseño
sapply(lapop18, haven::zap_labels)
library(survey)
diseno18 = svydesign(ids = ~upm, strata = ~strata, weights = ~weight1500, nest=TRUE, data=lapop18)

#Percepción del estado de la economía por país
votoxpais = as.data.frame(round(prop.table(svytable(~pais+idio2r, design=diseno18), 1)*100, 0))
votoxpais

ggplot(data=votoxpais, aes(fill=idio2r, x=Freq, y=pais))+
  geom_bar(stat="identity", width=0.3)


#Apoyo a la democracia
df2 = as.data.frame(compmeans(lapop18$ing4r, lapop18$pais2, lapop18$weight1500, plot=F))

varnames = c("media", "n", "sd")
colnames(df2) <- varnames
df2$pais = row.names(df2)
df2$err.st = df2$sd/sqrt(df2$n)
df2$ci = df2$err.st*1.96
df2 = df2[c(-19),]

ggplot(df2, aes(x=reorder(pais, -media), y=media))+
  geom_bar(width=0.5, fill="purple", colour="black", stat="identity")+
  geom_errorbar(aes(ymin=media-ci, ymax=media+ci), width=0.2)+
  geom_text(aes(label=paste(round(media, 0), "%")), vjust=-2.5, size=2)+
  xlab("")+
  ylab("Apoyo a la democracia segun países")
