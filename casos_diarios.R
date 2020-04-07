# Descarga la base actualizada de covid19
# paquete necesario (si no esta, instalarlo primero por única vez)
library(utils)

# bajar el data set (se actualiza periódicamente) y guardarlo como datos_covid
datos_covid <-
  read.csv(
    "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
    na.strings = "", fileEncoding = "UTF-8-BOM")
#variables cargadas
names(datos_covid)

# una tabla de países
table(datos_covid$countriesAndTerritories)

# me quedo solo con Argentina
datos_covid_argentina<-
  subset(
    datos_covid,
    datos_covid$countriesAndTerritories=="Argentina")


# como está ordenada desde el dato más reciente, genero el número de día
# de 32 a 1
datos_covid_argentina$num_dia<-32:1

datos_covid_argentina<-
  datos_covid_argentina[order(datos_covid_argentina$num_dia),]

library(ggplot2)
# grafico los casos nuevos por día
ggplot(datos_covid_argentina,aes(num_dia,cases))+geom_point()+
  geom_smooth(se=FALSE)

library(mblm)

estimador_theil_sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

# función lineal robusta
ggplot(datos_covid_argentina,aes(num_dia,cases))+
  geom_point()+ geom_smooth(se=FALSE, method = "estimador_theil_sen")


ggplot(datos_covid_argentina,aes(num_dia,cases))+
  geom_point()+geom_smooth(se=FALSE, method = "glm")


#cambio respecto del día anterior
tasa<-vector()
for (i in 2:32) {
  tasa[i]=
    datos_covid_argentina$cases[
      datos_covid_argentina$num_dia==i]/datos_covid_argentina$cases[
        datos_covid_argentina$num_dia==i-1]
}

# los que se dividieron por cero van a NA
tasa[is.infinite(tasa)]<-NA
tasa
# pego el vector tasa a la base
datos_covid_argentina<-
  data.frame(
    datos_covid_argentina,tasa)

# revisar, no están bien las tasas,
# además son muy volátiles, ver lags, tres, cuatro

ggplot(datos_covid_argentina,aes(num_dia, tasa))+
  geom_point()+
  geom_smooth()

# lag = 3 (se divide por el promedio de los tres días anteriores)
tasa_3<-vector()
for (i in 4:32) {
  tasa_3[i]=
    3*datos_covid_argentina$cases[
      datos_covid_argentina$num_dia==i]/(datos_covid_argentina$cases[
        datos_covid_argentina$num_dia==i-1]+
          datos_covid_argentina$cases[
            datos_covid_argentina$num_dia==i-2]+
          datos_covid_argentina$cases[
            datos_covid_argentina$num_dia==i-3])
}

datos_covid_argentina<-data.frame(datos_covid_argentina, tasa_3)
ggplot(datos_covid_argentina,aes(num_dia, tasa_3))+
  geom_point()+
  geom_smooth()

ggplot(datos_covid_argentina,aes(num_dia, tasa_3))+
  geom_point()+
  geom_smooth(method = "estimador_theil_sen")

ggplot(datos_covid_argentina,aes(num_dia,tasa_3))+
  geom_point()+
  geom_smooth(se=FALSE, method = "glm")

