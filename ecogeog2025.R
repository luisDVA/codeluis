## ---------------------------------------------------------------------
#| eval: true
#| message: false
# cargar paquetes
library(readr)         
library(sf)            
library(ggplot2)       
library(rnaturalearth) 
library(dplyr)
library(ggthemes)      
library(geodata)        
library(terra)         
library(tidyterra)
library(visreg)
library(scico)


## ---------------------------------------------------------------------
#| message: false
# importar datos directamente desde un repositorio de github 
ab_mex <- read_csv("https://github.com/luisDVA/SturniraMacroecology/raw/master/data/flwbiocl.csv")



## ---------------------------------------------------------------------
# en R base
plot(FL~elevation,data=ab_mex)
# con ggplot2
theme_set(theme_base())

ggplot(ab_mex)+
  geom_point(aes(x=elevation,y=FL),pch=1)



## ---------------------------------------------------------------------

ggplot(ab_mex)+
  geom_point(aes(x=elevation,y=FL,color=species))
ggplot(ab_mex)+
  geom_point(aes(x=elevation,y=FL,color=species))+
  facet_wrap(~species)
# qué tal con la temperatura mínima anual?
ggplot(ab_mex)+
  geom_point(aes(x=AnnualTmin,y=FL,fill=species,size=elevation),pch=21)



## ---------------------------------------------------------------------
ab_mex_sf <- st_as_sf(ab_mex,coords = c(X="longitude",Y="latitude"))
# mapa de algunos valores continuos
plot(ab_mex_sf["sex"])
plot(ab_mex_sf["elevation"])



## ---------------------------------------------------------------------
# estados de México, pedimos un objeto sf
divpol <- ne_states(country="Mexico",returnclass = "sf") 
# podemos graficar todos los atributos o solo el contorno
plot(st_geometry(divpol))

# 'disolver' los estados
divpol <- divpol %>% mutate(idnuevo="a") %>% summarise(.by=idnuevo)
plot(st_geometry(divpol))


## ---------------------------------------------------------------------
#| eval: true
#| error: true

try({
# especificando el objeto en cada 'geom' que estamos dibujando
ggplot()+
  geom_sf(data=divpol)+
  geom_sf(data=ab_mex_sf)

})


## ---------------------------------------------------------------------
ab_mex_sf <- st_as_sf(ab_mex,coords = c(X="longitude",Y="latitude"),crs=st_crs(divpol))


## ---------------------------------------------------------------------
ggplot()+
  geom_sf(data=divpol)+
  geom_sf(data=ab_mex_sf)+
  theme_map()


## ---------------------------------------------------------------------
# se puede acotar la descarga al 'bounding box' de un país
tminrast <- worldclim_country("Mexico", var= "tmin",res=2.5, path=tempdir())
tmaxrast <- worldclim_country("Mexico", var= "tmax",res=2.5, path=tempdir())
precrast <- worldclim_country("Mexico", var = "prec",
        res=2.5, path=tempdir())


## ---------------------------------------------------------------------
tminrast
tminrast[[1]]
tmaxrast
tmaxrast[[1]]
precrast
precrast[[2]]


## ---------------------------------------------------------------------
# el método mean ya está implementado para objetos SpatRaster con varias capas
tminpromedio <- mean(tminrast)
tminpromedio
tmaxpromedio <- mean(tmaxrast)
tmaxpromedio
# las sumas también
prectot <- sum(precrast)
# salió??
plot(tminpromedio)
plot(prectot)


## ---------------------------------------------------------------------
varstemp <- c(tminpromedio,tmaxpromedio,prectot)
names(varstemp) <- c("tmin","tmax","prec")
valspts <- extract(varstemp,ab_mex_sf)
ab_mex <- bind_cols(ab_mex,valspts)


## ---------------------------------------------------------------------
ggplot(ab_mex)+
  geom_point(aes(tmin,AnnualTmin/10))



## ---------------------------------------------------------------------
flmod <- lm(FL~tmin+tmax+prec,data = ab_mex)
summary(flmod)


## ---------------------------------------------------------------------
visreg(flmod,"tmax",type = "conditional")


## ---------------------------------------------------------------------
flpred <- predict(varstemp,flmod)
# predicción
plot(flpred)

# más bonito
ggplot()+
  geom_spatraster(data=flpred)+
  geom_sf(data=divpol,fill="transparent")+
  theme_map()+
  scale_fill_scico(palette = "lipari",
                   na.value="black",name="Largo del antebrazo")


