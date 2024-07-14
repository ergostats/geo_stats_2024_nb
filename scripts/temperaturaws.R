library(tidyverse)
library(sf)

temperaturas <- read_delim("data/inamhi-temperaturamedia-2019diciembre.csv",
                           delim = ";",
                           locale = locale(decimal_mark = ","))

mapa_ecuador <- st_read("data/shp/LIMITE_PROVINCIAL_CONALI_CNE_2022.shp")

temperaturas %>% 
  count(NombreEstacion)

estaciones <- temperaturas %>% 
  distinct(NombreEstacion,latitud2,longitud2,altitud) %>% 
  filter(if_any(c(latitud2,longitud2,altitud), ~!is.na(.x))) 

mapa_ecuador <- st_simplify(mapa_ecuador, dTolerance = 1000)

estaciones_puntos <- st_as_sf(estaciones,coords = c("longitud2","latitud2"),crs = 4326) %>% 
  st_transform(32717)

estaciones_puntos %>%
  ggplot()+
  geom_sf(data = mapa_ecuador)+
  geom_sf(color = "red")+
  theme_minimal()

mapa_pichincha <- mapa_ecuador %>% 
  filter(PROVINCIA == "PICHINCHA")


dentro_pichincha <- st_join(estaciones_puntos, 
                            mapa_pichincha, 
                            join = st_within,
                            left = FALSE)


dentro_pichincha %>%
  ggplot()+
  geom_sf(data = mapa_pichincha)+
  geom_sf(color = "red")+
  theme_minimal()


dentro_pichincha <- dentro_pichincha %>% 
  select(-altitud) %>% 
  left_join(temperaturas, by = c("NombreEstacion" = "NombreEstacion")) 


dentro_pichincha %>%
  as_tibble() %>% 
  count(anio) %>% 
  arrange(desc(anio))


estacion_tola <- dentro_pichincha %>%
  filter(anio == 2019,
         NombreEstacion == "LA TOLA")


estacion_tola <- estacion_tola %>% 
  select(NombreEstacion,anio,ene:dic) 

estadisticos <- estacion_tola %>% 
  as_tibble() %>% 
  select(ene:oct) %>% 
  pivot_longer(cols = everything()) %>% 
  summarise(media = mean(value),
            sd = sd(value))


noviembre <- estacion_tola$nov

diciembre <- 16.4

prior_prob <- dnorm(diciembre, mean = estadisticos$media, sd = estadisticos$sd)

# Probabilidad condicional
conditional_prob <- dnorm( noviembre, mean = diciembre, sd = estadisticos$sd)

# Probabilidad total
total_prob <- dnorm(noviembre, mean = estadisticos$media, sd = estadisticos$sd)

# Probabilidad posterior
posterior_prob <- (conditional_prob * prior_prob) / total_prob

posterior_prob
