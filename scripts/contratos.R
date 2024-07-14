library(tidyverse)
library(sf)
library(ggrepel)

unzip("data/shp/LIMITE_PROVINCIAL_CONALI_CNE_2022.zip")

data <- read_sf("LIMITE_PROVINCIAL_CONALI_CNE_2022.shp")

data <- data %>% 
  st_simplify(dTolerance = 50)


# En general los contratos se concentran en las ciudades capitales adminsitrativas

data %>% 
  ggplot() +
  geom_sf(aes(fill = CONTRATOS))

# Distribución de la variables
data %>% 
  ggplot() +
  geom_histogram(aes(x = CONTRATOS))

# Usamos transformaciones pero OJO que al transformar la variable
# aleatoria cambia su función de distribución

data %>% 
  mutate(log_contratos = log(CONTRATOS)) %>% 
  ggplot() +
  geom_histogram(aes(x = log_contratos))


# usamos el logaritmo para plotear pero los valores presentados correpsonden a la variable original


data %>% 
  mutate(log_contratos = log(CONTRATOS)) %>% 
  ggplot() +
  geom_sf(aes(fill = log_contratos))



# Usamos mutate para crear una nueva variable considerando un vector externo

data <- data %>% 
  mutate(
    log_contratos = log(CONTRATOS),
    area = data %>% 
           st_area(),
    log_area = log(area))


# El modelo log log

lm(data = data,formula = log_contratos ~ log_area) %>% 
  summary()

# que hace geom_label

data %>% 
  ggplot() +
  geom_histogram(aes(x = CONTRATOS)) +
  geom_label(aes(x = 10,y = 10, label = "Hola")  )


# Vamos a sacar centroides 

centroides <- data %>% 
  st_centroid() 


centroides <- bind_cols(centroides,centroides %>% st_coordinates())

centroides <- centroides %>% 
  mutate(label = str_c(PROVINCIA,"\n",CONTRATOS)) 

centroides %>% 
  pull(geometry)

# usamos el logaritmo para plotear pero los valores presentados correpsonden a la variable original


data %>% 
  mutate(log_contratos = log(CONTRATOS)) %>% 
  ggplot() +
  geom_sf(aes(fill = log_contratos)) +
  geom_label_repel(data = centroides, aes(x = X, y = Y,label = label))


