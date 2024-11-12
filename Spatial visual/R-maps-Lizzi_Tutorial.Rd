#### instalação de pacotes

#install.packages("sf")
#install.packages("terra")
#install.packages("spData")
#install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

### carregar os pacotes necessários
library(sf)    # classes and functions for vector data
library(terra) # classes and functions for raster data
library(dplyr)  #manipulacao de dados
library(spData)  # load geographic data
library(spDataLarge)  # load larger geographic data

########################################################
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

### banco de dados do R
class(world) ###[ classe de dados]
names(world) ## nomes das colunas [estruturado]

plot(world) ## mapas das variáveis


########################################################
summary(world["lifeExp"]) ### descrição
world_mini = world[1:2, 1:3]
world_mini


########################################################
world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))
world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)
class(world_tbl)


########################################################
### mapas básicos
plot(world[3:6])
plot(world["pop"])

### filtro 
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

### destaque no gráfico
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

##Continentes dos países (representados por cor de preenchimento) e populações ###de 2015 (representadas por círculos, com área proporcional à população).

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

#st_centroid()para converter um tipo de geometria (polígonos) 
##em outro (pontos), cuja estética varia com o cexargumento.

#Índia em contexto, demonstrando o argumento expandBB.
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)


########################################################
### dados de raster
## O pacote terra suporta objetos raster em R.

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)


my_rast

plot(my_rast)

