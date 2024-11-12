#####################################################################
#####################################################################
#install.packages("geoR", dep=TRUE) 
#install.packages("sp", dep=TRUE) 
#install.packages("spdep", dep=TRUE)
#install.packages("classInt", dep=TRUE)
#install.packages("RColorBrewer", dep=TRUE)
#####################################################################
#####################################################################
require(geoR)         ## geoestatistica
require(sp)           ## (classes) para representação de dados espaciais no R 
require(spdep)        ## funções análises de dados de áreas
#require(classInt)    ## rotinas para facilitar a divisão de dados em classes
require(RColorBrewer) ##  criar palhetas de cores nas visualizações em mapas
require(maptools)     ## manipulação de arquivos em formatos shape




#################################################################
######## Exemplo 01 com dados de suicidio -Integração##########################
############### Upload dos dados para trabalho #####################
### Tabela 

data1=read.csv2("Suicidio.csv",header=T, sep=";",dec=",")
attach(data1)
head(data1)

#################################################################
## Carregar o mapa
###br<- readShapePoly(file.choose()) ### dentro da pasta "arquivo shape" esta o mapa
br=readShapePoly("CemBrEst.shp")
plot(br) ### desenhar o mapa
class(br) ## classe de dados
names(br)  ## nomes das variaveis

br@data ### mostrar dados em formato de tabela


#######################################################################
########################################################################
######## Definindo a classe dos dados ##################################
########################################################################

class(br)
slotNames(br)
head(br@data)

class(br@polygons)
names(br@polygons)
length(br@polygons)
class(br@polygons[[1]])
slotNames(br@polygons[[1]])
length(br@polygons[[1]]@Polygons)
class(br@polygons[[1]]@Polygons)
class(br@polygons[[1]]@Polygons[[1]])
slotNames(br@polygons[[1]]@Polygons[[1]])

## centroide do poligono
br@polygons[[1]]@Polygons[[1]]@labpt

## pegando os centroides de todos as UFs
centroides <- t(sapply(br@polygons, function(x) x@Polygons[[1]]@labpt))
head(centroides)
dim(centroides)

class(br@polygons[[1]]@Polygons)


######################################################################
################ BASE FINAL DE ANALISE ###############################
################ COMPOSICAO DOS BANCOS DE DADOS ######################
######################################################################

## combinando os atributos, porÈm com opcao para mandar a ordem
## do objeto espacial (centroides): o objeto espacial deve vir primeiro
## e sort=FALSE evita reordenar perdendo a ordem correta das geometrias

dados = merge(br@data,data1, by="CD_GEOCUF",sort=FALSE)
dim(dados)
head(dados)
class(dados)
names(dados)


#############################################################
#### Construindo a matriz de vizinhanÁa #####################
#############################################################

coords = coordinates(br)
vizinho <- poly2nb(br) ## matriz de vizinhança
class(vizinho)
print(vizinho)
summary(vizinho) 

###################################################
####### Visualizando o grafo de vizinhança ########
###################################################

##Outra forma de calcular transformando para um objeto sp 

vizinho <- poly2nb(as(br, "SpatialPolygons"))
plot(br,border="gray", main="Grafo da Vizinhança")
plot(vizinho, coordinates(br), add=TRUE, col="blue", pch=",")
##Grafo de vizinhança


################################################################################
################################################################################
#### Gerar a matriz de vizinhanÁa #####
#library(spdep)
temp <- poly2nb(br)
nb2INLA("LDN.graph", temp)
LDN.adj <- paste(getwd(),"/LDN.graph",sep="")


summary(taxa_ot_2011)  
summary(taxa_ot_2012)
summary(taxa_ot_2013) 
summary(taxa_ot_2014)
summary(taxa_ot_2015) 
summary(taxa_ot_2016) 
summary(taxa_ot_2017)
summary(taxa_ot_2018)  
summary(taxa_ot_2019)  
summary(taxa_ot_2020)


summary( taxa_os_2011) 
summary( taxa_os_2012)
summary( taxa_os_2013)
summary( taxa_os_2014)  
summary( taxa_os_2015)  
summary( taxa_os_2016)  
summary( taxa_os_2017)
summary( taxa_os_2018) 
summary( taxa_os_2019)  
summary( taxa_os_2020) 



############################################################
######################## Obitos Totais #####################
brks <- c(-Inf, 430,530,600, Inf)
cols<-brewer.pal(4,"BrBG")### cores em escala amarelas
###########################################################
############################################################

#jpeg(file="taxa_ot_2011.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2011, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2011")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()


#jpeg(file="taxa_ot_2012.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2012, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2012")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()



#jpeg(file="taxa_ot_2013.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2013, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2013")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()


#jpeg(file="taxa_ot_2014.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2014, brks)])
#text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2014")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()



jpeg(file="taxa_ot_2015.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2015, brks)])
#text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2015")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
dev.off()



jpeg(file="taxa_ot_2016.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2016, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2016")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
dev.off()

jpeg(file="taxa_ot_2017.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2017, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2017")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
dev.off()


jpeg(file="taxa_ot_2018.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2018, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2018")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
dev.off()


jpeg(file="taxa_ot_2019.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2019, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2019")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
dev.off()

#jpeg(file="taxa_ot_2020.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2020, brks)])
#text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Ûbitos totais \n Ano 2020")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()







############################################################
######################## Obitos Totais #####################
brks <- c(-Inf, 4.50,5.50,6.50, Inf)
cols<-brewer.pal(4,"BrBG")### cores em escala amarelas
###########################################################
############################################################

#jpeg(file="taxa_os_2011.jpeg")
plot(br, col=cols[findInterval(dados$taxa_os_2011, brks)])
#text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Obitos suicidos \n Ano 2011")
legend("bottomright", c("<4.50", "4.50|-5.50", "5.50|-6.50", ">=6.50"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()


#jpeg(file="taxa_ot_2012.jpeg")
plot(br, col=cols[findInterval(dados$taxa_ot_2012, brks)])
text(coordinates(br), as.character(dados$NM_ESTADO), cex=0.6)
title(main="Taxa de Óbitos totais \n Ano 2012")
legend("bottomright", c("<430", "430|-530", "530|-600", ">=600"), fill=cols, title = "No. por 100 mil habit.",cex=0.9,bty="n")
#dev.off()


















