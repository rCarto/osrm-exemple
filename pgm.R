# Chargement des données
load("data/LoireAtlantique.RData")

# chargement des librairies
library(osrm)
library(rgeos)
library(cartography)

# création d'un data frame des communes avec leurs coordonées en WGS84
COM44.df <- data.frame(as.character(COM44.spdf$INSEE_COM), 
                       coordinates(spTransform(COM44.spdf, 
                                               CRSobj = 
                                                 "+init=epsg:4326")))
# renommer les colonnes
colnames(COM44.df) <- c("id", "x", "y")


mateuclid <- rgeos::gDistance(COM44.spdf, byid = T)


# création d'une matrice distances routières
matroute <- osrmTableOD(dfo = COM44.df, 
                        ido = "id", xo = "x", yo = "y", 
                        dfd = COM44.df,
                        idd = "id", xd = "x", yd = "y")





# Calcul d'indice d'accessibilité local d'ensemble
shimbel <- data.frame(id = COM44.df$id, 
                      ShimbelRoute = sum(matroute) / colSums(matroute), 
                      ShimbelEuclid = sum(mateuclid) / colSums(mateuclid))







png("img/shimbEuclidi.png", width = 474, height = 339)
par(mar = c(0,0,1.1,0))
# plot vide pour cadrer la carte
plot(COM44.spdf, col = NA, border = NA, bg = "#A6CAE0")
# plot du fond france
plot(FRA.spdf, add=T, col = "#E3DEBF", border = "#03779E")
# Index de Shimbel avec les distances euvclidiennes
choroLayer(spdf = COM44.spdf, spdfid = "INSEE_COM",
           df = shimbel, 
           var = 'ShimbelEuclid', nclass = 6, 
           legend.pos = "left",
           legend.title.txt = "Indice de\nShimbel",
           add=T)
# Ajout d'un habillage
layoutLayer(title = "Accessibilité euclidienne globale des communes de Loire-Atlantique", 
            sources = "T. Giraud, 2015", 
            author = "Fond de carte : GEOFLA 2015, IGN")
dev.off()



png("img/shimbRoute.png", width = 474, height = 339)
par(mar = c(0,0,1.1,0))
# plot vide pour cadrer la carte
plot(COM44.spdf, col = NA, border = NA, bg = "#A6CAE0")
# plot du fond france
plot(FRA.spdf, add=T, col = "#E3DEBF", border = "#03779E")
# Index de Shimbel avec les distances euvclidiennes
choroLayer(spdf = COM44.spdf, spdfid = "INSEE_COM",
           df = shimbel, 
           var = 'ShimbelRoute', nclass = 6, 
           legend.pos = "left",
           legend.title.txt = "Indice de\nShimbel",
           add=T)
# Ajout d'un habillage
layoutLayer(title = "Accessibilité routière globale des communes de Loire-Atlantique",
            sources = "T. Giraud, 2015", 
            author = "Fond de carte : GEOFLA 2015, IGN ; Data (c) OpenStreetMap contributors ; Routes: OSRM. http://project-osrm.org/")

dev.off()




# calcul du rapport entre les deux indices
shimbel$eff <-  shimbel$ShimbelRoute/shimbel$ShimbelEuclid

# obtension d'une tuile osm-transport sur le departement
# avec la fonction getTiles de cartography
tiles44 <- getTiles(spdf = COM44.spdf, type = "osm-transport")

png("img/rapport.png", width = 474, height = 339)
par(mar = c(0,0,1.1,0))
## affichage de la carte
# plot vide pour cadrer la carte
plot(COM44.spdf, col = NA, border = NA)
# plot du fond de carte osm-transport
tilesLayer(tiles44, add=T)
# Index de Shimbel avec les distances euvclidiennes
choroLayer(spdf = COM44.spdf, spdfid = "INSEE_COM",
           df = shimbel, 
           var = 'eff', 
           breaks = c(min(shimbel$eff),
                      0.8,0.9,0.95, 1/0.95, 1/0.9, 
                      max(shimbel$eff)),
           col = carto.pal(pal1 = "green.pal",3, 
                           "wine.pal",2, 
                           transparency = T, 
                           middle = T),
           lwd = 0.2,
           legend.pos = "left",
           legend.title.txt = "Efficacité",
           legend.frame = TRUE,
           legend.values.rnd = 2,
           add=T)

# Ajout d'un habillage
layoutLayer(title = "Efficacité du réseau routier des communes de Loire-Atlantique", 
            sources = "T. Giraud, 2015", 
            author = "Fond de carte : OpenStreetMap contributors ; Routes: OSRM. http://project-osrm.org/")

dev.off()



# affichage de la carte
png("img/route.png", width = 474, height = 339)
par(mar = c(0,0,1.1,0))
# plot vide pour cadrer la carte
plot(COM44.spdf, col = NA, border = NA)
# plot du fond de carte osm-transport
tilesLayer(tiles44, add=T)
# identifiants des points de départ et d'origine
idOrigine <- "44120"
idDestination <- c("44080", "44045", "44158", "44015", "44128","44068","44137", 
                   "44052", "44013","44176")
for (i in idDestination){
  
  # Calcul du trajet par le plus court chemin
  route <- osrmViarouteGeom(xo = COM44.df[COM44.df$id==idOrigine,"x"], 
                            yo = COM44.df[COM44.df$id==idOrigine,"y"],
                            xd = COM44.df[COM44.df$id==i,"x"], 
                            yd = COM44.df[COM44.df$id==i,"y"])
  # Transformation en SpatialLinesDataFrame
  routeLines <- Lines(slinelist = Line(route[,2:1]), 
                      ID = paste(idOrigine, i, sep="_"))
  routeSpatialLines <- SpatialLines(LinesList = list(routeLines), 
                                    proj4string = CRS("+init=epsg:4326"))
  df <- data.frame(id = sapply(methods::slot(routeSpatialLines, "lines"), 
                               methods::slot, "ID"))
  routeSpatialLinesDataFrame <- SpatialLinesDataFrame(routeSpatialLines, 
                                                      data = df, 
                                                      match.ID = FALSE)   
  # projection de la route dans la projection des communes
  maroute <- spTransform(x = routeSpatialLinesDataFrame, 
                         CRSobj = COM44.spdf@proj4string)
  plot(maroute, 
       add=T, lwd = 3, col = "red")
}
plot(COM44.spdf[COM44.spdf$INSEE_COM=="44120",],add=T, col = "#00ff0050")

layoutLayer(title = "Quelques trajets à partir de la commune Le Pellerin",
            author="Fond de carte : OpenStreetMap contributors ; Routes: OSRM. http://project-osrm.org/", 
            sources = "T. Giraud, 2015")
dev.off()

