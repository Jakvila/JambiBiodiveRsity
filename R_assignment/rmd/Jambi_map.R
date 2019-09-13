library(raster)
library(data.table)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maptools)
library(grid)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
library(ggrepel)
library(ggsn)
Indo = getData("GADM", country = "Indonesia", level = 2)
load("~/ownCloud/Efficiency/Data/C01PlotTable.RDA")
C01PlotTable = data.table(read.csv("Data/Sample2018.csv"))
Jambi = Indo[Indo$NAME_1 == "Jambi",]
plot(Jambi)
VillageDataRaw = data.table(read.csv("Data/VillageRaw.csv"))
VillageDataRaw2015 = VillageDataRaw[wave == 2015, ]
VillageKey = unique(VillageDataRaw2015)
VillageKey[, c("hid", "vid") := lapply(.SD, as.character), .SDcols = c("hid", "vid")]
VillageKey = VillageKey[, .(hid, vid, rid, vname)]
C01PlotTable = merge(C01PlotTable, VillageKey, by = "hid", all.x = T)
C01PlotTable$CoordinateS =  C01PlotTable$CoordinateS*-1
C01PlotTable[, hid := as.character(hid)]
points(C01PlotTable$CoordinateE, C01PlotTable$CoordinateS, cex = 2)



Jambi@data$id = rownames(Jambi@data)
JambiPoints = fortify(Jambi, region="id")
JambiDF = join(JambiPoints, Jambi@data, by="id")


villageCoordinate = aggregate(C01PlotTable, by=list(vname=C01PlotTable$vname), mean, na.rm = T)[, c("vname", "CoordinateS", "CoordinateE")]
villagePlots = aggregate(C01PlotTable, by=list(vname=C01PlotTable$vname), length)[, c("vname", "hid")]
villageCoordinate = merge(villageCoordinate, villagePlots, by = "vname")
ggplot(JambiDF) + 
  geom_polygon(aes(long,lat,group=group,fill=NAME_2)) +
  coord_equal() +
  scale_fill_brewer(palette="Spectral") +
  geom_point(data = C01PlotTable, aes(x = CoordinateE, y = CoordinateS, color = vname), size = .8) +
  geom_point(data = villageCoordinate, aes(x = CoordinateE, y = CoordinateS, color = vname, size = hid)) +
  geom_text(data = villageCoordinate, aes(x = CoordinateE, y = CoordinateS, label = hid))+
  geom_text_repel(data = villageCoordinate, aes(x = CoordinateE, y = CoordinateS, label = vname), size =3) +
  # scalebar(1000, xy=c(103.5, -2.5), type='bar', divs=4)+
  geom_point(aes(x= 103.6131, y = -1.610123), size = 5)+
  geom_text(aes(x= 103.6131, y = -1.660123), size = 3, label = "Jambi City")+
  geom_point(aes(x= 102.3235, y = -2.076456), size = 5)+
  geom_text(aes(x= 102.3235, y= -2.076456), size = 3, label = "Bangko")+
  geom_point(aes(x= 102.7549, y= -2.622461), size = 5)+
  geom_text(aes(x= 102.7549, y= -2.622461), size = 3, label = "sarolangun")+
  guides(fill=FALSE, alpha=FALSE, size=FALSE, colour = FALSE)+
  ggsn::scalebar(JambiDF, dist = 100, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')+
  ggtitle("C01 Villages and Plots in Jambi Province")+
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
        legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
        axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
        axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))


library(cluster)
df <- data.frame(X = rnorm(100, 0), Y = rpois(100, 2))
plot(df$X, df$Y)
points(pam(df, 1)$medoids, pch = 16, col = "red")

C01PlotTable
HHIdentification = data.table(read.csv("Data/HHIdentification.csv"))
HHIdentification[, hid := as.character(hid)]
C01PlotTable = merge(C01PlotTable, HHIdentification, by = "hid", all.x = T)  