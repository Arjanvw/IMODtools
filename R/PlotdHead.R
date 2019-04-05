#' Dit is de functie waarmee de rasterlayerobject wordt geplot in een .png bestand
#'
#' @param title text die boven de plot komt te staan
#' @param outfolder map waarin de .png file moet worden opgeslagen
#' @param outname naam van de .png
#' @param shapename naam van een shapefile die ter orientatie over het figuur geplot wordt (zonder .shp)
#' @param shapefolder map waarin de shapefile staat
#' @param dHeadRaster Rasterlayer object dat geplot moet worden
#' @param xmin minimale en maximale x- en y-waardes van het plotwindow
#' @param xmax minimale en maximale x- en y-waardes van het plotwindow
#' @param ymin minimale en maximale x- en y-waardes van het plotwindow
#' @param ymax minimale en maximale x- en y-waardes van het plotwindow
#' @param width breedte van de png in cm
#' @param height hoogte van de png in cm
#' @export
plot.dhead=function(title, # text die boven de plot komt te staan
                    outfolder, # map waarin de .png file moet worden opgeslagen
                    outname, # naam van de .png
                    shapename, # naam van een shapefile die ter orientatie over het figuur geplot wordt (zonder .shp)
                    shapefolder, # map waarin de shapefile staat
                    dHeadRaster, # Rasterlayer object dat geplot moet worden
                    xmin,xmax,ymin,ymax,# minimale en maximale x- en y-waardes van het plotwindow
                    width, # breedte van de png in cm
                    height) # hoogte van de png in cm
{
  # klassen en kleur van het rasterobject
  breaks=c(-100,-1,-0.5,-0.2,-0.1,-0.05,0.05,0.1,0.2,0.5,1,100)
  breaklabels=c("<-1.0","-1.0 - -0.5","-0.5 - -0.2","-0.2 - -0.1","-0.1 - -0.05","-0.05 - 0.05","0.05 - 0.1", "0.1 - 0.2","0.2 - 0.5","0.5 - 1.0",">1.0")
  colbreaks=c("<-1.0"="darkred",
              "-1.0 - -0.5"="firebrick2",
              "-0.5 - -0.2"="darkorange",
              "-0.2 - -0.1"="yellow",
              "-0.1 - -0.05"="lightgoldenrod1",
              "-0.05 - 0.05"="grey80",
              "0.05 - 0.1"="aquamarine",
              "0.1 - 0.2"="deepskyblue2",
              "0.2 - 0.5"="dodgerblue3",
              "0.5 - 1.0"="darkorchid3",
              ">1.0"="darkslateblue")


  # omzetten rasterlayer object naar dataframe
  dhead_spdf <- as(dHeadRaster, "SpatialPixelsDataFrame")
  dhead_df <- as.data.frame(dhead_spdf)
  colnames(dhead_df) <- c("dhead", "x", "y")

  # Vervang de eerste waardes met de waardes van de breaklabels zodat alle labels in de plot komen
  dhead_df$classes=cut(x = dhead_df$dhead,breaks = breaks,labels = breaklabels,include.lowest = T,right = F)
  dhead_df$classes[1:11]=breaklabels

  # importeer shapefile
  projectgebied=readOGR(dsn = shapefolder,layer = shapename)

  # converteer shapefile naar dataframe voor ggplot
  projectgebied_df=fortify(projectgebied)

  # Definieer een ggplot input variabele map
  map = ggplot() +
    # plot de raster
    geom_tile(data = dhead_df,aes(x=x, y=y, fill=classes))+
    # plot de orientatie shapefile
    geom_path(data = projectgebied_df,aes(x = long, y = lat, group = group),color = 'black', size = 0.5)+ # project area shapefile
    # Instellen van de kleuren van de breaks
    scale_fill_manual(values = colbreaks)+
    # zet de coordinaten zodat dx gelijk is aan dy. Instellen xmin, xmax, ymin en ymax
    coord_fixed(ratio = 1, xlim = c(xmin,xmax), ylim = c(ymin,ymax), expand = F)+
    # Opmaak van de figuur op basis van thema in GGplot2theme.r
    maptheme(base_size = 10,base_family = "Arial")+
    # titel van de grafiek
    ggtitle(label = title)+
    # Naam van de legenda
    guides(fill=guide_legend(title="Verschil"))+
    # kleur van de legendavulling
    theme(legend.key = element_rect(fill = "grey", colour = "grey"))

  # plot de ggplot map in png in outputfolder
  if(dir.exists(outfolder)==F){dir.create(outfolder)}
  png(file = paste(outfolder,"/",outname,".png",sep = ""),width = width,height = height, units = "cm",res=600)
  print(map)
  dev.off()
}
