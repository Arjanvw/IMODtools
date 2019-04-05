#' In dit script kan met een data.frame met afwijkingen en een rasterlayer objext een bollenkaart gemaakt worden
#'
#' @param title text die boven de plot komt te staan
#' @param data data frame met afwijkingen die geplot moeten worden
#' @param diffcol kolom in data waarin de afwijkingen per laag staan
#' @param xcol kolom in data waar de x-waarde van het meetpunt staat
#' @param ycol komom in data waar de y-waarde van het meetpunt staat
#' @param laycolkolom in data waar de modellaag staat
#' @param minlay bovenste laag die geplot moet worden, gebruiken voor meerdere modellagen binnen een wvp
#' @param maxlay onderste laag die geplot moet worden, gebruiken voor meerdere modellagen binnen een wvp
#' @param outfolder map waarin de .png file moet worden opgeslagen
#' @param outname naam van de .png
#' @param shapename naam van een shapefile die ter orientatie over het figuur geplot wordt (zonder .shp)
#' @param shapefolder map waarin de shapefile staat
#' @param GGRaster Rasterlayer object dat geplot moet worden
#' @param xmin minimale en maximale x- en y-waardes van het plotwindow
#' @param xmax minimale en maximale x- en y-waardes van het plotwindow
#' @param ymin minimale en maximale x- en y-waardes van het plotwindow
#' @param ymax minimale en maximale x- en y-waardes van het plotwindow
#' @param width breedte van de png in cm
#' @param height hoogte van de png in cm
#' @return .png met bollenkaart
#' @export
plot.bollenkaart=function(title, # text die boven de plot komt te staan
                          data, # data frame met afwijkingen die geplot moeten worden
                          diffcol, # kolom in data waarin de afwijkingen per laag staan
                          xcol, # kolom in data waar de x-waarde van het meetpunt staat
                          ycol, # komom in data waar de y-waarde van het meetpunt staat
                          laycol, #kolom in data waar de modellaag staat
                          minlay,maxlay, # modellagen die geplot moeten worden in bollenkaart
                          outfolder, # map waarin de .png file moet worden opgeslagen
                          outname, # naam van de .png
                          shapename, # naam van een shapefile die ter orientatie over het figuur geplot wordt (zonder .shp)
                          shapefolder, # map waarin de shapefile staat
                          GGRaster, # Rasterlayer object dat geplot moet worden
                          xmin,xmax,ymin,ymax, # minimale en maximale x- en y-waardes van het plotwindow
                          width, # breedte van de png in cm
                          height) # hoogte van de png in cm
{
  # Definieer data
  d=data


  # hernoem diffcol, xcol and ycol
  colnames(d)[diffcol]="Dif"
  colnames(d)[xcol]="X"
  colnames(d)[ycol]="Y"
  colnames(d)[laycol]="Lay"

  #verwijder alle punten buiten x- en y-limiet
  d=d[d$X<xmax,]
  d=d[d$X>xmin,]
  d=d[d$Y<ymax,]
  d=d[d$Y>ymin,]

  # zet d om naar data.frame met 4 kolommen
  d=d[,c(xcol,ycol,laycol,diffcol)]

  # Defenitie klassen afwijkingen bollenkaart
  breaks=c(-100,-1,-0.5,-0.2,-0.1,-0.05,0.05,0.1,0.2,0.5,1,100)
  breaklabels=c("<-1.0","-1.0 - -0.5","-0.5 - -0.2","-0.2 - -0.1","-0.1 - -0.05","-0.05 - 0.05","0.05 - 0.1", "0.1 - 0.2","0.2 - 0.5","0.5 - 1.0",">1.0")

  # Kleuren voor de klassen
  colbreaks=c("<-1.0"="darkred",
              "-1.0 - -0.5"="firebrick2",
              "-0.5 - -0.2"="darkorange",
              "-0.2 - -0.1"="yellow",
              "-0.1 - -0.05"="lightgoldenrod1",
              "-0.05 - 0.05"="white",
              "0.05 - 0.1"="aquamarine",
              "0.1 - 0.2"="deepskyblue2",
              "0.2 - 0.5"="dodgerblue3",
              "0.5 - 1.0"="darkorchid3",
              ">1.0"="darkslateblue")


  # bereken klassen voor modelafwijkingen
  d$Afwijking=cut(x = d$Dif,breaks = breaks,labels = breaklabels,include.lowest = T,right = F)

  # dframe d2 met alle klasses zodat alle klasses geplot worden
  d2=data.frame(X=NA,Y=NA,Lay=minlay,Dif=0,Afwijking=breaklabels)

  # Voeg d2 aan d toe
  d3=rbind(d,d2)

  # importeer shapefile
  projectgebied=readOGR(dsn = shapefolder,layer = shapename)

  # converteer shapefile naar dataframe voor ggplot
  projectgebied_df=fortify(projectgebied)

  # selecteer metingen binnen minlay en maxlay uit d3
  s=subset(x = d3,Lay>(minlay-1)&Lay<(maxlay+1))

  # Zet achtergrond rasterlayer object om naar data.frame
  GG_spdf <- as(GGRaster, "SpatialPixelsDataFrame")
  GG_df <- as.data.frame(GG_spdf)
  colnames(GG_df) <- c("GGNAP", "x", "y")

  # Definieer een ggplot input variabele map
  map = ggplot() +
    #PLot rasterlayer object
    geom_raster(data = GG_df,aes(x=x, y=y, fill=GGNAP))+
    # plot isohypsen van raster, elke 25 cm
    geom_contour(data = GG_df,aes(x=x, y=y,z=GGNAP),colour="grey50",binwidth=0.25,size=0.15)+
    # plot isohypsen van raster, elke 100 cm
    geom_contour(data = GG_df,aes(x=x, y=y,z=GGNAP),colour="grey50",binwidth=1,size=0.35)+
    # plot orientatie shapefile
    geom_path(data = projectgebied_df,aes(x = long, y = lat, group = group),color = 'black', size = 0.5)+
    # kleuren voor rasterlayer object
    scale_fill_gradientn(colours = matlab.like2(20))+
    # labels voor modelafwijkingen (bollen)
    geom_text_repel(data = s,aes(x = X,y = Y),label=round(x = s$Dif,digits = 2),size=2.5,min.segment.length = 0)+
    # puntlocaties modelafwijkingen (bollen)
    geom_point(data = s,aes(x = X,y = Y),size=2,shape=16,colour="Black")+
    # buffer rondom puntlocaties modelafwijkingen (bollen)
    geom_point(data = s,aes(x = X,y = Y,colour=Afwijking),size=1,shape=19)+
    # titel grafiek
    ggtitle(label = title)+
    # kleuren voor de modelafwijkingen (bollen)
    scale_colour_manual(values = colbreaks)+
    # zet de coordinaten zodat dx gelijk is aan dy. Instellen xmin, xmax, ymin en ymax
    coord_fixed(ratio = 1, xlim = c(xmin,xmax), ylim = c(ymin,ymax), expand = F)+
    # instellen volgorde legenda
    guides(fill = guide_colourbar(order = 1),colour = guide_legend(order = 2))+
    # Opmaak van de figuur op basis van thema in GGplot2theme.r
    maptheme(base_size = 10,base_family = "Arial")+
    # kleur van de legendavulling
    theme(legend.key = element_rect(fill = "grey", colour = "grey"))

  # plot de ggplot map in png in outputfolder
  if(dir.exists(outfolder)==F){dir.create(outfolder)}
  png(file = paste(outfolder,"/",outname,".png",sep = ""),width = width,height = height, units = "cm",res=600)
  print(map)
  dev.off()
}
