#' Deze functies maakt tijdreeksen van iMODFLOW bestanden.
#'
#' @param ts_folder map waarin .txt bestanden staat met daarin gemeten en berekende grondwaterstanden. Dit wordt aangemaakt met de idf timeserietool
#' @param outfolder locatie waar alle tijdreeksen weggeschreven worden. voor elke modellaag wordt een map aangemaakt
#' @param metafile File met daarin minimaal de volgende data: naamkolom en kolom met de modellaag van de peilbuis. Dit kan zowel een csv als een ipf zijn
#' @param namecol kolom met de naam van de peilbuis. Deze moet overeenkomen met de naam van de .txt bestanden.
#' @param laycol kolom met de modellaag van de peilbuisfilter
#' @param nlayers aantal lagen dat het model heeft
#' @param datemin minimum datum grafiek
#' @param datemax maximum datum grafiek
#' @param width breedte grafiek in cm, standaard 16 cm
#' @param height hoogte grafiek in cm, standaard 24,7/3
#' @export
plot.timeseries=function(ts_folder, # map waarin .txt bestanden staat met daarin gemeten en berekende grondwaterstanden. Dit wordt aangemaakt met de idf timeserietool
                         outfolder, # locatie waar alle tijdreeksen weggeschreven worden. voor elke modellaag wordt een map aangemaakt
                         metafile, # File met daarin minimaal de volgende data: naamkolom en kolom met de modellaag van de peilbuis. Dit kan zowel een csv als een ipf zijn
                         namecol, # kolom met de naam van de peilbuis. Deze moet overeenkomen met de naam van de .txt bestanden.
                         laycol, # kolom met de modellaag van de peilbuisfilter
                         nlayers, # aantal lagen dat het model heeft
                         datemin, # minimum datum grafiek
                         datemax, # maximum datum grafiek
                         width = 16, # breedte grafiek in cm, standaard 16 cm
                         height = 24.7/3) # hoogte grafiek in cm, standaard 24,7/3
{
  # Hier worden de namen van alle .txt bestanden in de ts_folder in een list gezet
  files=list.files(ts_folder)

  # Deze stap maakt voor elke laag een map in de outfolder
  for(i in 1:nlayers)
  {
    Layfolder=paste(outfolder,"/laag",i,sep = "")
    if(dir.exists(Layfolder)==F){dir.create(Layfolder)}
  }

  # In deze for-loop worden de txt-files uitgelezen en wordt de tijdreeks geplot
  for(i in 1:length(files))
  {
    # lees alle data in txt file i in in data frame d
    txtfile=files[i]
    d=read.ipf(ipf_file = paste(ts_folder,txtfile,sep = "/"))

    #zet datum kolom om naar POSIXct-format
    d[,1]= as.Date(as.character(d[,1]), "%Y%m%d")
    d[,1]=as.POSIXct(d[,1],"%d-%m-%Y")

    #Vervang -9999 door NA
    d[,2]=ifelse(test = d[,2]==-9999,NA,d[,2])
    d[,3]=ifelse(test = d[,3]==-9999,NA,d[,3])

    #Maak aparte dataset met gemeten waardes en gemodelleerde waardes en ze deze onder elkaar in de data.frame input
    calculated=d[!is.na(d[,2]),1:2]
    calculated[,3]="Model"
    colnames(calculated)=c("datum","waarde","bron")
    measured=d[,c(1,3)]
    measured[,3]="Gemeten"
    colnames(measured)=c("datum","waarde","bron")
    input=rbind(calculated,measured)

    #Definieer dmin en dmax
    if(missing(datemin)){dmin=min(input$datum)}else{dmin=datemin}
    if(missing(datemax)){dmax=max(input$datum)}else{dmax=datemax}

    # Lees metadata file naar data frame m
    m=if(substr(x = metafile,nchar(metafile)-3,nchar(metafile))==".csv"){read.csv(file = metafile,header = T)}else{read.ipf(ipf_file = metafile)}

    # Bepaal in welke modellaag de peilbuisfilter is aan de hand van de metafile
    laag=m[match(x = as.factor(paste(substr(txtfile,1,nchar(txtfile)-4),sep = "")),table = m[,namecol]),laycol]

    # Definieer een ggplot input variabele p
    p=ggplot(data = input,aes(x = datum,y = waarde,group = bron))+
      # Plot lijnen voor de modeldata
      geom_line(aes(linetype=bron),size=0.75,color="red")+
      # Plot punten voor de gemeten data
      geom_point(aes(color=bron,shape=bron,size=bron))+
      # definieer de kleur van de punten
      scale_color_manual(values=c("black","red"))+
      # definieer de vorm van de punten
      scale_shape_manual(values=c(19,20))+
      # definieert de grootte van de punten
      scale_size_manual(values=c(0.75,0.01))+
      # definieer het lijntype
      scale_linetype_manual(values=c("blank","solid"))+
      # Zet een titel boven de grafiek
      ggtitle(label = paste(substr(txtfile,1,nchar(txtfile)-4),", laag",laag,sep = ""))+
      # Rond de y coordinaten af naar hele getallen
      scale_y_continuous(expand = c(0,0),limits = c(floor(min(d[,2:3],na.rm = T)),ceiling(max(d[,2:3],na.rm = T))))+
      # Label voor y-as
      ylab(label = "Grondwaterstand t.o.v. NAP")+
      # Instelling voor x-as
      scale_x_datetime(expand=c(0,0),date_breaks = "2 years",date_minor_breaks = "1 year",limits=c(dmin,dmax))+
      # Opmaak van de figuur op basis van thema in GGplot2theme.r
      theme3(base_size = 10,base_family = "Arial")+
      # Verwijder de titel van de x-as
      theme(axis.title.x = element_blank(),legend.title=element_blank(),legend.position = "right")

    # Plot p en exporteer naar PNG in de outfolder
    png(file = paste(outfolder,"/laag",laag,"/",substr(txtfile,1,nchar(txtfile)-4),".png",sep = ""),width = width,height = height, units = "cm",res=600)
    print(p)
    dev.off()
  }

}
