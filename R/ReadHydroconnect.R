#' In dit script staat de functie om csv-files in het hydroconnect format te lezen.
#'
#' @param csv naam .csv bestand met extentie
#' @return  output van deze functie is een lijst met de metadata en de data
#' @exports
read.hydroconnect=function(csv)# naam .csv bestand met extentie
  {
  # lees alle regels van CSV bestand als tekst-strings
  lines=readLines(csv)

  # Locatie van de headers in de file op basis van de zoekterm "NITGcode"
  headers=grep(pattern = "NITGCode",x = lines)[2:3]

  # Lees metadata in data.frame "metadata"
  # Eerste en laatste regel van metadata deel
  sline_m=headers[1]+2
  eline_m=headers[2]-2

  metadata=t(as.data.frame(strsplit(lines[sline_m:eline_m],",")))
  rownames(metadata)=seq(from=1,to=nrow(metadata),by=1)
  colnames(metadata)=strsplit(lines[headers[1]],",")[[1]]
  metadata=as.data.frame(metadata)
  metadata$id=paste(metadata$NITGCode,"00",metadata$FilterNo,sep = "")
  unique(metadata$id)
  metadata=metadata[!sapply(metadata, function(x) all(x == ""))]

  #Lees tijdreeksen in data.frame "data"
  # Eerste en laatste regel van data deel
  sline_d=headers[2]+2
  eline_d=length(lines)

  data=t(as.data.frame(strsplit(lines[sline_d:eline_d],",")))
  rownames(data)=seq(from=1,to=nrow(data),by=1)
  colnames(data)=strsplit(lines[headers[2]],",")[[1]]
  data=as.data.frame(data)
  data$id=paste(data$NITGCode,"00",data$FilterNo,sep = "")
  data=data[!sapply(data, function(x) all(x == ""))]


  return(list(data,metadata))
}
