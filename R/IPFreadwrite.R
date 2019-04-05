#' functie om ipf-files in te lezen.
#'
#' @param ipf_file ipf file input
#' @param sep separator in the ipf file. By default sep is ","
#' @return returns a dataframe with the ipf data
#' @export
read.ipf=function(ipf_file,sep=",")
{
  lines = readLines(ipf_file)
  nrecords=as.double(lines[1])
  nfields=as.double(lines[2])
  fields=as.array(lines[3:(2+nfields)])
  data=read.delim(ipf_file,col.names = fields,sep = sep,skip = nfields+3, header = F,row.names = NULL)
  return(data)
}

#' functie om ipf-files weg te schrijven.
#'
#' @param x dataframe to convert to ipf file
#' @param ipf_out name of the output ipf, including extension. By default "output.ipf" is generated
#' @param txtcol column in which the ipf file refers to an ipf text file. By default this parameter is 0 (no text file)
#' @param sep separator in the ipf file. By default sep is ","
#' @return returns an ipf file
#' @export
write.ipf=function(x,ipf_out="output.ipf",txtcol=0, sep=",")
{
  sink(file=ipf_out,append = F)
  cat(nrow(x),"\n",sep = "")
  cat(ncol(x),"\n",sep = "")
  for(i in 1:ncol(x)){cat(colnames(x[i]),"\n",sep = "")}
  cat(paste(txtcol,",txt",sep = ""))
  for(i in 1:nrow(x)){
    cat("\n")
    for(j in 1:ncol(x))
    {
      cat(ifelse(j<ncol(x),paste(x[i,j],sep,sep = ""),paste(x[i,j],sep = "")))
    }
  }
  cat("\n")
  sink()
}
