
#' SEMACH_aqua_data_prep
#'
#' Converter for "new" SEMACH aqua data format in SEMACHcalc readable format
#'
#' @param directory filepath of the raw SEMACHaqua file
#' @param overwrite Should raw file be overwritten with converted structure, or should
#' converted file be saved as .../prepped_filename





SEMACH_aqua_data_prep<-function(directory,overwrite=F){
  require(readr)
  require(dplyr)

  data<-read.delim(file=directory, sep=";")
  data<-data[-1,] # rm empty row with sampling rate
  l<-length(data[[1]]) # compute length of actual data
  data_prepped<-paste(data[3:l,1],data[3:l,2],sep=".") # initialise with timestamps (minus calibration lines)
  for (i in names(data)[3:11]){ # 11 channels
    data_prepped<-data.frame(data_prepped,data[[i]][3:l]*data[[i]][1]+data[[i]][2])
  }
  names(data_prepped)<-names(data)[-2]

  if(overwrite==T){
    write.table(data_prepped,sep=";",
                file=directory,
                row.names = F)
  }else{

    dir_new<-paste0(dirname(directory),"/prepped_",basename(directory))
    #print(dir_new)
    write.table(data_prepped,sep=";",
                file=dir_new,
                row.names = F)
  }
  #return(data_prepped)
}



#' BATCH_prep
#'
#' Converter for "new" SEMACH aqua data format in SEMACHcalc readable format - apllies conversion to folder with data
#'
#' @param directory folder containing the raw SEMACHaqua file
#' @param overwrite Should raw file be overwritten with converted structure, or should
#' converted file be saved as .../prepped_filename

BATCH_prep<-function(directory,exclude=c("FIRST README.txt","Gassampler_log.txt")){
  require(stringr)
  if(str_ends(directory,"/")==F){
    directory<-paste0(directory,"/")
  }
  files<-list.files(directory)
  files<-subset(files,files%in%exclude==F)
  for (i in files){
  SEMACH_aqua_data_prep(directory = paste0(directory,i),overwrite = T)
  }
  #return(files)
}
#"C:/Users/SeanA/Desktop/Brasilien-neu/RobimoTrop_III/Kammersystem Messungen/prepped/tst3.csv"

#"C:/Users/SeanA/Desktop/Brasilien-neu/RobimoTrop_III/Kammersystem Messungen/15.09.2022/150922-161234-ADC1.csv"
