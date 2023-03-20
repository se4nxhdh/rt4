
#### evalualtion of single folder measurement campaign with metafile as excel sheet (with specs as explained in SEMACHcalc::batch_processor)

#' Evaluate
#'
#' Takes folder directory containing SEMACH raw data files (.txt) AND a metadata file called "metadata" (.xlsx).
#' The metadata file should be constructed as described in SMEACHcalc::batch_processor and contain all raw data files in the
#' folder (which the user wants to evaluate) without the .txt ending. The sheet name in the excel file has to be "Tabelle1". The function calculates flux and averages as described in SMACHcalc::batch_processor
#' for each raw file and returns values as data frame. When save == TRUE also creates csv-file called "summary" in the directory.
#' @param directory Folder directory of folder that contains data and metafile. Example: "C:/Project/Data/Folder" or "C:/Project/Data/Folder/".
#' @param save Should summary.csv be created in directory? Default == TRUE.
#' @param draw Which variables should be included in plot? Default is c("none").
#'  Other possible inputs are vector containing variable names
#'  ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"). To plot all, use c("all") instead.
#' @param save_plot Should the plot be saved? T/F Default is F.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.
#' @param tz Timezone of timestamps

evaluate <- function(directory,save=TRUE,
                     draw=c("none"),
                     save_plot=F,
                     show_plot=F,
                     draw.method="svg",
                     tz="America/Manaus") {

  if (substr(directory, nchar(directory), nchar(directory)) != "/") {
    directory <- paste(directory, "/", sep = "")
  }
  # old read-in
  # metafile <- as.data.frame(readxl::read_excel(
  #  paste(directory, "metafile.xlsx", sep = ""), # be sure to create metafile
  # cell_cols(A:D),
  #  col_types = c("text", "numeric", "numeric", "numeric", "numeric"),
  #  na = "NA"
  #))

  # new read in
  metafile <- as.data.frame(readxl::read_excel(
    paste(directory, "metafile.xlsx", sep = ""), # be sure to create metafile
    sheet = "Tabelle1",  # be sure that metafile sheet is named Tabelle1
    cellranger::cell_cols(c("A","F")), # only required cols are read in
    col_types = c("text", "numeric", "numeric", "numeric", "numeric","text"),
    na = "NA"
  ))
  metafile<-metafile[1:5]  # cropping excess col (is used because cell_cols automatically applies rm_NA for 1st and last col)

  output <-
    batch_processor(
      directory = directory,
      filenames = metafile[[1]],
      metafile = metafile,
      method = "individual",
      draw=draw,
      save_plot=save_plot,
      show_plot=show_plot,
      draw.method=draw.method,
      tz=tz
    )
  if(save==TRUE){
    write.table(output,file=paste(directory,"summary.csv",sep=""),dec=".",sep=",",row.names = F) #because excel is weired
  }
  return(output)
}
