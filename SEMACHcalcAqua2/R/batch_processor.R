

















#' Batch processor
#'
#' Allows quick processing of multiple SEMACH-output-files. Either default parameters (1min-5min)
#' or custom time interval and chamber dimension settings. All .txt-files have to be situated in the same folder. Returns dataframe with averages over selected measurement timeframe, CO2 accumulation rate (ppm/min) and CO2 flux (µmol m-2 s-1)
#' @param directory Path of folder containing data to be processed.
#' @param filenames Vector of actual filenames of the .txt-files to be processed. Only filename, do NOT add .txt (e.g., "filename.txt")
#' @param method Either "default" (also literally default), "fixed", or "individual". Default uses default chamber proportions (area, volume) and time intervals (1min-5min).
#' Fixed allows customisation of several or all parameters to a fixed value different to the default. Input through A_chamber, V_chamber, start and end. If no parameters are edited, use default instead.
#' Individual allows for individual parameter customisation for each .txt-file. Requires input of metadata file. If no parameters are edited, use default instead.
#' @param start Starting point of evaluation in seconds after start of recording. Used for method="fixed"
#' @param end End point of evaluation in seconds after start of recording. Used for method="fixed"
#' @param V_chamber Chamber volume in m³. Useful e.g., when chamber volume is extended by base ring. Used for method="fixed"
#' @param A_chamber Chamber base area in m². Implemented for completion, usually not changed. Used for method="fixed"
#' @param metafile data frame containing 5 columns:
#' 1 - 'filenames' As used in filenames parameter. Each measurement file has it's own parameter row.
#' 2 - 'V_chamber' Deviating chamber volume in m³ for each measurement file. If default, set to NA or enter default manually.
#' 3 - 'A_chamber' Deviating chamber area in m² for each measurement file. If default, set to NA or enter default manually.
#' 4 - 'start' Starting point of evaluation in seconds after start of recording for each measurement file. If default, set to NA or enter default manually.
#' 5 - 'end' Endpoint of evaluation in seconds after start of recording for each measurement file. If default, set to NA or enter default manually.
#' @param draw Which variables should be included in plot? Default is c("none").
#'  Other possible inputs are vector containing variable names
#'  ("rH_i","T_i","P_i","SM","ST","CO2","rH_o","T_o","P_o","PAR"). To plot all, use c("all") instead.
#' @param save_plot Should the plot be saved? T/F Default is F.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.
#' #' @param tz Timezone of timestamps
#'
batch_processor <-
  function(directory,
           #path including folder name containing raw SEMACH data (as txt) as string
           filenames,
           # vector containing filenames as string (only names, no .txt necessary)
           method = "default",
           # default uses data from 1min to 5min and default chamber dimensions
           start = NA,
           # fixed value manipulation of parameters
           end = NA,
           A_chamber = NA,
           V_chamber = NA,
           metafile,
           draw=c("none"),
           save_plot=F,
           show_plot=F,
           draw.method = "svg",
           tz="America/Manaus") {



      if(substr(directory,nchar(directory),nchar(directory))!="/"){ # adds '/' if not last character of directory string already. Skipps if user already used '/' as last character
        directory<-paste(directory,"/",sep="")
      }

      if (method == "default") { ## default #####################################################################################################

        output <- data.frame()
        for (i in filenames) {
          output <-
            rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                paste(directory, i, ".csv", sep = ""),tz = tz
            )))
        }
        names(output) <-
          c(
            "Measuring point",
            "Date",
            "Time",
            "CO2 [ppm/min]",
            "T_i [°C]",
            "P_i [hPa]",
            "rH_i [%]",
            "WT [°C]",
            "rH_o [%]",
            "T_o [°C]",
            "P_o [hPa]",
            "PAR [µmol m-2 s-1]",
            "F_CO2 [µmol m-2 s-1]",
            "V_ch [m3]",
            "A_ch [m2]",
            "start [s]",
            "end [s]"

          )
        return(output)


      } else if (method == "fixed") { ## fixed ##########################################################################################################
        #fixed val manipul.
        output <- data.frame()
        if (is.na(A_chamber) == F &
            is.na(V_chamber) == F & is.na(start) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               V_chamber = V_chamber,
                               start = start,
                               end = end,
                               tz=tz
                )
              ))
          }
        } else if (is.na(V_chamber) == F &
                   is.na(start) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               V_chamber = V_chamber,
                               start = start,
                               end = end,
                               tz=tz
                )
              ))
          }
        } else if (is.na(A_chamber) == F &
                   is.na(start) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               start = start,
                               end = end,
                               tz=tz
                )
              ))
          }
        } else if (is.na(A_chamber) == F &
                   is.na(V_chamber) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               V_chamber = V_chamber,
                               end = end,
                               tz=tz
                )
              ))
          }
        } else if (is.na(A_chamber) == F &
                   is.na(V_chamber) == F & is.na(start) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               V_chamber = V_chamber,
                               start = start,
                               tz=tz
                )
              ))
          }
        } else if (is.na(start) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                start = start,
                                                end = end,
                                                tz=tz
              )))
          }
        } else if (is.na(A_chamber) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                A_chamber = A_chamber,
                                                end = end,
                                                tz=tz
              )))
          }
        } else if (is.na(A_chamber) == F & is.na(V_chamber) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               V_chamber = V_chamber,
                               tz=tz
                )
              ))
          }
        } else if (is.na(V_chamber) == F & is.na(start) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               V_chamber = V_chamber,
                               start = start,
                               tz=tz
                )
              ))
          }
        } else if (is.na(A_chamber) == F & is.na(start) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = A_chamber,
                               start = start,
                               tz=tz
                )
              ))
          }
        } else if (is.na(V_chamber) == F & is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                V_chamber = V_chamber,
                                                end = end,
                                                tz=tz
              )))
          }
        } else if (is.na(A_chamber) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), A_chamber = A_chamber, tz=tz
              )))
          }
        } else if (is.na(V_chamber) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), V_chamber = V_chamber, tz=tz
              )))
          }
        } else if (is.na(start) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), start = start, tz=tz
              )))
          }
        } else if (is.na(end) == F) {
          for (i in filenames) {
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), end = end, tz=tz
              )))
          }
        } else{
          cat("Error, Method 'fixed' invalid.\nPlease use method 'default' or enter parameters.\n")
        }
        names(output) <-
          c(
            "Measuring point",
            "Date",
            "Time",
            "CO2 [ppm/min]",
            "T_i [°C]",
            "P_i [hPa]",
            "rH_i [%]",
            "WT [°C]",
            "rH_o [%]",
            "T_o [°C]",
            "P_o [hPa]",
            "PAR [µmol m-2 s-1]",
            "F_CO2 [µmol m-2 s-1]",
            "V_ch [m3]",
            "A_ch [m2]",
            "start [s]",
            "end [s]"

          )
        return(output)


      } else if (method == "individual") { ## individual #################################################################################################################################
        # specified vals for each file
        output <- data.frame()

        for (i in filenames) {
          j<-as.numeric(row.names(metafile[which(metafile[1]==i),]))
          if (is.na(metafile[j,3]) == F &
              is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
            cat("A, V, start, and end given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               V_chamber = metafile[j,2],
                               start = metafile[j,4],
                               end = metafile[j,5],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,2]) == F &
                     is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
            cat("V, start, and end given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               V_chamber = metafile[j,2],
                               start = metafile[j,4],
                               end = metafile[j,5],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,3]) == F &
                     is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
            cat("A, start, and end given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               start = metafile[j,4],
                               end = metafile[j,5],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,3]) == F &
                     is.na(metafile[j,2]) == F & is.na(metafile[j,5]) == F) {
            cat("A, V, and end given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               V_chamber = metafile[j,2],
                               end = metafile[j,5],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,3]) == F &
                     is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F) {
            cat("A, V, and start given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               V_chamber = metafile[j,2],
                               start = metafile[j,4],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,4]) == F & is.na(metafile[j,5]) == F) {
            cat("start and end given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                start = metafile[j,4],
                                                end = metafile[j,5],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,5]) == F) {
            cat("A and end given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                A_chamber = metafile[j,3],
                                                end = metafile[j,5],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,2]) == F) {
            cat("A and V given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               V_chamber = metafile[j,2],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,2]) == F & is.na(metafile[j,4]) == F) {
            cat("V and start given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               V_chamber = metafile[j,2],
                               start = metafile[j,4],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,3]) == F & is.na(metafile[j,4]) == F) {
            cat("A and start given\n")
            output <-
              rbind(output, c(
                i,
                flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                 paste(directory,  i, ".csv", sep = ""),
                               A_chamber = metafile[j,3],
                               start = metafile[j,4],
                               tz=tz
                )
              ))

          } else if (is.na(metafile[j,2]) == F & is.na(metafile[j,5]) == F) {
            cat("V and end given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""),
                                                V_chamber = metafile[j,2],
                                                end = metafile[j,5],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,3]) == F) {
            cat("A given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), A_chamber = metafile[j,3],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,2]) == F) {
            cat("V given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), V_chamber = metafile[j,2],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,4]) == F) {
            cat("start given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), start = metafile[j,4],
                                                tz=tz
              )))

          } else if (is.na(metafile[j,5]) == F) {
            cat("end given\n")
            output <-
              rbind(output, c(i, flux_calc_aqua(draw.method=draw.method,draw=draw, save_plot=save_plot, show_plot=show_plot, filepath=
                                                  paste(directory,  i, ".csv", sep = ""), end = metafile[j,5],
                                                tz=tz
              )))

          } else{
            cat("Error, Method 'individual' is invalid.\nPlease use method 'default' or enter parameters.\n")
          }
        }

        names(output) <-
          c(
            "Measuring point",
            "Date",
            "Time",
            "CO2 [ppm/min]",
            "T_i [°C]",
            "P_i [hPa]",
            "rH_i [%]",
            "WT [°C]",
            "rH_o [%]",
            "T_o [°C]",
            "P_o [hPa]",
            "PAR [µmol m-2 s-1]",
            "F_CO2 [µmol m-2 s-1]",
            "V_ch [m3]",
            "A_ch [m2]",
            "start [s]",
            "end [s]"

          )
        return(output)

      } else{
        #wrong method
        print("Error, Method invalid.")
        return()
      }
  }








