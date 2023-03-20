#'data_plot
#'
#'Plots raw SEMACH data and saves it (as svg) and / or displays it on console if desired.
#'Mostly for internal use.
#'
#' @param xx Subset of SEMACH raw data to be drawn as internally created by flux.calc. If the function is used
#' manually, pease be sure to add t_sec column, with the respective measurement time in seconds. Furthermore the columns
#' as described for draw should be included and named accordingly.
#' @param filepath Path from which data is loaded. E.g., "E:/Folder/measurement.txt"
#' @param draw Which variables should be included in plot? Default is c("all").
#' Other possible inputs are vector containing variable names.
#' ("CO2","rH_i","T_i","P_i","rH_o","T_o","P_o","PAR","WT"), or empty vector c().
#' @param save_plot Should the plot be saved? T/F Default is T.
#' @param show_plot Should the plot be drawn on console window T/F Default is F.
#' @param draw.method Which format should the plots be drawn in? Default is svg, other option is jpeg (lower res.).
#' Using c("svg","jpeg") allows to draw both formats at once.

data_plot <- function(xx,
                      filepath,
                      draw = c("all"),   # if no plotting in flux.calc is desired, externally skipping (no "empty" case required)
                      save_plot = F,
                      show_plot = T,
                      draw.method = "svg") {
  ## plotter
  require(ggplot2)
  require(plotly)
  require(dplyr)
  require(tidyr)
  require(tools)
  require(ggpubr)
  # for some reason .5 is rounded down. This give needed row amount


  if(draw=="all"){
    draw<-c("CO2",
            "T_i",
            "rH_i",
            "P_i",
            "T_o",
            "rH_o",
            "P_o",
            "PAR",
            "WT")
  }


  #### plotly new ####
  #set to F to exclude, can be used in future to implement interactive plots
  if(F){
  pivot_longer(xx,cols = all_of(draw))->y

  ggplotly(
  ggplot(y,aes(x=t_sec,y=value))+
    geom_point()+
    geom_smooth(method="lm")+
    scale_alpha_manual(breaks=draw,values=c(1,rep(0,length(draw)-1)))+
    facet_wrap(facets=vars(name),scale="free_y")
  )

  }



#### classic drawing ####


  # kind of local dictionary for ylab names and units
  unit <- list(
    "%",
    "\u00b0C",
    "hPa",
    "%",
    "\u00b0C",
    "ppm",
    "%",
    "\u00b0C",
    "hPa",
    expression(paste(mu,"mol ",m^-2," ",s^-1)),
    "\u00b0C"
  )
  names(unit)<-
    c(
      "rH_i",
      "T_i",
      "P_i",
      "SM",
      "ST",
      "CO2",
      "rH_o",
      "T_o",
      "P_o",
      "PAR",
      "WT")

  titles<-list(
    "rH in",
    expression(paste(T[air]," in")),
    "P in",
    "SM",
    "ST",
    expression(CO[2]),
    "rH out",
    expression(paste(T[air]," out")),
    "P out",
    "PAR",
    "WT"
  )
  names(titles)<-
    c(
      "rH_i",
      "T_i",
      "P_i",
      "SM",
      "ST",
      "CO2",
      "rH_o",
      "T_o",
      "P_o",
      "PAR",
      "WT")

  plotlist <- vector("list", length(draw)) #list with draw names to store individual plots (and use to create final plot later)
  names(plotlist) <- draw


  for (i in draw) {
    if (i == "CO2") { #"CO2" with glm, others with mean and loess
      slope<-paste("  ",format(lm(data = xx, CO2 ~ t_sec)$coefficients[[2]] * 60, digits = 3),
                   " ppm/min",
                   sep = "")
      plot <- ggplot(xx, aes(x = t_sec, y = .data[[i]])) +
        geom_point() +
        geom_smooth(method = "gam") +
        geom_smooth(method = "glm",color="red") +
        # geom_text(aes(                     # slope ppm/min as text in plot with positioning; maybe ajust for low or negative slopes
        #   x = .9 * max(t_sec),               # geom_text is original version, new as part of ggtitle
        #   y = quantile(CO2, .2, names = F),
        #   label = paste(format(
        #     lm(data = xx, CO2 ~ t_sec)$coefficients[[2]] * 60, digits = 3
        #   ),
        #   " ppm/min", sep = "")
        # ))
        xlab("s")+
        ylab(unit[[i]])+
        ggtitle(bquote(paste(CO[2],"  ",.(slope))))+    # bquote allows to include variable with .(), unlike expression
        theme(
          title=element_text(size=7)
        )#,axis.title = element_text(size=10))
      plotlist[[i]] <- plot
    } else{
      plot <- ggplot(xx, aes(x = t_sec, y = .data[[i]])) +
        geom_point() +
        geom_smooth(method = "gam") +
        geom_abline(slope = 0,
                    intercept = mean(xx[[i]]),
                    col = "red")+
        xlab("s")+
        ylab(unit[[i]])+
        ggtitle(titles[[i]])+
        theme(title=element_text(size=7))#,axis.title = element_text(size=10))
      plotlist[[i]] <- plot
    }
  }


  #  strtrim(basename(filepath),4) # extracting filename without .txt

  PLOT <- ggarrange(plotlist = plotlist,
                    ncol = 2,
                    nrow = round(length(draw) / 2 + .1))
  PLOT <-
    annotate_figure(
      PLOT,
      top = "",
      fig.lab = paste(
        basename(
          strtrim(
            filepath,
            nchar(filepath) - nchar(basename(filepath)
                                    )
            )
          ),
        basename(filepath)
      ),
      basename(filepath),
      fig.lab.face = "bold",
      fig.lab.size = 12
    )
  if(show_plot==T){
    plot(PLOT)
  }

  if (save_plot == T&"svg"%in%draw.method==T) {          # using %in% allows for use of charater or vector containing both
    svg(paste(
      strtrim(filepath, nchar(filepath) - nchar(basename(filepath))),
      strtrim(basename(filepath), nchar(basename(filepath))-(nchar(file_ext(filepath))+1)),
      ".svg",
      sep = ""
    ))
    plot(PLOT)
    dev.off()
  }


  if (save_plot == T&"jpeg"%in%draw.method==T) {
    jpeg(paste(
      strtrim(filepath, nchar(filepath) - nchar(basename(filepath))),
      strtrim(basename(filepath), nchar(basename(filepath))-(nchar(file_ext(filepath))+1)),
      ".jpeg",
      sep = ""
    ),quality = 100,width=600,height=800)  # a bit less compression than default
    plot(PLOT)
    dev.off()
  }
}




