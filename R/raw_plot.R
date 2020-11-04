#' Plot the aggregated data per individual
#'
#' @param data Data in a data.frame format (e.g. Output of function 'interval_aggregation')
#' @param variable Name of variable in the data set you would like to plot
#' @param ID Name of ID variable in the data set
#' @param Date Name of date variable in the data set
#' @param Time Name of time variable in the data set
#' 
#' @importFrom hms hms
#' @importFrom hms as_hms
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 geom_text
#'
#' @export

raw_plot = function(data, variable, ID, Date, Time)
{
  timediff = diff(data[,colnames(data) == Time])
  tinterval = min(timediff[timediff > 0]) # Time interval
  timeinterval = as_hms(seq(hms(00, 00, 00), hms(00, 00, 24), length = 1 + 60*60*24/as.numeric(tinterval)))
  
  IDs = unique(data[,colnames(data) == ID])
  Days = unique(data[,colnames(data) == Date])
  
  plot.data = expand.grid(IDs, Days, timeinterval)
  colnames(plot.data) = c(ID, Date, Time)
  
  plot.data = merge(plot.data, data, by = c(ID, Date, Time), all.x = TRUE)
  
  plotlist = vector("list", length(IDs))
  
  for(i in 1:length(plotlist))
  {
    plot.data.i = plot.data[plot.data[,colnames(plot.data) == ID] == IDs[i],]
    
    Date2 <- enquo(Date)
    
    plotlist[[i]] = ggplot(data = plot.data.i, aes(x = !!as.name(Time), y = !!as.name(variable), group = !!as.name(Date))) + geom_line() + geom_point() +
                              theme_bw() + facet_wrap(. ~ date, scales = "free", ncol = 5) + xlab("") + ylab("") +
      scale_x_continuous(breaks = seq(hms(0, 0, 0), hms(00, 00, 24), length = length(timeinterval)),
                         labels = substr(as_hms(seq(hms(0, 0, 0), hms(00, 00, 24), length = length(timeinterval))), 1, 5)) +
      theme(axis.text.x = element_text(angle = 90))
  }
  
  print(plotlist)
  
}
