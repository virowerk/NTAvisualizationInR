#' dirksTheme
#'
#' @param ThePlot is a ggplot object that is further modified
#'
#' @return a ggoplot object
#'
#' @examples
#'ThePlot = data.frame(x=seq(1:10),y=seq(1:10)) %>% ggplot(aes(x,y)) + geom_point() 
#'print(ThePlot)
#'print(dirksTheme(ThePlot))
dirksTheme = function(InFile,
                      OutFile){
  testit::assert("this is not a ggplot object", is_ggplot(InFile))
  TheTextSize = 14
  OutFile = InFile +
    theme_bw() +
    theme(legend.position = 'right') +
    theme(axis.text.x = element_text(size = TheTextSize, angle = 90, vjust = 0.5)) +
    theme(axis.text.y = element_text(size = TheTextSize,  angle = 0)) +
    theme(axis.title.x = element_text(size = TheTextSize, angle = 0)) +
    theme(axis.title.y = element_text(size = TheTextSize, angle = 90)) +
    theme(strip.text.x = element_text(size = TheTextSize)) +
    theme(strip.background = element_rect(colour = "white", fill = "white")) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    labs(caption = lubridate::now())
  
  return(OutFile)
  
}

nta_PlotByCatergory = function(InFile = TheData, 
                                  OutFile = OutFile)
{
  
  message(paste("The number of features or rows is:", nrow(InFile)))
  
  InFile = InFile |>
    mutate(Concentration = Concentration+1)|>
    na.omit()
  
  InFile <- InFile |>
    filter(Concentration > 0)
  
  ThePlot <- InFile |>
    mutate(Zeta = Zeta_Potential)|>
    ggplot(aes(x = Sample, 
               y = Median_Size,
               color = Zeta,
               shape = Biorep)) +
    scale_color_fermenter(palette = "BuPu",direction = 1) +
    expand_limits(y = c(25, 175))+
    geom_jitter(
                size = 5,
                alpha = 0.7,
                height  = 0,
                width = 0.2) +
    #scale_y_log10(limits = c(1e10, 1e15))+
    geom_hline(yintercept = 100, 
               linetype = "solid",
               #linewidth = 1,
               color = "darkblue") +
    labs(title = "")
  
  # Apply dirks theme
  ThePlot <- dirksTheme(ThePlot)
  
  # Ensure directory exists
  dir.create(dirname(OutFile), recursive = TRUE, showWarnings = FALSE)
  
  # Save plot to PNG
  ggplot2::ggsave(filename = OutFile, plot = ThePlot,
                  width = 8, height = 6, dpi = 300)
  message("Graph saved to: ", OutFile)
  
  invisible(OutFile)
}

