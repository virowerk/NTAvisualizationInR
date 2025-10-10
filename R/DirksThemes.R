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