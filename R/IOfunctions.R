# IO functions

readNTA = function(InFilePath, TheSheet = "toR"){
  
  
  testit::assert("No input filename", 
                 stringr::str_length(InFilePath) > 1)
  
  message(paste("The md5sum is",
                tools::md5sum(InFilePath)
  )
  )
  TheData <- readxl::read_xlsx(InFilePath, 
                               sheet = TheSheet)
  
  testit::assert("Readin failed", nrow(TheData) > 1)
  
  message(paste("There are",
                ncol(TheData),
                "columns and",
                nrow(TheData),
                "rows")
  )
  
  colnames(TheData) = make.unique(colnames(TheData))

  
  TheData <- TheData |>
    dplyr::select(
      -c(      )
    )
  
  TheData <- TheData |>
    dplyr::rename(
    )
  
  #TheData <- TheData |>
  #  dplyr::select(Features, contains("TPM"))
  
  return(TheData)
}


TableByBiorep = function(TheData){
  
  message(paste("The number of features or rows is:", nrow(TheData)))
  
  TheDataSummary = TheData |>
    group_by(Sample) |>
    summarize(
      dplyr::across(
        where(is.numeric),
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd   = \(x) sd(x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
      
  print(t(TheDataSummary))
  
  # Ensure directory exists
  # dir.create(dirname(OutFile), recursive = TRUE, showWarnings = FALSE)
  
  invisible(TheDataSummary)
}

vanillaPlotByCatergory = function(InFile = TheData, 
                                  OutFile = OutFile)
  {
  
  message(paste("The number of features or rows is:", nrow(InFile)))
  
  InFile = InFile |>
    mutate(Concentration = Concentration+1)
  
  ThePlot <- InFile |>
    ggplot(aes(x = Sample, y = Concentration)) +
    geom_jitter(col = "darkred",
               size = 5,
               alpha = 0.7,
               height  = 0,
               width = 0.2) +
    scale_fill_brewer(palette = "Set1") +
    scale_y_log10(limits = c(1e10, 1e15))+
    geom_hline(yintercept = 0, 
               linetype = "solid",
               linewidth = 1,
               color = "darkgray") +
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