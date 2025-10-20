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
  
  message(paste(nrow(TheData),
                " rows were read in.")
  )
  
  colnames(TheData) = make.unique(colnames(TheData))

  
  TheData <- TheData |>
    dplyr::select(
      -c(      )
    )
  
  TheData <- TheData |>
    dplyr::rename(
    )
  
  TheData <- TheData |>
    na.omit()
  message(paste(nrow(TheData))," rows after NA remove")
  
  #TheData <- TheData |>
  #  dplyr::select(Features, contains("TPM"))
  
  return(TheData)
}


TableByBiorep = function(TheData = TheData,
                         OutFile = OutFile){
  
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
    )|>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        \(x) formatC(x, format = "e", digits = 2)  
        # scientific notation, 4 significant digits
      )
    )
  
      
  print(t(TheDataSummary))
  
  # Ensure output directory exists
  dir.create(dirname(OutFile), recursive = TRUE, showWarnings = FALSE)
  
  # Write the table to file (tab-separated for readability)
  readr::write_tsv(TheDataSummary, OutFile)
  
  message(paste("Summary table written to:", OutFile))
  
  return(OutFile)
}

