# Load packages required to define the pipeline:
library(targets)

# Set WD to source file ----
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble",
               "tidyverse",
               "here",
               "factoextra",
               "broom",
               "testit") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your pipeline
list(
  
  # 1. Input file path  ----
  #(only defines the file path)
  tar_target(
    InFilePath, 
    here::here("TheData", 
               "PlasmaNTABiorep123.xlsx"),
    format = "file"
  ),
  
  # 2. Load the data from file ----
  #(shared by all steps) 
  tar_target(
    TheData,
    readNTA(InFilePath,
            TheSheet = "toR")
  ),
  
  # 2. Pretty table ----
  #(shared by all steps) 
  tar_target(
    PrettyTable,
    TableByBiorep(TheData)
  ),
  
  
  # 3. graph  ----
  #(shared by all steps) 
  tar_target(
    SimpleGraph,
    vanillaPlotByCatergory(
      InFile = TheData,
      OutFile = here::here("TheOutput", 
                         "Concentration.png")
    ),
    format = "file"
  )
  
)


