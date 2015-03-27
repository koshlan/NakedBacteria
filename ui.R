library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(em("Dehalococcoides Browser")),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      textInput("Input0", label="Enter Project Name", value = ""),
      textInput("size_bp", label="Enter genome size", value = 1500000),
      selectizeInput("genomes", label = "Choose Dehalococcoides Strain(s):", 
                  choices =c("195" = "NC_002936.3",
                             "CBDB1" = "NC_007356.1",
                             "BAV1" = "NC_009455.1",
                             "VS" = "NC_013552.1",
                             "GT" = "NC_013890.1",
                             "DCMB5" = "NC_020386.1",
                             "BTF08" = "NC_020387.1",
                             "GY50" = "NC_022964.1",
                             "CG1" = "NZ_CP006949.1",
                             "CG4" = "NZ_CP006950.1",
                             "CG5" = "NZ_CP006951.1"),
                  selected = NULL, 
                  multiple = TRUE, 
                  options = NULL
                  ),
      
      selectizeInput("keywords", label="Choose Common Gene Keywords", c("dehalogenase","^tRNA","hydrogenase","vhu","ech","integrase"), selected = NULL, 
                     multiple = TRUE, options = NULL),
      textInput("ad1", label="Enter an additional custom search terms [regex]", value = NULL),
      textInput("ad2", label="Enter an additional custom search terms [regex]", value = NULL),
      textInput("locus_tags", label="Enter locus tags of interest (seperated by commas with NO spaces)", value = NULL),
      submitButton("Submit"),
      
      sliderInput("track_width",
                  "Track Diameter:",
                  min = 5,
                  max = 16,
                  value = 14)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("announce")
    )
  )
))
