library(shiny)

ui <- fluidPage(
  titlePanel("Simple Data Cleaner"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      textInput("delimiter", "Delimiter (leave blank to auto-detect)", value = ""),
      numericInput("skip", "Skip Rows", value = 0, min = 0),
      numericInput("nrows", "Rows to Preview", value = 10, min = 1),
      checkboxInput("snake", "Rename Columns to Snake Case", FALSE),
      checkboxInput("remove_const", "Remove Constant Columns", FALSE),
      checkboxInput("remove_na", "Remove Rows with Null Values", FALSE),
      downloadButton("download", "Download Cleaned CSV")
    ),
    
    mainPanel(
      h4("Raw Data Preview"),
      tableOutput("rawTable"),
      h4("Cleaned Data Preview"),
      tableOutput("cleanedTable")
    )
  )
)

server <- function(input, output) {
  
  rawData <- reactive({
    req(input$file)
    delim <- ifelse(input$delimiter == "", ",", input$delimiter)
    read.csv(input$file$datapath, sep = delim, skip = input$skip, nrows = input$nrows)
  })
  
  fullData <- reactive({
    req(input$file)
    delim <- ifelse(input$delimiter == "", ",", input$delimiter)
    read.csv(input$file$datapath, sep = delim, skip = input$skip)
  })
  
  cleanedData <- reactive({
    df <- fullData()
    
    if (input$snake) {
      colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
    }
    
    if (input$remove_const) {
      df <- df[, sapply(df, function(x) length(unique(x)) > 1)]
    }
    
    if (input$remove_na) {
      df <- df[complete.cases(df), ]
    }
    
    df
  })
  
  output$rawTable <- renderTable({
    rawData()
  })
  
  output$cleanedTable <- renderTable({
    cleanedData()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("cleaned_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cleanedData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
