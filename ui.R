library(shiny)

fluidPage(
  titlePanel("_SPACE_"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose PCA File'),
      downloadButton("export_button", "Export Data"),
      #radioButtons("plot_type", "Plot Type", c("PCA", "Line")),
      tags$hr(),
      fluidRow(
        column(6, uiOutput('colorBy')),
        column(6, uiOutput('box_summary'))
      ),
      fluidRow(
        column(4, selectInput('palette', 'Color Palette', c('Subset', 'Greyed Out', 'All'))),
        column(4, uiOutput('pc_x')),
        column(4, uiOutput('pc_y'))
      ), 
      fluidRow(
        column(6, sliderInput('cex', 'Dot size', min = .1, max=3, value = 1)),
        column(6, sliderInput('alpha', 'Transparency', min = .1, max=1, value = 1))
      ),
      fluidRow(
        column(6, sliderInput('num_pch', 'Shapes', min = 1, max=10, value = 1, step = 1)),
        column(6, sliderInput('rotate', 'Rotate', min = 1, max=180, value = 1, step = 1))
      ), 
      tags$hr(),
      fluidRow(
        column(6, uiOutput('subset_1')),
        column(6, uiOutput('subset_2'))
      ),
      fluidRow(
        column(3, 
          br(),
          actionButton("toggle_boxes_1", "Toggle")
        ), 
        column(3, numericInput('subset_thresh_1', label = "Threshold", value = "0")), 
        column(3, 
          br(),
          actionButton("toggle_boxes_2", "Toggle")
        ), 
        column(3, numericInput('subset_thresh_2', label = "Threshold", value = "0"))
      ), 
      fluidRow(
        column(6, uiOutput('region_select_1')),
        column(6, uiOutput('region_select_2'))
      ),
      downloadLink("sampleData", label = "example.txt")
    ),
    
    mainPanel(
      plotOutput('plot1', 
        height = "600px",
        dblclick = "plot1_dblclick",
        brush = brushOpts(
          id = "plot1_brush"
        )),
      uiOutput('legend.ui'),
      fluidRow(
        tags$head(tags$style(type="text/css", 
                  '#brush_info {font-size: 8px;}'
                            )
                 ),
         br(),
      verbatimTextOutput("brush_info2")
      ,
      fluidRow(tags$head(tags$style(type="text/css", 
                         '#brush_info {font-size: 12px;}'
                                   )
                        ),
               dataTableOutput("brush_info"))         
      )
    )
  )
)

