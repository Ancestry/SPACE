library(shiny)
options(shiny.maxRequestSize=30*1024^2) 
#library(fields)


function(input, output, session) {
  
  #plot ranges are reactive values to allow zooming when a region is selected and double-clicked
  ranges <- reactiveValues(x = NULL, y = NULL)

  #reactive x and y limits for the selected PCs
  default_x_min <- reactive({
    min(visible_data()[, input$pc_x])     
  })
  
  default_x_max <- reactive({
    max(visible_data()[, input$pc_x]) 
  })
  
  default_y_min <- reactive({
    min(visible_data()[, input$pc_y]) 
  })
  
  default_y_max <- reactive({
    max(visible_data()[, input$pc_y]) 
  })
  
  
  #group names from file header to be used by color by, box summary, and the subset dropdowns
  group_names <- reactive({
    headers <- names(pca_data())
    
    headers[!(grepl("^PC\\d", headers) | headers %in% c('All', 'NA.', 'NA', 'Individual', 'IID'))]  
  })
   

  #PC names from file header
  PC_names <- reactive({
    headers <- names(pca_data())
    
    headers[grepl("^PC\\d", headers)]  
  })
   

  #reads the file
  pca_data <- reactive({

    inFile <- input$file1
    if (is.null(inFile))
      #inFile$datapath = "./test.pca"
      
      return()
    
    ranges$x <- NULL
    
    read.table(inFile$datapath, head = T)
  })


  
  #set the color scheme 
  color_scheme <- reactive({
    #cat(Sys.time(), 'color scheme', is.null(visible_data()), is.null(input$colorBy), input$colorBy, "\n")
	if (is.null(visible_data()) | is.null(input$colorBy))
	  return(NULL)
    
    # trouble importing fields on AWS. Use native colors for now
    #cols <- adjustcolor(tim.colors(length(unique(pca_data()[, input$colorBy]))), input$alpha)  
    #if (input$palette == 'Subset' | input$palette == 'Greyed Out') {
    #  cols <- adjustcolor(tim.colors(length(unique(visible_data()[, input$colorBy]))), input$alpha)
    #} 
    #cols

    cols <- rainbow(length(unique(pca_data()[, input$colorBy])), alpha = input$alpha)
    if (input$palette == 'Subset' | input$palette == 'Greyed Out') {
       cols <- rainbow(length(unique(visible_data()[, input$colorBy])), alpha = input$alpha)
    }
    cols
  })
  
  #set point colors
  point_colors <- reactive({
    #cat(Sys.time(), 'point colors', "\n")

      point_cols <- color_scheme()[match(visible_data()[, input$colorBy], sort(unique(pca_data()[, input$colorBy])))]
      if (input$palette == 'Subset' | input$palette == 'Greyed Out') {
        point_cols <- color_scheme()[match(visible_data()[, input$colorBy], sort(unique(visible_data()[, input$colorBy])))]
      } 
  
    point_cols
  })

  lineColors <- function(n){
    rainbow(n)
  }

  #set shape scheme
  pch_scheme <- reactive({
    #cat(Sys.time(), 'pch scheme', "\n")
      pch_vals <- c(20, 4, 17, 5, 15, 10, 18, 6, 12, 8)
      rep(pch_vals[1:input$num_pch], ceiling(nrow(visible_data()) / input$num_pch))[1:nrow(visible_data())]
   })

    
 #set point shapes
  point_pchs <- reactive({
    #cat(Sys.time(), 'point pchs', "\n")
      point_pchs <- pch_scheme()[match(visible_data()[, input$colorBy], sort(unique(pca_data()[, input$colorBy])))]
      if (input$palette == 'Subset' | input$palette == 'Greyed Out') {
        point_pchs <- pch_scheme()[match(visible_data()[, input$colorBy], sort(unique(visible_data()[, input$colorBy])))]
      } 
  
    point_pchs
  })

  # handle roation
  rotated_data <- reactive({
    rotated = pca_data()
    theta <- pi * (input$rotate/90)
    M <- matrix( c(cos(theta), -sin(theta), sin(theta), cos(theta)), 2, 2)

    # rotate the two active columns
    x = rotated[, input$pc_x]
    y = rotated[, input$pc_y]
    df = data.frame(x, y)

    r = as.matrix(df) %*% M
    rotated[, input$pc_x] = r[,1]
    rotated[, input$pc_y] = r[,2]

    # re-center plot. (reverses any zoom)
    ranges$x <- c(min(r[,1]), max(r[,1]))
    ranges$y <- c(min(r[,2]), max(r[,2]))

    rotated
  })


  #set visibility
  visible_data <- reactive({
    #cat(Sys.time(), 'visibility', is.null(input$subset_1), is.null(input$subset_2), is.null(input$region_select_1), is.null(input$region_select_2), is.null(input$subset_1) | is.null(input$subset_2) | is.null(input$region_select_1) | is.null(input$region_select_2), "\n")
	if (is.null(input$subset_1) | is.null(input$subset_2) | is.null(input$region_select_1) | is.null(input$region_select_2))
      return(NULL)
    
    rotated_data()[which(pca_data()[, input$subset_1] %in% input$region_select_1 & pca_data()[, input$subset_2] %in% input$region_select_2), ]
  })
 
  # download data
  output$export_button <- downloadHandler(
    filename = "space.out",
    content = function(file) {
      write.csv(brushedPoints(visible_data(), input$plot1_brush, input$pc_x, input$pc_y), file)
    }
  )
 
  # The main plot
  output$plot1 <- renderPlot({
    if (is.null(visible_data()) | is.null(input$subset_1) | is.null(input$subset_2) | is.null(input$colorBy) | is.null(input$pc_x) | is.null(input$pc_y))
      return(NULL)
 
    if (nrow(visible_data()) == 0)
	  return(NULL)
 
    # Set the range based on all points (not just visible)
    if (is.null(ranges$x)){
      ranges$x <-  c(default_x_min(), default_x_max())
      ranges$y <-  c(default_y_min(), default_y_max())
    }
   
    par(mar = c(5.1, 4.1, 0, 0))
   
    # PCA plot
    #if (input$plot_type == "PCA"){
    if (TRUE){
        if (input$palette == 'Greyed Out') {
            greyed_out <- !(pca_data()[, input$subset_1] %in% input$region_select_1 & pca_data()[, input$subset_2] %in% input$region_select_2)  
            plot(pca_data()[greyed_out, input$pc_x], pca_data()[greyed_out, input$pc_y], xlim=ranges$x, ylim=ranges$y, xlab=input$pc_x, ylab=input$pc_y, pch=20, col=adjustcolor('lightgrey', .1), cex=input$cex)    
            points(visible_data()[, input$pc_x], visible_data()[, input$pc_y], col=point_colors(), cex=input$cex, pch=point_pchs())    
        } else {
            plot(visible_data()[, input$pc_x], visible_data()[, input$pc_y], xlim=ranges$x, ylim=ranges$y, xlab=input$pc_x, ylab=input$pc_y, col=point_colors(), pch=point_pchs(), cex=input$cex)    
    }
        
    # Line plot
    } else {
        
        x = visible_data()[,3]
        y = visible_data()[,4]
        t = dim(visible_data())[2] # the number of data columns 
        colors = lineColors(t-3)

        # plot the first data column
        plot(x, y, type = 'l', col=colors[1], xlim=ranges$x, ylim=ranges$y, lwd = 2, xlab = names(visible_data())[3])

        # plot the remaining data columns
        for (i in 5:t){
            y = visible_data()[,i]
            lines(x, y, col=colors[i-3], lwd = 2)
        }
    }
  })         

  #resizes legend plot based on number of rows in legend assuming 12 columns.
  output$legend.ui <- renderUI({
    #cat(Sys.time(), 'resize legend', "\n")
    if (is.null(input$colorBy) | is.null(visible_data))
      return(NULL)
	
    plotOutput("legend", height = ceiling(length(unique(visible_data()[, input$colorBy])) / 40) * 35 + 20) 
  })
 
  #draws legend in its own plot
  output$legend <- renderPlot(execOnResize =T, {
    if (is.null(visible_data()) | is.null(input$colorBy))
      return(NULL)

    if (input$palette == 'Subset' & nrow(visible_data()) == 0)
      return(NULL)
  
    region_list <- sort(unique(pca_data()[, input$colorBy]))
    if (input$palette == 'Subset' | input$palette == 'Greyed Out') {
      region_list <- sort(unique(visible_data()[, input$colorBy]))
    }
   
    #if (input$plot_type == "PCA"){
    if (TRUE){
	  if(length(region_list) > 0) {
        cs = color_scheme()
        
        par(mar = c(0, 0, 0, 0))
        plot(1, type='n', xlim=c(0,10), ylim=c(0,10), xlab='', ylab='', xaxt='n', yaxt='n', bty='n')
        legend(0, 10, bty='o', sapply(region_list, substr, 1, 12), col=cs, pch=pch_scheme(), ncol=12, cex=.8, pt.cex=1.4)  
	  }
    } else {
        par(mar = c(0, 0, 0, 0))
        plot(1, type='n', xlim=c(0,10), ylim=c(0,10), xlab='', ylab='', xaxt='n', yaxt='n', bty='n')
        t = dim(visible_data())[2] - 3
        colors = lineColors(t)
        n = names(visible_data())[4:(t+3)]
        legend(0, 10, bty='o', legend=n, fill=colors, ncol=12)
    }
  })

  #toggle button 1
  observe({
    #cat(Sys.time(), 'toggle 1', "\n")
    if(is.null(pca_data()) | is.null(input$subset_1))
      return()
      
    region_list = sort(unique(pca_data()[, input$subset_1]))
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_1]), ')', sep='')

    if(input$toggle_boxes_1 == 0) {
      return(NULL) 
      }
    else if (input$toggle_boxes_1%%2 == 0) {
      updateCheckboxGroupInput(session, "region_select_1", "", choices=region_list, selected=region_list)
    }
    else {
      updateCheckboxGroupInput(session, "region_select_1", "", choices=region_list)
    }
  })


  #subset threshold 1
  observe({
    #cat("subset_thresh_1", "\n")
    if(is.null(pca_data()) | is.null(input$subset_1) | is.na(input$subset_thresh_1) | input$subset_thresh_1 <= 0)
      return()
	
    region_list = sort(unique(pca_data()[, input$subset_1]))
    group_counts <- table(pca_data()[, input$subset_1])
    
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_1]), ')', sep='')

    updateCheckboxGroupInput(session, "region_select_1", "", choices=region_list, selected=region_list[group_counts >= input$subset_thresh_1])
  })




  #toggle button 2
  observe({
    #cat(Sys.time(), 'toggle 2', "\n")
    if(is.null(pca_data()) | is.null(input$subset_1))
      return()
      
    region_list = sort(unique(pca_data()[, input$subset_2]))
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_2]), ')', sep='')

    if(input$toggle_boxes_2 == 0) {
      return(NULL) 
      }
    else if (input$toggle_boxes_2%%2 == 0) {
      updateCheckboxGroupInput(session, "region_select_2", "", choices=region_list, selected=region_list)
    }
    else {
      updateCheckboxGroupInput(session, "region_select_2", "", choices=region_list)
    }
  })


  #subset threshold 2
  observe({
    #cat("subset_thresh_2", "\n")
    if(is.null(pca_data()) | is.null(input$subset_2) | input$subset_thresh_2 <= 0 | is.na(input$subset_thresh_2) | input$subset_thresh_2 <= 0)
      return()
    
    region_list = sort(unique(pca_data()[, input$subset_2]))
    group_counts <- table(pca_data()[, input$subset_2])
    
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_2]), ')', sep='')

    updateCheckboxGroupInput(session, "region_select_2", "", choices=region_list, selected=region_list[group_counts >= input$subset_thresh_2])
  })



  #dynamically populates Color By dropdown with group names obtained from file header
  output$colorBy <- renderUI({
    #cat(Sys.time(), 'colorBy', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
	def_group <- which(group_names() == 'Region')
	if (length(def_group) == 0) {
	  def_group <- 1
	}
	
    selectInput('colorBy', 'Color By', group_names(), selected=group_names()[def_group])
  })


  #dynamically populates Bow Summary dropdown with group names (and their unique pairs) obtained from file header
  output$box_summary <- renderUI({
    #cat(Sys.time(), 'box_summary', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
	def_group <- which(group_names() == 'Population')
	if (length(def_group) == 0) {
	  def_group <- 1
	}
	
	box_summary_choices <- c(group_names(), apply(combn(group_names(), 2), 2, paste, collapse = ' - '))
	
    selectInput('box_summary', 'Box Summary', box_summary_choices, selected=box_summary_choices[def_group])
  })


  #pc x
  output$pc_x <- renderUI({
    #cat(Sys.time(), 'PC_x', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
    selectInput('pc_x', 'PC x', PC_names())
  })


  #pc y
  output$pc_y <- renderUI({
    #cat(Sys.time(), 'PC_y', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
    selectInput('pc_y', 'PC y', PC_names(), selected=PC_names()[2])
  })


  #subset_1 
  output$subset_1 <- renderUI({
    #cat(Sys.time(), 'subset_1', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
  	def_group <- which(group_names() == 'Region')
  	if (length(def_group) == 0) {
  	  def_group <- 1
  	}
      selectInput('subset_1', 'Subset 1', group_names(), selected=group_names()[def_group])
  })


  #checkbox group 1
  output$region_select_1 <- renderUI({
    #cat(Sys.time(), 'checkbox_1', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()) | is.null(input$subset_1))
      return()

    # Create the checkboxes and select them all by default
    region_list = sort(unique(pca_data()[, input$subset_1]))
    
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_1]), ')', sep='')
        
    checkboxGroupInput("region_select_1", "", 
                        choices  = region_list,
                        selected = region_list)
  })




  #subset_2 
  output$subset_2 <- renderUI({
    #cat(Sys.time(), 'subset_2', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()))
      return()
    
	def_group <- which(group_names() == 'Dataset')
	if (length(def_group) == 0) {
	  def_group <- 2
	}

    selectInput('subset_2', 'Subset 2', group_names(), selected=group_names()[def_group])
  })


  #checkbox group 2
  output$region_select_2 <- renderUI({
    #cat(Sys.time(), 'checkbox_2', "\n")
    # If missing input, return to avoid error later in function
    if(is.null(pca_data()) | is.null(input$subset_2))
      return()

    # Create the checkboxes and select them all by default
    region_list = sort(unique(pca_data()[, input$subset_2]))
    
    region_list <- as.character(region_list)
    names(region_list) <- paste(region_list, ' (', table(pca_data()[, input$subset_2]), ')', sep='')
        
    checkboxGroupInput("region_select_2", "", 
                        choices  = region_list,
                        selected = region_list)
  })


  # Make the plot brushable  
  output$brush_info <- renderDataTable({
    #cat(Sys.time(), 'brushable', "\n")
    if(is.null(input$plot1_brush) | is.null(visible_data()))
      return()
    
    brushedPoints(visible_data(), input$plot1_brush, input$pc_x, input$pc_y)
  }, options = list(lengthMenu = c(100, 1000, 10000, 100000), pageLength = 1000))
  
  
  
  #plot summary box to provide aggregate counts of selected points
  output$brush_info2 <- renderPrint({
    if(is.null(input$plot1_brush) | is.null(visible_data()))
      return()
    
    selected_points <- brushedPoints(visible_data(), input$plot1_brush, input$pc_x, input$pc_y)
    
    if (length(grep(' - ', input$box_summary)) > 0) {
      groups <- strsplit(input$box_summary, ' - ')[[1]]
      group_counts <- table(selected_points[, groups[1]], selected_points[, groups[2]])
      if (length(table(pca_data()[, groups[2]])) > length(table(pca_data()[, groups[1]])))
        {group_counts <- table(selected_points[, groups[2]], selected_points[, groups[1]])}
      
      return(group_counts[which(apply(group_counts, 1, sum) > 0), , drop=F])
    } else {
      group_counts <- table(selected_points[, input$box_summary])
      group_counts_df <- stack(group_counts)[group_counts > 0, ]
      names(group_counts_df) <- c('Points', input$box_summary)
      
      return(print(group_counts_df[with(group_counts_df, order(-group_counts_df[, 1], group_counts_df[, 2])), ], row.names=F))
    } 
  })
  
  #sample data download
  output$sampleData <- downloadHandler(
    file = "example.txt", 
    content = function(file){
        file.copy("example.txt", file)
    }
  )

  

  #zoom on double click
  observeEvent(input$plot1_dblclick, {
    #cat(Sys.time(), 'dblclick', "\n")
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <-  c(default_x_min(), default_x_max())
      ranges$y <-  c(default_y_min(), default_y_max())
    }
  })
  

  #change ranges if pc_x changed
  observeEvent(input$pc_x,{
    #cat(Sys.time(), 'observe x', "\n")
    if (!is.null(visible_data()) | is.null(input$pc_x) | is.null(input$pc_y))
      ranges$x <-  c(default_x_min(), default_x_max())
  })  


  #change ranges if pc_y changed
  observeEvent(input$pc_y,{
    #cat(Sys.time(), 'observe y', "\n")
    if (!is.null(visible_data()) | is.null(input$pc_x) | is.null(input$pc_y))
      ranges$y <-  c(default_y_min(), default_y_max())
  })  
}
