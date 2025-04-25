
#Load necessary packages.
library(shiny)
library(shinyjs)
library(EBImage)
library(jpeg)
library(magick)
library(ggplot2)
library(writexl)
library(shinydashboard)
library(dplyr)
library(tibble)
library(ComplexHeatmap)
library(grid)
library(gridExtra)
library(cowplot)
library(DT)
library(waiter)
library(viridis)
library(slingshot)
library(shinycssloaders)
library(plotly)


#Server function.
server <- function(input, output) {
  
  #### Placeholders #####################################################################################################################################
  # All these lines loads example images as placeholder for each page.
 
  #RGC Quantification #1 placeholder.
   output$pdf <- renderPlot({ 
    rbpms <- readImage("RBPMS.tif")
    plot(rbpms)
  })
  
  #RGC Quantification #2 placeholder.
  output$pdf_1 <- renderPlot({ 
    rbpms <- readImage("RBPMS.tif")
    plot(rbpms)
  })
  
  #Total Mean Fluorescence Intensity (MFI) Quantification placeholder.
  output$pdf_2 <- renderPlot({ 
    PNS <- readImage("PNS.tif")
    plot(PNS)
  })
  
  #Normalized Mean Fluorescence Intensity (MFI) Quantification -> SCG10 Example placeholder.
  output$pdf_3 <- renderPlot({ 
    PNS_2 <- readImage("PNS_2.tif")
    plot(PNS_2)
  })
  
  #Normalized Mean Fluorescence Intensity (MFI) Quantification -> Hoechst Example placeholder. 
  output$pdf_4 <- renderPlot({ 
    PNS_3 <- readImage("PNS_2_DAPI.tif")
    plot(PNS_3)
  })
  
  #Normalized Mean Fluorescence Intensity (MFI) Quantification -> SCG10 Example placeholder.
  output$pdf_33 <- renderPlot({ 
    PNS_2 <- readImage("PNS_2.tif")
    plot(PNS_2)
  })
  
  #Total Axon Quantification and Total Axon Quantification for Multiple Files placeholder.
  output$pdf_5 <- renderPlot({ 
    ON <- readImage("Optic_Nerve.tif")
    plot(ON)
  })
  
  #Normalized Axon Quantification -> CTB Example placeholder.
  output$pdf_6 <- renderPlot({ 
    ON_2 <- readImage("Optic_Nerve_2.tif")
    plot(ON_2)
  })
  
  #Normalized Axon Quantification -> Hoechst Example placeholder.
  output$pdf_7 <- renderPlot({ 
    ON_3 <- readImage("Optic_Nerve_2_Hoechst.tif")
    plot(ON_3)
  })
  
  #Total Axon Quantification and Total Axon Quantification for Multiple Files #2 placeholder.
  output$pdf_8 <- renderPlot({ 
    ON <- readImage("Optic_Nerve.tif")
    plot(ON)
  })
  
  #### Total Axon Quantification Tab #####################################################################################################################################
  
  # This resets all the images and the table that were created by the program for Total Axon Quantification.
  observeEvent(input$reset, {
    
    output$img <- renderPlot({
    })
    output$mask <- renderPlot({
    })
    output$obj <- renderPlot({
    })
    output$int_1 <- renderPlot({
    })
    output$int_2 <- renderPlot({
    })
    output$int_3 <- renderPlot({
    })
    output$int_4 <- renderPlot({
    })
    output$results <- renderDataTable({
    })
  })
  
  #This describes what is going on in the program when clicking on submit.
  observeEvent(input$submit, {
    
    #Helper function to calculate the mask and features.
    process_image <- function(image){
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(image, kern)
      nmask <- thresh(eidilat, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_features <- data.frame(computeFeatures.shape(nmask))
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      nmask_feature_moment <- data.frame(nmask_features$s.area, nmask_moments$m.eccentricity)
      small_objects  <- which(nmask_feature_moment[,"nmask_features.s.area"] < 400)
      cleaned_mask <- rmObjects(nmask, small_objects)
      cleaned_mask
    }
    
    #Function to colorize image.
    colorize_image <- function(mask){
      cols <- c('black', sample(rainbow(max(mask))))
      colored_img <- Image(cols[1 + mask], dim = dim(mask))
      colored_img
    }
    
    #Reactive to create initial image.
    upload_image <- reactive({
      validate(need(input$file1, "Please upload an image."))
      readImage(input$file1$datapath)
    })
    
    # Reactive to create the mask.
    upload_mask <- reactive({
      process_image(upload_image())
    })
    
    #Reactive to create the colorized mask.
    upload_obj <- reactive({
      colorize_image(upload_mask())
    })
    
    #Reactive to create first interval colored image.
    upload_int_1 <- reactive({
      nmask <- upload_mask()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image()[1:x_2,]
      zero_img <- upload_mask()[x_2_2_2:x_2_2_2_2,]
      zero_stats <- data.frame(computeFeatures.shape(zero_img))
      zero_obj <- nrow(zero_stats)
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      zero_img_color
    })
    
    #Reactive to create second interval colored image.
    upload_int_2 <- reactive({
      nmask <- upload_mask()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image()[x_2:y_2,]
      second_img <- upload_mask()[y_2_2_2:y_2_2_2_2,]
      second_stats <- data.frame(computeFeatures.shape(second_img))
      second_obj <- nrow(second_stats)
      cols = c('black', sample(rainbow(max(second_img))))
      Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      Second_img_color
    })
    
    #Reactive to create third interval colored image.
    upload_int_3 <- reactive({
      nmask <- upload_mask()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image()[y_2:z_2,]
      third_img <- upload_mask()[z_2_2_2:z_2_2_2_2,]
      third_stats <- data.frame(computeFeatures.shape(third_img))
      third_obj <- nrow(third_stats)
      cols = c('black', sample(rainbow(max(third_img))))
      Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      Third_img_color
    })
    
    #Reactive to create fourth interval colored image.
    upload_int_4 <- reactive({
      nmask <- upload_mask()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image()[z_2:zz_2,]
      fourth_img <- upload_mask()[zz_2_2_2:zz_2,]
      fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
      fourth_obj <- nrow(fourth_stats)
      cols = c('black', sample(rainbow(max(fourth_img))))
      Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      Fourth_img_color
    })
    
    #Reactive to quantify amount of axons at each interval.
    
    #First interval colored image.
    upload_results <- reactive({
      nmask <- upload_mask()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image()[1:x_2,]
      zero_img <- upload_mask()[x_2_2_2:x_2_2_2_2,]
      zero_stats <- data.frame(computeFeatures.shape(zero_img))
      zero_obj <- nrow(zero_stats)
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      zero_img_color
      
      #Second interval colored image.
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image()[x_2:y_2,]
      second_img <- upload_mask()[y_2_2_2:y_2_2_2_2,]
      second_stats <- data.frame(computeFeatures.shape(second_img))
      second_obj <- nrow(second_stats)
      cols = c('black', sample(rainbow(max(second_img))))
      Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      Second_img_color
      
      #Third interval colored image.
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image()[y_2:z_2,]
      third_img <- upload_mask()[z_2_2_2:z_2_2_2_2,]
      third_stats <- data.frame(computeFeatures.shape(third_img))
      third_obj <- nrow(third_stats)
      cols = c('black', sample(rainbow(max(third_img))))
      Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      Third_img_color
      
      #Fourth interval colored image.
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image()[z_2:zz_2,]
      fourth_img <- upload_mask()[zz_2_2_2:zz_2,]
      fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
      fourth_obj <- nrow(fourth_stats)
      cols = c('black', sample(rainbow(max(fourth_img))))
      Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      Fourth_img_color
      
      
      #Quantification of axons at each interval.
      num_axon_section_first_int <- as.integer(round(zero_obj))
      num_axon_section_second_int <- as.integer(round(second_obj))
      num_axon_section_third_int <- as.integer(round(third_obj))
      num_axon_section_fourth_int <- as.integer(round(fourth_obj))
      
      #Convert axons per section to axons per nerve.
      num_axon_nerve_first_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_first_int/0.3)/0.014))
      num_axon_nerve_second_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_second_int/0.3)/0.014))
      num_axon_nerve_third_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_third_int/0.3)/0.014))
      num_axon_nerve_fourth_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_fourth_int/0.3)/0.014))
      
      #Create table that contains all the quantification information to display. 
      Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
      Number_axons_per_section <- c(num_axon_section_first_int, num_axon_section_second_int, num_axon_section_third_int, num_axon_section_fourth_int)
      Number_axons_per_nerve <- c(num_axon_nerve_first_int, num_axon_nerve_second_int, num_axon_nerve_third_int, num_axon_nerve_fourth_int)
      data.frame(Distance_from_Injury_Site, Number_axons_per_section, Number_axons_per_nerve)
    })
    
    #Output to plot entire initial image.
    output$img <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_image())
    })
    
    #Output to plot entire initial mask.
    output$mask <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_mask())
    })
    
    #Output to plot entire initial object-identified image.
    output$obj <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_obj())
    })
    
    #Output to plot first interval image.
    output$int_1 <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_int_1())
    })
    
    #Output to plot second interval image.
    output$int_2 <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_int_2())
    })
    
    #Output to plot third interval image.
    output$int_3 <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_int_3())
    })
    
    #Output to plot fourth interval image.
    output$int_4 <- renderPlot({ 
      validate(need(input$submit, "Please upload an image."))
      plot(upload_int_4())
    })
    
    #Output to plot data table with quantification information.
    output$results <- renderDataTable({ 
      validate(need(input$submit, "Please upload an image."))
      datatable(
      upload_results(),
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 4,  # Show 4 rows per page
          autoWidth = FALSE, # Automatically adjust column widths
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )
    })
  })

#Code below is the repeat of the output code above, but is necessary for the Shiny to function properly.
  output$img <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_image())
  })
  
  output$mask <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_mask())
  })
  
  output$obj <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_obj())
  s})
  
  output$int_1 <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_int_1())
  })
  
  output$int_2 <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_int_2())
  })
  
  output$int_3 <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_int_3())
  })
  
  output$int_4 <- renderPlot({ 
    validate(need(input$submit, "Please upload an image."))
    plot(upload_int_4())
  })
  
  output$results <- renderDataTable({ 
    validate(need(input$submit, "Please upload an image."))
    datatable(
      upload_results(),
      
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Total Axon Quantification for Multiple Files Tab #####################################################################################################################################
  #Helper functions
  #Function to create mask of original uploaded image.
  upload_mask <- function(image_to_process) {
    upload_image <- image_to_process
    kern <- makeBrush(5, shape='gaussian')
    eidilat <- dilate(upload_image, kern)
    nmask <- eidilat
    nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
    nmask <- opening(nmask, makeBrush(1, shape='line'))
    nmask <- bwlabel(nmask)
    nmask_features <- data.frame(computeFeatures.shape(nmask))
    nmask_moments <- data.frame(computeFeatures.moment(nmask))
    nmask_feature_moment <- data.frame(nmask_features$s.area, nmask_moments$m.eccentricity)
    sel  <- which(nmask_feature_moment[, "nmask_features.s.area"] < 400)
    xe <- rmObjects(nmask, sel)
    xe
  }
  
  #Function to quantify multiple images at once at four intervals and create a data table output with the results. 
  upload_results <- function(image_to_process, image_name) {
    nmask <- upload_mask(image_to_process)
    dim(nmask)
    width <- dim(nmask)[1]
    height <- dim(nmask)[2]
    x_2 <- width/4
    x_2_2 <- x_2/6
    x_2_2_2 <- x_2 - x_2_2
    x_2_2_2_2 <- x_2 + x_2_2
    p1x2 <- image_to_process[1:x_2,]
    zero_img <- upload_mask(image_to_process)[x_2_2_2:x_2_2_2_2,]
    zero_stats <- data.frame(computeFeatures.shape(zero_img))
    zero_obj <- nrow(zero_stats)
    cols = c('black', sample(rainbow(max(zero_img))))
    zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
    zero_img_color
    x_2 <- width/4
    y_2 <- width/2
    y_2_2 <- (y_2 - x_2)/6
    y_2_2_2 <- y_2 - y_2_2
    y_2_2_2_2 <- y_2 + y_2_2
    p2y2 <- image_to_process[x_2:y_2,]
    second_img <- upload_mask(image_to_process)[y_2_2_2:y_2_2_2_2,]
    second_stats <- data.frame(computeFeatures.shape(second_img))
    second_obj <- nrow(second_stats)
    cols = c('black', sample(rainbow(max(second_img))))
    Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
    Second_img_color
    y_2 <- width/2
    z_2 <- (3*width/4)
    z_2_2 <- (z_2 - y_2)/6
    z_2_2_2 <- z_2 - z_2_2
    z_2_2_2_2 <- z_2 + z_2_2
    p3z2 <- image_to_process[y_2:z_2,]
    third_img <- upload_mask(image_to_process)[z_2_2_2:z_2_2_2_2,]
    third_stats <- data.frame(computeFeatures.shape(third_img))
    third_obj <- nrow(third_stats)
    cols = c('black', sample(rainbow(max(third_img))))
    Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
    Third_img_color
    z_2 <- (3*width/4)
    zz_2 <- (width)
    zz_2_2 <- (zz_2-z_2)/6
    zz_2_2_2 <- zz_2 - zz_2_2
    p4zz2 <- image_to_process[z_2:zz_2,]
    fourth_img <- upload_mask(image_to_process)[zz_2_2_2:zz_2,]
    fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
    fourth_obj <- nrow(fourth_stats)
    cols = c('black', sample(rainbow(max(fourth_img))))
    Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
    Fourth_img_color
    num_axon_section_first_int <- as.integer(round(zero_obj))
    num_axon_section_second_int <- as.integer(round(second_obj))
    num_axon_section_third_int <- as.integer(round(third_obj))
    num_axon_section_fourth_int <- as.integer(round(fourth_obj))
    num_axon_nerve_first_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_first_int/0.3)/0.014))
    num_axon_nerve_second_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_second_int/0.3)/0.014))
    num_axon_nerve_third_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_third_int/0.3)/0.014))
    num_axon_nerve_fourth_int <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_section_fourth_int/0.3)/0.014))
    Number_axons_per_section <- c(num_axon_section_first_int, num_axon_section_second_int, num_axon_section_third_int, num_axon_section_fourth_int)
    Number_axons_per_nerve <- c(num_axon_nerve_first_int, num_axon_nerve_second_int, num_axon_nerve_third_int, num_axon_nerve_fourth_int)
    combined_numbers <- cbind(Number_axons_per_section, Number_axons_per_nerve)
    colnames(combined_numbers) <- c(
    paste0("Number_axons_per_section_", image_name),
    paste0("Number_axons_per_nerve_", image_name))
    combined_numbers <- as.data.frame(combined_numbers)
    return(combined_numbers)
  }
  
  #Observe the submit button click event.
    output_table_cj <- eventReactive(input$submit_multiple_files, {
        req(input$list_multiple_files_axon_quantification)  # Only proceed if files are uploaded.
      ## Initialize two lists to store the new images and their name.
    new_images <- list()
    new_names <- list()
    
    #Loop through each uploaded file to get the image and its name.
    for (i in seq_along(input$list_multiple_files_axon_quantification$datapath)) {
      #Read each uploaded image and their name.
      new_image <- readImage(input$list_multiple_files_axon_quantification$datapath[i])
      new_name <- input$list_multiple_files_axon_quantification$name[i]
      #Add the image and name to the list of new images and names, respectively.
      new_images[[i]] <- new_image
      new_names[[i]] <- new_name
    }
  
    #Creates the table with all the values of interest.
      images <- new_images
      image_names <- new_names
    
    #Ensures images have been uploaded.
    req(images, length(images) > 0)
    
    #Creates a dataframe with 4 rows to which we will append our data.
    Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
    result_table <- data.frame(distance_injury_site = Distance_from_Injury_Site)
    
    #Creates a loading bar.
    withProgress(message = 'Processing...', value = 0, {
      
      #Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
      steps <- length(images)
      counter <- as.integer(1)
      
      #Generate table for each image in the list.
      for (image in images) {
        # Increments the progress bar.
        incProgress(1/steps, detail = paste("Image", counter, "of", steps))
        # Processes the image and creates the table.
        results_current <- upload_results(image, image_names[counter])
        # Add the created table to the final table.
        result_table <- cbind(result_table, results_current)
        counter <- counter + 1
      }
    })
        result_table
  })
    output$results_multiple_files <- renderDT({
    #Use datatable() to display and make the table interactive.
    datatable(
        ##result_table,
        output_table_cj(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Normalized Axon Quantification Tab #####################################################################################################################################
  
  #Reset button function to reset all images and data output on page. 
  observeEvent(input$reset_4, {
    output$img_4 <- renderPlot({
    })
    output$mask_4 <- renderPlot({
    })
    output$obj_4 <- renderPlot({
    })
    output$int_1_4 <- renderPlot({
    })
    output$int_2_4 <- renderPlot({
    })
    output$int_3_4 <- renderPlot({
    })
    output$int_4_4 <- renderPlot({
    })
    output$img_5 <- renderPlot({
    })
    output$mask_5 <- renderPlot({
    })
    output$obj_5 <- renderPlot({
    })
    output$int_1_5 <- renderPlot({
    })
    output$int_2_5 <- renderPlot({
    })
    output$int_3_5 <- renderPlot({
    })
    output$int_4_5 <- renderPlot({
    })
    output$results_4 <- renderDataTable({
    })
  })
  
  #Function for the submit button to process and quantify uploaded images. 
  observeEvent(input$submit_4, {
    
    #Upload original image.
    upload_image_4 <- reactive({
      readImage(input$file4$datapath)
    })
    
    #Create mask from original image.
    upload_mask_4 <- reactive({
      upload_image_4 <- upload_image_4()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(upload_image_4, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_features <- data.frame(computeFeatures.shape(nmask))
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      nmask_feature_moment <- data.frame(nmask_features$s.area, nmask_moments$m.eccentricity)
      sel  <- which(nmask_feature_moment[, "nmask_features.s.area"] < 400)
      xe <- rmObjects(nmask, sel)
      xe
    })
    
    #Create object-identified image from mask image. 
    upload_obj_4 <- reactive({
      xe <- upload_mask_4()
      cols <- c('black', sample(rainbow(max(xe))))
      zrainbow <- Image(cols[1+xe], dim=dim(xe))
      zrainbow
    })
    
    #Create first interval image based on mask.
    upload_int_1_4 <- reactive({
      nmask <- upload_mask_4()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_4()[1:x_2,]
      zero_img <- upload_mask_4()[x_2_2_2:x_2_2_2_2,]
      zero_stats <- data.frame(computeFeatures.shape(zero_img))
      zero_obj <- nrow(zero_stats)
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      zero_img_color
    })
    
    #Create second interval image based on mask.
    upload_int_2_4 <- reactive({
      nmask <- upload_mask_4()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_4()[x_2:y_2,]
      second_img <- upload_mask_4()[y_2_2_2:y_2_2_2_2,]
      second_stats <- data.frame(computeFeatures.shape(second_img))
      second_obj <- nrow(second_stats)
      cols = c('black', sample(rainbow(max(second_img))))
      Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      Second_img_color
    })
    
    #Create third interval image based on mask.
    upload_int_3_4 <- reactive({
      nmask <- upload_mask_4()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_4()[y_2:z_2,]
      third_img <- upload_mask_4()[z_2_2_2:z_2_2_2_2,]
      third_stats <- data.frame(computeFeatures.shape(third_img))
      third_obj <- nrow(third_stats)
      cols = c('black', sample(rainbow(max(third_img))))
      Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      Third_img_color
    })
    
    #Create fourth interval image based on mask.
    upload_int_4_4 <- reactive({
      nmask <- upload_mask_4()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_4()[z_2:zz_2,]
      fourth_img <- upload_mask_4()[zz_2_2_2:zz_2,]
      fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
      fourth_obj <- nrow(fourth_stats)
      cols = c('black', sample(rainbow(max(fourth_img))))
      Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      Fourth_img_color
    })
    
    #Output the original image. 
    output$img_4 <- renderPlot({ 
      plot(upload_image_4())
    })
    
    #Output the masked image. 
    output$mask_4 <- renderPlot({ 
      plot(upload_mask_4())
    })
    
    #Output the original image. 
    output$obj_4 <- renderPlot({ 
      plot(upload_obj_4())
    })
    
    #Output the first interval image. 
    output$int_1_4 <- renderPlot({ 
      plot(upload_int_1_4())
    })
    
    #Output the second interval image. 
    output$int_2_4 <- renderPlot({ 
      plot(upload_int_2_4())
    })
    
    #Output the third interval image. 
    output$int_3_4 <- renderPlot({ 
      plot(upload_int_3_4())
    })
    
    #Output the fourth interval image. 
    output$int_4_4 <- renderPlot({ 
      plot(upload_int_4_4())
    })
    
    #Upload the original nuclear-stained image. 
    upload_image_5 <- reactive({
      readImage(input$file5$datapath)
    })
    
    #Create the mask from the original nuclear-stained image.
    upload_mask_5 <- reactive({
      upload_image_5 <- upload_image_5()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(upload_image_5, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask
    })
    
    #Create the object-identified image from the mask.
    upload_obj_5 <- reactive({
      xe <- upload_mask_5()
      cols <- c('black', sample(rainbow(max(xe))))
      zrainbow <- Image(cols[1+xe], dim=dim(xe))
      zrainbow
    })
    
    #Create the first interval image based on the mask of the nuclear-stained image.
    upload_int_1_5 <- reactive({
      nmask <- upload_mask_5()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_5()[1:x_2,]
      zero_img <- upload_mask_5()[x_2_2_2:x_2_2_2_2,]
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      zero_img_color
    })
    
    #Create the second interval image based on the mask of the nuclear-stained image.
    upload_int_2_5 <- reactive({
      nmask <- upload_mask_5()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_5()[x_2:y_2,]
      second_img <- upload_mask_5()[y_2_2_2:y_2_2_2_2,]
      cols = c('black', sample(rainbow(max(second_img))))
      Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      Second_img_color
    })
    
    #Create the third interval image based on the mask of the nuclear-stained image.
    upload_int_3_5 <- reactive({
      nmask <- upload_mask_5()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_5()[y_2:z_2,]
      third_img <- upload_mask_5()[z_2_2_2:z_2_2_2_2,]
      cols = c('black', sample(rainbow(max(third_img))))
      Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      Third_img_color
    })
    
    #Create the fourth interval image based on the mask of the nuclear-stained image.
    upload_int_4_5 <- reactive({
      nmask <- upload_mask_5()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_5()[z_2:zz_2,]
      fourth_img <- upload_mask_5()[zz_2_2_2:zz_2,]
      cols = c('black', sample(rainbow(max(fourth_img))))
      Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      Fourth_img_color
    })
    
    #Output the original nuclear-stained image.
    output$img_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_image_5())
    })
    
    #Output the masked nuclear-stained image.
    output$mask_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_mask_5())
    })
    
    #Output the object-identified nuclear-stained image.
    output$obj_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_obj_5())
    })
    
    #Output the first interval of the nuclear-stained image.
    output$int_1_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_1_5())
    })
    
    #Output the second interval of the nuclear-stained image.
    output$int_2_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_2_5())
    })
    
    #Output the third interval of the nuclear-stained image.
    output$int_3_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_3_5())
    })
    
    #Output the fourth interval of the nuclear-stained image.
    output$int_4_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_4_5())
    })
    
    #Below repeats the above output code, which is essential to the Shiny to function properly. 
    output$img_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_image_5())
    })
    
    output$mask_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_mask_5())
    })
    
    output$obj_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_obj_5())
    })
    
    
    output$int_1_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_1_5())
    })
    
    output$int_2_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_2_5())
    })
    
    output$int_3_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_3_5())
    })
    
    output$int_4_5 <- renderPlot({ 
      validate(need(input$submit_4, "Please upload an image."))
      plot(upload_int_4_5())
    })
    
    #Reactive function to quantify the number of axons at each interval and normalize this to the nerve width, and then create a data output table with the results.
    upload_results_4 <- reactive({
      #Upload original image and create object-identified image. 
      #First interval processing.
      validate(need(input$submit_4, "Please upload an image."))
      nmask <- upload_mask_4()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_4()[1:x_2,]
      zero_img <- upload_mask_4()[x_2_2_2:x_2_2_2_2,]
      zero_stats <- data.frame(computeFeatures.shape(zero_img))
      zero_obj <- nrow(zero_stats)
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      #Second interval processing.
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_4()[x_2:y_2,]
      second_img <- upload_mask_4()[y_2_2_2:y_2_2_2_2,]
      second_stats <- data.frame(computeFeatures.shape(second_img))
      second_obj <- nrow(second_stats)
      cols = c('black', sample(rainbow(max(second_img))))
      Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      #Third interval processing.
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_4()[y_2:z_2,]
      third_img <- upload_mask_4()[z_2_2_2:z_2_2_2_2,]
      third_stats <- data.frame(computeFeatures.shape(third_img))
      third_obj <- nrow(third_stats)
      cols = c('black', sample(rainbow(max(third_img))))
      Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      #Fourth interval processing.
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_4()[z_2:zz_2,]
      fourth_img <- upload_mask_4()[zz_2_2_2:zz_2,]
      fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
      fourth_obj <- nrow(fourth_stats)
      cols = c('black', sample(rainbow(max(fourth_img))))
      Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      #Upload original image and create object-identified image. 
      dapi_nmask <- upload_mask_5()
      #First interval processing on nuclear-stained image.
      dapi_width <- dim(dapi_nmask)[1]
      dapi_height <- dim(dapi_nmask)[2]
      dapi_xx_2 <-  dapi_width/4
      dapi_xx_2_2 <-  dapi_xx_2/6
      dapi_xx_2_2_2 <-  dapi_xx_2 -  dapi_xx_2_2
      dapi_xx_2_2_2_2 <-  dapi_xx_2 +  dapi_xx_2_2
      dapi_p1xx2 <- upload_image_5()[1: dapi_xx_2,]
      dapi_zero_img <- upload_mask_5()[dapi_xx_2_2_2:dapi_xx_2_2_2_2,]
      dapi_zero_stats <- data.frame(computeFeatures.moment(dapi_zero_img))
      max_zero <- max(dapi_zero_stats$m.cy)
      min_zero <- min(dapi_zero_stats$m.cy)
      dif_zero <- max_zero - min_zero
      width_zero <- round((((dif_zero * (1/(input$mtpr))) * 25400)/50000000))
      dapi_zero_obj <- nrow(dapi_zero_stats)
      cols = c('black', sample(rainbow(max(dapi_zero_img))))
      dapi_zero_img_color <- Image(cols[1+dapi_zero_img], dim=dim(dapi_zero_img))
      #Second interval processing on nuclear-stained image.
      dapi_yy_2 <- dapi_width/2
      dapi_yy_2_2 <- (dapi_yy_2 - dapi_xx_2)/6
      dapi_yy_2_2_2 <- dapi_yy_2 - dapi_yy_2_2
      dapi_yy_2_2_2_2 <- dapi_yy_2 + dapi_yy_2_2
      dapi_p2yy2 <- upload_image_5()[dapi_xx_2:dapi_yy_2,]
      dapi_second_img <- upload_mask_5()[dapi_yy_2_2_2:dapi_yy_2_2_2_2,]
      dapi_second_stats <- data.frame(computeFeatures.moment(dapi_second_img))
      max_second <- max(dapi_second_stats$m.cy)
      min_second <- min(dapi_second_stats$m.cy)
      dif_second <- max_second - min_second
      width_second <- round((((dif_second * (1/(input$mtpr))) * 25400) / 50000000))
      dapi_second_obj <- nrow(dapi_second_stats)
      cols = c('black', sample(rainbow(max(dapi_second_img))))
      dapi_second_img_color <- Image(cols[1+dapi_second_img], dim=dim(dapi_second_img))
      #Third interval processing on nuclear-stained image. 
      dapi_zz_2 <- (3*dapi_width/4)
      dapi_zz_2_2 <- (dapi_zz_2 - dapi_yy_2)/6
      dapi_zz_2_2_2 <- dapi_zz_2 - dapi_zz_2_2
      dapi_zz_2_2_2_2 <- dapi_zz_2 + dapi_zz_2_2
      dapi_p3zz2 <- upload_image_5()[dapi_yy_2:dapi_zz_2,]
      dapi_third_img <- upload_mask_5()[dapi_zz_2_2_2:dapi_zz_2_2_2_2,]
      dapi_third_stats <- data.frame(computeFeatures.moment(dapi_third_img))
      max_third <- max(dapi_third_stats$m.cy)
      min_third <- min(dapi_third_stats$m.cy)
      dif_third <- max_third - min_third
      width_third <- round((((dif_third * (1/(input$mtpr))) * 25400) / 50000000))
      dapi_third_obj <- nrow(dapi_third_stats)
      cols = c('black', sample(rainbow(max(dapi_third_img))))
      dapi_third_img_color <- Image(cols[1+dapi_third_img], dim=dim(dapi_third_img))
      #Fourth interval processing on nuclear-stained image. 
      dapi_zzz_2 <- (dapi_width)
      dapi_zzz_2_2 <- (dapi_zzz_2-dapi_zz_2)/6
      dapi_zzz_2_2_2 <- dapi_zzz_2 - dapi_zzz_2_2
      dapi_p4zzz2 <- upload_image_5()[dapi_zz_2:dapi_zzz_2,]
      dapi_fourth_img <- upload_mask_5()[dapi_zzz_2_2_2:dapi_zzz_2,]
      dapi_fourth_stats <- data.frame(computeFeatures.moment(dapi_fourth_img))
      max_fourth <- max(dapi_fourth_stats$m.cy)
      min_fourth <- min(dapi_fourth_stats$m.cy)
      dif_fourth <- max_fourth - min_fourth
      width_fourth <- round((((dif_fourth * (1/(input$mtpr))) * 25400) / 50000000))
      dapi_fourth_obj <- nrow(dapi_fourth_stats)
      cols = c('black', sample(rainbow(max(dapi_fourth_img))))
      dapi_fourth_img_color <- Image(cols[1+dapi_fourth_img], dim=dim(dapi_fourth_img))
      #Quantify the amount of axons per section at each interval. 
      num_axon_per_300um_section_0_500um <- round(zero_obj)
      num_axon_per_300um_section_500_1000um <- round(second_obj)
      num_axon_per_300um_section_1000_1500um <- round(third_obj)
      num_axon_per_300um_section_1500_2000um <- round(fourth_obj)
      #Convert the amount of axons per section to axons per nerve. 
      num_axon_per_nerve_0_500um <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_per_300um_section_0_500um/0.3)/0.014))
      num_axon_per_nerve_500_1000um <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_per_300um_section_500_1000um/0.3)/0.014))
      num_axon_per_nerve_1000_1500um <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_per_300um_section_1000_1500um/0.3)/0.014))
      num_axon_per_nerve_1500_2000um <- as.integer(round(3.14159*(0.3/2)^2*(num_axon_per_300um_section_1500_2000um/0.3)/0.014))
      #Create variables for the nerve width at each interal. 
      width_0_500um <- width_zero
      width_500_1000um <- width_second
      width_1000_1500um <- width_third
      width_1500_2000um <- width_fourth
      #Quantify the amount of axons per micrometer of nerve width. 
      num_axon_per_nerve_width_0_500um <- round((num_axon_per_300um_section_0_500um/width_zero), digits = 2)
      num_axon_per_nerve_width_500_1000um <- round((num_axon_per_300um_section_500_1000um/width_second), digits = 2)
      num_axon_per_nerve_width_1000_1500um <- round((num_axon_per_300um_section_1000_1500um/width_third), digits = 2)
      num_axon_per_nerve_width_1500_2000um <- round((num_axon_per_300um_section_1500_2000um/width_fourth), digits = 2)
      #Create data table output for all quantification information. 
      Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
      Number_of_Axons_per_Section <- c(num_axon_per_300um_section_0_500um, num_axon_per_300um_section_500_1000um, num_axon_per_300um_section_1000_1500um, num_axon_per_300um_section_1500_2000um)
      Number_of_Axons_per_Nerve <- c(num_axon_per_nerve_0_500um, num_axon_per_nerve_500_1000um, num_axon_per_nerve_1000_1500um, num_axon_per_nerve_1500_2000um)
      Nerve_Width_in_Micrometers <- c(width_0_500um, width_500_1000um, width_1000_1500um, width_1500_2000um) 
      Number_Axons_per_Width_Micrometers <- c(num_axon_per_nerve_width_0_500um, num_axon_per_nerve_width_500_1000um, num_axon_per_nerve_width_1000_1500um, num_axon_per_nerve_width_1500_2000um)
      df <- data.frame(Distance_from_Injury_Site, Number_of_Axons_per_Section, Number_of_Axons_per_Nerve, Nerve_Width_in_Micrometers, Number_Axons_per_Width_Micrometers)
      df
    })
    
    #Output quantification information results into data table for user. 
    output$results_4 <- renderDataTable({
      validate(need(input$submit_4, "Please upload an image."))
      datatable(
        upload_results_4(),
        
        extensions = 'Buttons',
        
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 4,  # Show 4 rows per page
          autoWidth = FALSE, # Automatically adjust column widths
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display"
      )
    })
  })
  
  #Below is a repeat of code that is necessary for the Shiny to function properly. 
  output$img_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_image_4())
  })
  
  output$mask_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_mask_4())
  })
  
  output$obj_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_obj_4())
  })
  
  
  output$int_1_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_1_4())
  })
  
  output$int_2_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_2_4())
  })
  
  output$int_3_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_3_4())
  })
  
  output$int_4_4 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_4_4())
  })
  
  
  output$img_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_image_5())
  })
  
  output$mask_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_mask_5())
  })
  
  output$obj_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_obj_5())
  })
  
  
  output$int_1_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_1_5())
  })
  
  output$int_2_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_2_5())
  })
  
  output$int_3_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_3_5())
  })
  
  output$int_4_5 <- renderPlot({ 
    validate(need(input$submit_4, "Please upload an image."))
    plot(upload_int_4_5())
  })
  
  upload_results_4 <- reactive({
    validate(need(input$submit_4, "Please upload an image."))
    nmask <- upload_mask_4()
    dim(nmask)
    width <- dim(nmask)[1]
    height <- dim(nmask)[2]
    x_2 <- width/4
    x_2_2 <- x_2/6
    x_2_2_2 <- x_2 - x_2_2
    x_2_2_2_2 <- x_2 + x_2_2
    p1x2 <- upload_image_4()[1:x_2,]
    zero_img <- upload_mask_4()[x_2_2_2:x_2_2_2_2,]
    zero_stats <- data.frame(computeFeatures.shape(zero_img))
    zero_obj <- nrow(zero_stats)
    cols = c('black', sample(rainbow(max(zero_img))))
    zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
    zero_img_color
    x_2 <- width/4
    y_2 <- width/2
    y_2_2 <- (y_2 - x_2)/6
    y_2_2_2 <- y_2 - y_2_2
    y_2_2_2_2 <- y_2 + y_2_2
    p2y2 <- upload_image_4()[x_2:y_2,]
    second_img <- upload_mask_4()[y_2_2_2:y_2_2_2_2,]
    second_stats <- data.frame(computeFeatures.shape(second_img))
    second_obj <- nrow(second_stats)
    cols = c('black', sample(rainbow(max(second_img))))
    Second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
    Second_img_color
    y_2 <- width/2
    z_2 <- (3*width/4)
    z_2_2 <- (z_2 - y_2)/6
    z_2_2_2 <- z_2 - z_2_2
    z_2_2_2_2 <- z_2 + z_2_2
    p3z2 <- upload_image_4()[y_2:z_2,]
    third_img <- upload_mask_4()[z_2_2_2:z_2_2_2_2,]
    third_stats <- data.frame(computeFeatures.shape(third_img))
    third_obj <- nrow(third_stats)
    cols = c('black', sample(rainbow(max(third_img))))
    Third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
    Third_img_color
    z_2 <- (3*width/4)
    zz_2 <- (width)
    zz_2_2 <- (zz_2-z_2)/6
    zz_2_2_2 <- zz_2 - zz_2_2
    p4zz2 <- upload_image_4()[z_2:zz_2,]
    fourth_img <- upload_mask_4()[zz_2_2_2:zz_2,]
    fourth_stats <- data.frame(computeFeatures.shape(fourth_img))
    fourth_obj <- nrow(fourth_stats)
    cols = c('black', sample(rainbow(max(fourth_img))))
    Fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
    Fourth_img_color
    dapi_nmask <- upload_mask_5()
    dim(dapi_nmask)
    dapi_width <- dim(dapi_nmask)[1]
    dapi_height <- dim(dapi_nmask)[2]
    dapi_xx_2 <-  dapi_width/4
    dapi_xx_2_2 <-  dapi_xx_2/6
    dapi_xx_2_2_2 <-  dapi_xx_2 -  dapi_xx_2_2
    dapi_xx_2_2_2_2 <-  dapi_xx_2 +  dapi_xx_2_2
    dapi_p1xx2 <- upload_image_5()[1: dapi_xx_2,]
    dapi_zero_img <- upload_mask_5()[dapi_xx_2_2_2:dapi_xx_2_2_2_2,]
    dapi_zero_stats <- data.frame(computeFeatures.moment(dapi_zero_img))
    max_zero <- max(dapi_zero_stats$m.cy)
    min_zero <- min(dapi_zero_stats$m.cy)
    dif_zero <- max_zero - min_zero
    width_zero <- round((((dif_zero * (1/(input$mtpr))) * 25400) / 50000000))
    dapi_zero_obj <- nrow(dapi_zero_stats)
    cols = c('black', sample(rainbow(max(dapi_zero_img))))
    dapi_zero_img_color <- Image(cols[1+dapi_zero_img], dim=dim(dapi_zero_img))
    plot(dapi_zero_img_color)
    dapi_yy_2 <- dapi_width/2
    dapi_yy_2_2 <- (dapi_yy_2 - dapi_xx_2)/6
    dapi_yy_2_2_2 <- dapi_yy_2 - dapi_yy_2_2
    dapi_yy_2_2_2_2 <- dapi_yy_2 + dapi_yy_2_2
    dapi_p2yy2 <- upload_image_5()[dapi_xx_2:dapi_yy_2,]
    dapi_second_img <- upload_mask_5()[dapi_yy_2_2_2:dapi_yy_2_2_2_2,]
    dapi_second_stats <- data.frame(computeFeatures.moment(dapi_second_img))
    max_second <- max(dapi_second_stats$m.cy)
    min_second <- min(dapi_second_stats$m.cy)
    dif_second <- max_second - min_second
    width_second <- round((((dif_second * (1/(input$mtpr))) * 25400) / 50000000))
    dapi_second_obj <- nrow(dapi_second_stats)
    cols = c('black', sample(rainbow(max(dapi_second_img))))
    dapi_second_img_color <- Image(cols[1+dapi_second_img], dim=dim(dapi_second_img))
    plot(dapi_second_img_color)
    dapi_zz_2 <- (3*dapi_width/4)
    dapi_zz_2_2 <- (dapi_zz_2 - dapi_yy_2)/6
    dapi_zz_2_2_2 <- dapi_zz_2 - dapi_zz_2_2
    dapi_zz_2_2_2_2 <- dapi_zz_2 + dapi_zz_2_2
    dapi_p3zz2 <- upload_image_5()[dapi_yy_2:dapi_zz_2,]
    dapi_third_img <- upload_mask_5()[dapi_zz_2_2_2:dapi_zz_2_2_2_2,]
    dapi_third_stats <- data.frame(computeFeatures.moment(dapi_third_img))
    max_third <- max(dapi_third_stats$m.cy)
    min_third <- min(dapi_third_stats$m.cy)
    dif_third <- max_third - min_third
    width_third <- round((((dif_third * (1/(input$mtpr))) * 25400) / 50000000))
    dapi_third_obj <- nrow(dapi_third_stats)
    cols = c('black', sample(rainbow(max(dapi_third_img))))
    dapi_third_img_color <- Image(cols[1+dapi_third_img], dim=dim(dapi_third_img))
    plot(dapi_third_img_color)
    dapi_zzz_2 <- (dapi_width)
    dapi_zzz_2_2 <- (dapi_zzz_2-dapi_zz_2)/6
    dapi_zzz_2_2_2 <- dapi_zzz_2 - dapi_zzz_2_2
    dapi_p4zzz2 <- upload_image_5()[dapi_zz_2:dapi_zzz_2,]
    dapi_fourth_img <- upload_mask_5()[dapi_zzz_2_2_2:dapi_zzz_2,]
    dapi_fourth_stats <- data.frame(computeFeatures.moment(dapi_fourth_img))
    max_fourth <- max(dapi_fourth_stats$m.cy)
    min_fourth <- min(dapi_fourth_stats$m.cy)
    dif_fourth <- max_fourth - min_fourth
    width_fourth <- round((((dif_fourth* (1/(input$mtpr))) * 25400) / 50000000))
    dapi_fourth_obj <- nrow(dapi_fourth_stats)
    cols = c('black', sample(rainbow(max(dapi_fourth_img))))
    dapi_fourth_img_color <- Image(cols[1+dapi_fourth_img], dim=dim(dapi_fourth_img))
    plot(dapi_fourth_img_color)
    num_axon_per_300um_section_0_500um <- round(zero_obj)
    num_axon_per_300um_section_500_1000um <- round(second_obj)
    num_axon_per_300um_section_1000_1500um <- round(third_obj)
    num_axon_per_300um_section_1500_2000um <- round(fourth_obj)
    num_axon_per_300um_0_500um <- round((num_axon_per_300um_section_0_500um/width_zero), digits = 2)
    num_axon_per_300um_500_1000um <- round((num_axon_per_300um_section_500_1000um/width_second), digits = 2)
    num_axon_per_300um_1000_1500um <- round((num_axon_per_300um_section_1000_1500um/width_third), digits = 2)
    num_axon_per_300um_1500_2000um <- round((num_axon_per_300um_section_1500_2000um/width_fourth), digits = 2)
    Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
    Number_of_Axons_per_Section <- c(num_axon_per_300um_section_0_500um, num_axon_per_300um_section_500_1000um, num_axon_per_300um_section_1000_1500um, num_axon_per_300um_section_1500_2000um)
    Number_of_Axons_per_Nerve <- c(num_axon_per_nerve_0_500um, num_axon_per_nerve_500_1000um, num_axon_per_nerve_1000_1500um, num_axon_per_nerve_1500_2000um)
    Nerve_Width_in_Micrometers <- c(width_0_500um, width_500_1000um, width_1000_1500um, width_1500_2000um) 
    Number_Axons_per_Width_Micrometers <- c(num_axon_per_nerve_width_0_500um, num_axon_per_nerve_width_500_1000um, num_axon_per_nerve_width_1000_1500um, num_axon_per_nerve_width_1500_2000um)
    df <- data.frame(Distance_from_Injury_Site, Number_of_Axons_per_Section, Number_of_Axons_per_Nerve, Nerve_Width_in_Micrometers, Number_Axons_per_Width_Micrometers)
    df
  })
  
  output$results_4 <- renderDataTable({
    validate(need(input$submit_4, "Please upload an image."))
    datatable(
      upload_results_4(),
      
      extensions = 'Buttons',
      
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      
      class = "display"
    )
  })

  
  #### Total Mean Fluorescence Intensity (MFI) Quantification Tab #####################################################################################################################################
  
  #Function for reset button to clear all images and results output.
  observeEvent(input$reset_3, {
    
    output$img_3 <- renderPlot({
    })
    output$mask_3 <- renderPlot({
    })
    output$obj_3 <- renderPlot({
    })
    output$int_1_3 <- renderPlot({
    })
    output$int_2_3 <- renderPlot({
    })
    output$int_3_3 <- renderPlot({
    })
    output$int_4_3 <- renderPlot({
    })
    output$results_3 <- renderDataTable({
    })
  })
  
  #Function for submit button to process uploaded images for quantification. 
  observeEvent(input$submit_3, {
  
    #Upload original image. 
    upload_image_3 <- reactive({
      validate(need(input$file3, "Please upload an image."))
      readImage(input$file3$datapath)
    })
    
    #Create the first interval of the image. 
    upload_int_1_3 <- reactive({
      validate(need(input$file3, "Please upload an image."))
      nmask <- upload_image_3()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- nmask[1:x_2,]
      zero_img <- nmask[x_2_2_2:x_2_2_2_2,]
      zero_mean <- mean(zero_img)
      zero_img
    })
    
    #Create the second interval of the image.
    upload_int_2_3 <- reactive({
      nmask <- upload_image_3()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- nmask[x_2:y_2,]
      second_img <- nmask[y_2_2_2:y_2_2_2_2,]
      second_mean <- mean(second_img)
      second_img
    })
    
    #Create the third interval of an image. 
    upload_int_3_3 <- reactive({
      nmask <- upload_image_3()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- nmask[y_2:z_2,]
      third_img <- nmask[z_2_2_2:z_2_2_2_2,]
      third_mean <- mean(third_img)
      third_img
    })
    
    #Create the fourth interval of the image. 
    upload_int_4_3 <- reactive({
      nmask <- upload_image_3()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- nmask[z_2:zz_2,]
      fourth_img <- nmask[zz_2_2_2:zz_2,]
      fourth_mean <- mean(fourth_img)
      fourth_img
    })
    
    #Reactive function to quantify the MFI for each interval along the image. 
    upload_results_3 <- reactive({
      #First interval quantification.
      nmask <- upload_image_3()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- nmask[1:x_2,]
      zero_img <- nmask[x_2_2_2:x_2_2_2_2,]
      zero_mean <- mean(zero_img)
      #Second interval quantification.
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- nmask[x_2:y_2,]
      second_img <- nmask[y_2_2_2:y_2_2_2_2,]
      second_mean <- mean(second_img)
      #Third interval quantification.
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- nmask[y_2:z_2,]
      third_img <- nmask[z_2_2_2:z_2_2_2_2,]
      third_mean <- mean(third_img)
      #Fourth interval quantification.
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- nmask[z_2:zz_2,]
      fourth_img <- nmask[zz_2_2_2:zz_2,]
      fourth_mean <- mean(fourth_img)
      #Create variables for each MFI value per interval.
      zero_MFI <- zero_mean
      second_MFI <- second_mean
      thrid_MFI <- third_mean
      fourth_MFI <- fourth_mean
      #Create data table to output quantification results for the user. 
      Distance_from_Injury_Site <- c("First_Interval", "Second_Interval", "Third_Interval", "Fourth_Interval")
      Average_MFI <- c(zero_MFI, second_MFI, thrid_MFI, fourth_MFI)
      data.frame(Distance_from_Injury_Site, Average_MFI)
    })
    
    #Output the original image. 
    output$img_3 <- renderPlot({ 
      validate(need(input$submit_3, "Please upload an image."))
      plot(upload_image_3())
    })
    
    #Output the first interval image. 
    output$int_1_3 <- renderPlot({ 
      validate(need(input$submit_3, "Please upload an image."))
      plot(upload_int_1_3())
    })
    
    #Output the second interval image.
    output$int_2_3 <- renderPlot({ 
      validate(need(input$submit_3, "Please upload an image."))
      plot(upload_int_2_3())
    })
    
    #Output the third interval image.
    output$int_3_3 <- renderPlot({ 
      validate(need(input$submit_3, "Please upload an image."))
      plot(upload_int_3_3())
    })
    
    #Output the fourth interval image.
    output$int_4_3 <- renderPlot({ 
      validate(need(input$submit_3, "Please upload an image."))
      plot(upload_int_4_3())
    })
    
    #Output the quantification information into a data table for the user.
    output$results_3 <- renderDataTable({ 
      validate(need(input$submit_3, "Please upload an image."))
      datatable(
        upload_results_3(),
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 4,  # Show 4 rows per page
          autoWidth = FALSE, # Automatically adjust column widths
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )
    })
  })
  
  #Below is code that repeats the output code above that is necessary for the function of the Shiny. 
  output$img_3 <- renderPlot({ 
    validate(need(input$submit_3, "Please upload an image."))
    plot(upload_image_3())
  })
  
  output$int_1_3 <- renderPlot({ 
    validate(need(input$submit_3, "Please upload an image."))
    plot(upload_int_1_3())
  })
  
  output$int_2_3 <- renderPlot({ 
    validate(need(input$submit_3, "Please upload an image."))
    plot(upload_int_2_3())
  })
  
  output$int_3_3 <- renderPlot({ 
    validate(need(input$submit_3, "Please upload an image."))
    plot(upload_int_3_3())
  })
  
  output$int_4_3 <- renderPlot({ 
    validate(need(input$submit_3, "Please upload an image."))
    plot(upload_int_4_3())
  })
  
  output$results_3 <- renderDataTable({ 
    validate(need(input$submit_3, "Please upload an image."))
    datatable(
      upload_results_3(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Total Mean Fluorescence Intensity (MFI) Quantification for Multiple Files Tab #####################################################################################################################################
  
  #Helper function
  #Function to process and quantify each image at four intervals. 
  upload_results_mfi <- function(image_to_process, image_name) {
    #Create the first interval of original image and calculate the MFI.
    nmask <- image_to_process
    dim(nmask)
    width <- dim(nmask)[1]
    height <- dim(nmask)[2]
    x_2 <- width/4
    x_2_2 <- x_2/6
    x_2_2_2 <- x_2 - x_2_2
    x_2_2_2_2 <- x_2 + x_2_2
    p1x2 <- nmask[1:x_2,]
    zero_img <- nmask[x_2_2_2:x_2_2_2_2,]
    zero_mean <- mean(zero_img)
    #Create the second interval of original image and calculate the MFI.
    y_2 <- width/2
    y_2_2 <- (y_2 - x_2)/6
    y_2_2_2 <- y_2 - y_2_2
    y_2_2_2_2 <- y_2 + y_2_2
    p2y2 <- nmask[x_2:y_2,]
    second_img <- nmask[y_2_2_2:y_2_2_2_2,]
    second_mean <- mean(second_img)
    #Create the third interval of original image and calculate the MFI.
    z_2 <- (3*width/4)
    z_2_2 <- (z_2 - y_2)/6
    z_2_2_2 <- z_2 - z_2_2
    z_2_2_2_2 <- z_2 + z_2_2
    p3z2 <- nmask[y_2:z_2,]
    third_img <- nmask[z_2_2_2:z_2_2_2_2,]
    third_mean <- mean(third_img)
    #Create the fourth interval of original image and calculate the MFI.
    zz_2 <- (width)
    zz_2_2 <- (zz_2-z_2)/6
    zz_2_2_2 <- zz_2 - zz_2_2
    p4zz2 <- nmask[z_2:zz_2,]
    fourth_img <- nmask[zz_2_2_2:zz_2,]
    fourth_mean <- mean(fourth_img)
    #Create variables to store quantification results into per interval.
    zero_MFI <- zero_mean
    second_MFI <- second_mean
    thrid_MFI <- third_mean
    fourth_MFI <- fourth_mean
    #Create data table output of quantification results.
    Average_MFI <- c(zero_MFI, second_MFI, thrid_MFI, fourth_MFI)
    Average_MFI <- round(Average_MFI, 2)
    Average_MFI <- data.frame(value = unlist(Average_MFI))
    names(Average_MFI) <- c(image_name)
    data.frame(Average_MFI)
  }
  
  
  #Observe the submit button click event.
  output_table_cj_MFI <- eventReactive(input$submit_multiple_files_mfi , {
    req(input$list_multiple_mfi)  # Only proceed if files are uploaded.
    ## Initialize two lists to store the new images and their name.
    new_images <- list()
    new_names <- list()
    
    #Loop through each uploaded file to get the image and its name.
    for (i in seq_along(input$list_multiple_mfi$datapath)) {
      #Read each uploaded image and their name.
      new_image <- readImage(input$list_multiple_mfi$datapath[i])
      new_name <- input$list_multiple_mfi$name[i]
      #Add the image and name to the list of new images and names, respectively.
      new_images[[i]] <- new_image
      new_names[[i]] <- new_name
    }
    
    #Creates the table with all the values of interest.
    images <- new_images
    image_names <- new_names
    
    #Ensures images have been uploaded.
    req(images, length(images) > 0)
    
    #Creates a dataframe with 4 rows to which we will append our data.
    Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
    result_table <- data.frame(distance_injury_site = Distance_from_Injury_Site)
    
    #Creates a loading bar.
    withProgress(message = 'Processing...', value = 0, {
      
      #Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
      steps <- length(images)
      counter <- as.integer(1)
      
      #Generate table for each image in the list.
      for (image in images) {
        # Increments the progress bar.
        incProgress(1/steps, detail = paste("Image", counter, "of", steps))
        # Processes the image and creates the table.
        results_current <- upload_results_mfi(image, image_names[counter])
        # Add the created table to the final table.
        result_table <- cbind(result_table, results_current)
        counter <- counter + 1
      }
    })
    result_table
  })
  
  output$results_multiple_files_mfi <- renderDT({
    #Use datatable() to display and make the table interactive.
    datatable(
      ##result_table,
      output_table_cj_MFI(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Normalized Mean Fluorescence Intensity (MFI) Quantification Tab #####################################################################################################################################
  #Function for reset button to clear original images, interval images, and quantification output results. 
   observeEvent(input$reset_5, {
    output$img_6 <- renderPlot({
    })
    output$int_1_6 <- renderPlot({
    })
    output$int_2_6 <- renderPlot({
    })
    output$int_3_6 <- renderPlot({
    })
    output$int_4_6 <- renderPlot({
    })
    output$img_7 <- renderPlot({
    })
    output$int_1_7 <- renderPlot({
    })
    output$int_2_7 <- renderPlot({
    })
    output$int_3_7 <- renderPlot({
    })
    output$int_4_7 <- renderPlot({
    })
    output$results_5 <- renderDataTable({
    })
  })
  
  #Function for submit button to process and quantify uploaded images.
  observeEvent(input$submit_5, {
    
    #Reactive function to upload original image. 
    upload_image_6 <- reactive({
      readImage(input$file6$datapath)
    })
    
    #Reactive function to generate the first interval of the image. 
    upload_int_1_6 <- reactive({
      nmask <- upload_image_6()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_6()[1:x_2,]
      zero_img <- upload_image_6()[x_2_2_2:x_2_2_2_2,]
      zero_img
    })
    
    #Reactive function to generate the second interval of the image.
    upload_int_2_6 <- reactive({
      nmask <- upload_image_6()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_6()[x_2:y_2,]
      second_img <- upload_image_6()[y_2_2_2:y_2_2_2_2,]
      second_img
    })
    
    #Reactive function to generate the third interval of the image.
    upload_int_3_6 <- reactive({
      nmask <- upload_image_6()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_6()[y_2:z_2,]
      third_img <- upload_image_6()[z_2_2_2:z_2_2_2_2,]
      third_img
    })
    
    #Reactive function to generate the fourth interval of the image.
    upload_int_4_6 <- reactive({
      nmask <- upload_image_6()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_6()[z_2:zz_2,]
      fourth_img <- upload_image_6()[zz_2_2_2:zz_2,]
      fourth_img
    })
    
    #Output the original image.
    output$img_6 <- renderPlot({ 
      plot(upload_image_6())
    })
    
    #Output the first interval of the original image.
    output$int_1_6 <- renderPlot({ 
      plot(upload_int_1_6())
    })
    
    #Output the second interval of the original image.
    output$int_2_6 <- renderPlot({ 
      plot(upload_int_2_6())
    })
    
    #Output the third interval of the original image.
    output$int_3_6 <- renderPlot({ 
      plot(upload_int_3_6())
    })
    
    #Output the fourth interval of the original image.
    output$int_4_6 <- renderPlot({ 
      plot(upload_int_4_6())
    })
    
    #Reactive function to upload the nuclear-stained image.
    upload_image_7 <- reactive({
      readImage(input$file7$datapath)
    })
    
    #Reactive function to process and quantify the first interval of the nuclear-stained image.
    upload_int_1_7 <- reactive({
      nmask <- upload_image_7()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(nmask, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_7()[1:x_2,]
      zero_img <- nmask[x_2_2_2:x_2_2_2_2,]
      yy_2 <- max(nmask_moments$m.cy)
      yy_1 <- min(nmask_moments$m.cy)
      zero_img <- zero_img[,yy_1:yy_2]
      zero_img
      zero_stats <- data.frame(computeFeatures.shape(zero_img))
      zero_obj <- nrow(zero_stats)
      cols = c('black', sample(rainbow(max(zero_img))))
      zero_img_color <- Image(cols[1+zero_img], dim=dim(zero_img))
      zero_img_color
    })
    
    #Reactive function to process and quantify the second interval of the nuclear-stained image.
    upload_int_2_7 <- reactive({
      nmask <- upload_image_7()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(nmask, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_7()[x_2:y_2,]
      second_img <- nmask[y_2_2_2:y_2_2_2_2,]
      yyy_2 <- max(nmask_moments$m.cy)
      yyy_1 <- min(nmask_moments$m.cy)
      second_img <- second_img[,yyy_1:yyy_2]
      second_img
      cols = c('black', sample(rainbow(max(second_img))))
      second_img_color <- Image(cols[1+second_img], dim=dim(second_img))
      second_img_color
    })
    
    #Reactive function to process and quantify the third interval of the nuclear-stained image.
    upload_int_3_7 <- reactive({
      nmask <- upload_image_7()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(nmask, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      y_2 <- width/2
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_7()[y_2:z_2,]
      third_img <- nmask[z_2_2_2:z_2_2_2_2,]
      yyyy_2 <- max(nmask_moments$m.cy)
      yyyy_1 <- min(nmask_moments$m.cy)
      third_img <- third_img[,yyyy_1:yyyy_2]
      third_img
      cols = c('black', sample(rainbow(max(third_img))))
      third_img_color <- Image(cols[1+third_img], dim=dim(third_img))
      third_img_color
    })
    
    #Reactive function to process and quantify the fourth interval of the nuclear-stained image.
    upload_int_4_7 <- reactive({
      nmask <- upload_image_7()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(nmask, kern)
      nmask <- eidilat
      nmask <- thresh(nmask, w=5, h= 15, offset= 0.1)
      nmask <- opening(nmask, makeBrush(1, shape='line'))
      nmask <- bwlabel(nmask)
      nmask_moments <- data.frame(computeFeatures.moment(nmask))
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      z_2 <- (3*width/4)
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_7()[z_2:zz_2,]
      fourth_img <- nmask[zz_2_2_2:zz_2,]
      yyyyy_2 <- max(nmask_moments$m.cy)
      yyyyy_1 <- min(nmask_moments$m.cy)
      fourth_img <- fourth_img[,yyyyy_1:yyyyy_2]
      fourth_img
      cols = c('black', sample(rainbow(max(fourth_img))))
      fourth_img_color <- Image(cols[1+fourth_img], dim=dim(fourth_img))
      fourth_img_color
    })
    
    #Output the original image. 
    output$img_6 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_image_6())
    })
    
    #Output the first interval of the original image. 
    output$int_1_6 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_1_6())
    })
    
    #Output the second interval of the original image. 
    output$int_2_6 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_2_6())
    })
    
    #Output the third interval of the original image. 
    output$int_3_6 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_3_6())
    })
    
    #Output the fourth interval of the original image. 
    output$int_4_6 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_4_6())
    })
    
    #Output the original nuclear-stained image. 
    output$img_7 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_image_7())
    })
    
    #Output the first interval of the nuclear-stained image.
    output$int_1_7 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_1_7())
    })
    
    #Output the second interval of the nuclear-stained image.
    output$int_2_7 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_2_7())
    })
    
    #Output the third interval of the nuclear-stained image.
    output$int_3_7 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_3_7())
    })
    
    #Output the fourth interval of the nuclear-stained image.
    output$int_4_7 <- renderPlot({ 
      validate(need(input$submit_5, "Please upload an image."))
      plot(upload_int_4_7())
    })
    
    #Reactive function to process and quantify both the original and nuclear-stained image at each of the four intervals. 
    upload_results_5 <- reactive({
      #Upload the original image.
      nmask <- upload_image_6()
      dim(nmask)
      width <- dim(nmask)[1]
      height <- dim(nmask)[2]
      x_2 <- width/4
      x_2_2 <- x_2/6
      x_2_2_2 <- x_2 - x_2_2
      x_2_2_2_2 <- x_2 + x_2_2
      p1x2 <- upload_image_6()[1:x_2,]
      zero_img <- upload_image_6()[x_2_2_2:x_2_2_2_2,]
      #Upload the nuclear-stained image.
      dapi_nmask_1 <- upload_image_7()
      kern <- makeBrush(5, shape='gaussian')
      eidilat <- dilate(dapi_nmask_1, kern)
      dapi_nmask_1 <- eidilat
      dapi_nmask_1 <- thresh(dapi_nmask_1, w=5, h= 15, offset= 0.1)
      dapi_nmask_1 <- opening(dapi_nmask_1, makeBrush(1, shape='line'))
      dapi_nmask_1 <- bwlabel(dapi_nmask_1)
      #Generate features of each object in the nuclear-stained image. 
      nmask_moments <- data.frame(computeFeatures.moment(dapi_nmask_1))
      dim(dapi_nmask_1)
      #Create the first interval of nuclear-stained image.
      dapi_width <- dim(dapi_nmask_1)[1]
      dapi_height <- dim(dapi_nmask_1)[2]
      dapi_xx_2 <-  dapi_width/4
      dapi_xx_2_2 <-  dapi_xx_2/6
      dapi_xx_2_2_2 <-  dapi_xx_2 -  dapi_xx_2_2
      dapi_xx_2_2_2_2 <-  dapi_xx_2 +  dapi_xx_2_2
      dapi_p1xx2 <- dapi_nmask_1[1: dapi_xx_2,]
      dapi_zero_img <- dapi_nmask_1[dapi_xx_2_2_2:dapi_xx_2_2_2_2,]
      dapi_zero_stats <- data.frame(computeFeatures.moment(dapi_zero_img))
      #Determine the maximum and minimum center of masses of objects in the interval.
      yy_2 <- max(dapi_zero_stats$m.cy)
      yy_1 <- min(dapi_zero_stats$m.cy)
      zero_img <- zero_img[,yy_1:yy_2]
      #Calculate the difference between the maximum and minimum center of mass in pixels.
      dif_dapi_zero <- yy_2 - yy_1
      #Convert pixels to micrometers. 
      width_dapi_zero <- round((((dif_dapi_zero * (1/(input$mtpr_2))) * 25400) / 50000000))
      zero_mean <- round(mean(zero_img), digits = 3)
      #Create the second interval of nuclear-stained image.
      y_2 <- width/2
      y_2_2 <- (y_2 - x_2)/6
      y_2_2_2 <- y_2 - y_2_2
      y_2_2_2_2 <- y_2 + y_2_2
      p2y2 <- upload_image_6()[x_2:y_2,]
      second_img <- upload_image_6()[y_2_2_2:y_2_2_2_2,]
      dapi_yy_2 <- dapi_width/2
      dapi_yy_2_2 <- (dapi_yy_2 - dapi_xx_2)/6
      dapi_yy_2_2_2 <- dapi_yy_2 - dapi_yy_2_2
      dapi_yy_2_2_2_2 <- dapi_yy_2 + dapi_yy_2_2
      dapi_p2yy2 <- dapi_nmask_1[dapi_xx_2:dapi_yy_2,]
      dapi_second_img <- dapi_nmask_1[dapi_yy_2_2_2:dapi_yy_2_2_2_2,]
      dapi_second_stats <- data.frame(computeFeatures.moment(dapi_second_img))
      #Determine the maximum and minimum center of masses of objects in the interval.
      yyy_2 <- max(dapi_second_stats$m.cy)
      yyy_1<- min(dapi_second_stats$m.cy)
      #Calculate the difference between the maximum and minimum center of mass in pixels.
      dif_dapi_second <- yyy_2 - yyy_1
      #Convert pixels to micrometers. 
      width_dapi_second <- round((((dif_dapi_second* (1/(input$mtpr_2))) * 25400) / 50000000))
      second_img <- second_img[,yyy_1:yyy_2]
      second_mean <- round(mean(second_img), digits = 3)
      #Create the third interval of nuclear-stained image.
      z_2 <- (3*width/4)
      z_2_2 <- (z_2 - y_2)/6
      z_2_2_2 <- z_2 - z_2_2
      z_2_2_2_2 <- z_2 + z_2_2
      p3z2 <- upload_image_6()[y_2:z_2,]
      third_img <- upload_image_6()[z_2_2_2:z_2_2_2_2,]
      dapi_zz_2 <- (3*dapi_width/4)
      dapi_zz_2_2 <- (dapi_zz_2 - dapi_yy_2)/6
      dapi_zz_2_2_2 <- dapi_zz_2 - dapi_zz_2_2
      dapi_zz_2_2_2_2 <- dapi_zz_2 + dapi_zz_2_2
      dapi_p3zz2 <- dapi_nmask_1[dapi_yy_2:dapi_zz_2,]
      dapi_third_img <- dapi_nmask_1[dapi_zz_2_2_2:dapi_zz_2_2_2_2,]
      dapi_third_stats <- data.frame(computeFeatures.moment(dapi_third_img))
      #Determine the maximum and minimum center of masses of objects in the interval.
      yyyy_2 <- max(dapi_third_stats$m.cy)
      yyyy_1 <- min(dapi_third_stats$m.cy)
      #Calculate the difference between the maximum and minimum center of mass in pixels.
      dif_dapi_third <- yyyy_2 - yyyy_1
      #Convert pixels to micrometers. 
      width_dapi_third <- round((((dif_dapi_third* (1/(input$mtpr_2))) * 25400) / 50000000))
      third_img <- third_img[,yyyy_1:yyyy_2]
      third_mean <- round(mean(third_img), digits = 3)
      #Create the fourth interval of nuclear-stained image.
      zz_2 <- (width)
      zz_2_2 <- (zz_2-z_2)/6
      zz_2_2_2 <- zz_2 - zz_2_2
      p4zz2 <- upload_image_6()[z_2:zz_2,]
      fourth_img <- upload_image_6()[zz_2_2_2:zz_2,]
      dapi_zzz_2 <- (dapi_width)
      dapi_zzz_2_2 <- (dapi_zzz_2-dapi_zz_2)/6
      dapi_zzz_2_2_2 <- dapi_zzz_2 - dapi_zzz_2_2
      dapi_p4zzz2 <- dapi_nmask_1[dapi_zz_2:dapi_zzz_2,]
      dapi_fourth_img <- dapi_nmask_1[dapi_zzz_2_2_2:dapi_zzz_2,]
      dapi_fourth_stats <- data.frame(computeFeatures.moment(dapi_fourth_img))
      #Determine the maximum and minimum center of masses of objects in the interval.
      yyyyy_2 <- max(dapi_fourth_stats$m.cy)
      yyyyy_1 <- min(dapi_fourth_stats$m.cy)
      #Calculate the difference between the maximum and minimum center of mass in pixels.
      dif_dapi_fourth <- yyyyy_2 - yyyyy_1
      #Convert pixels to micrometers. 
      width_dapi_fourth <- round((((dif_dapi_fourth* (1/(input$mtpr_2))) * 25400) / 50000000))
      fourth_img <- fourth_img[,yyyyy_1:yyyyy_2]
      fourth_mean <- round(mean(fourth_img), digits = 3)
      #For each interval, normalize the MFI to the nerve width.
      zero_MFI <- round((zero_mean / width_dapi_zero), digits = 7)
      second_MFI <- round((second_mean / width_dapi_second), digits = 7)
      thrid_MFI <- round((third_mean / width_dapi_third), digits = 7)
      fourth_MFI <- round((fourth_mean / width_dapi_fourth), digits = 7)
      #Create a data table to output the quantification information to the user. 
      Distance_from_Injury_Site <- c("First_Interval", "Second_Interval", "Third_Interval", "Fourth_Interval")
      Average_MFI_per_Section <- c(zero_mean, second_mean, third_mean, fourth_mean)
      Nerve_Width_in_Micrometers <- c(width_dapi_zero, width_dapi_second, width_dapi_third, width_dapi_fourth) 
      Average_MFI_per_Width_Micrometer <- c(zero_MFI, second_MFI, thrid_MFI, fourth_MFI)
      df <- data.frame(Distance_from_Injury_Site, Average_MFI_per_Section, Nerve_Width_in_Micrometers, Average_MFI_per_Width_Micrometer)
      df
    })
    
    #Output a data table with the quantification information to the user. 
    output$results_5 <- renderDataTable({
      validate(need(input$submit_5, "Please upload an image."))
      datatable(
        upload_results_5(),
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 4,  # Show 4 rows per page
          autoWidth = FALSE, # Automatically adjust column widths
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )
    })
  })
  
  #Below is output code that is repeated from above that is necessary for the Shiny to function properly. 
  output$img_6 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_image_6())
  })
  
  output$int_1_6 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_1_6())
  })
  
  output$int_2_6 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_2_6())
  })
  
  output$int_3_6 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_3_6())
  })
  
  output$int_4_6 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_4_6())
  })
  
  output$img_7 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_image_7())
  })
  
  output$int_1_7 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_1_7())
  })
  
  output$int_2_7 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_2_7())
  })
  
  output$int_3_7 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_3_7())
  })
  
  output$int_4_7 <- renderPlot({ 
    validate(need(input$submit_5, "Please upload an image."))
    plot(upload_int_4_7())
  })
  
  upload_results_5 <- reactive({
    nmask <- upload_image_6()
    dim(nmask)
    width <- dim(nmask)[1]
    height <- dim(nmask)[2]
    x_2 <- width/4
    x_2_2 <- x_2/6
    x_2_2_2 <- x_2 - x_2_2
    x_2_2_2_2 <- x_2 + x_2_2
    p1x2 <- upload_image_6()[1:x_2,]
    zero_img <- upload_image_6()[x_2_2_2:x_2_2_2_2,]
    dapi_nmask_1 <- upload_image_7()
    kern <- makeBrush(5, shape='gaussian')
    eidilat <- dilate(dapi_nmask_1, kern)
    dapi_nmask_1 <- eidilat
    dapi_nmask_1 <- thresh(dapi_nmask_1, w=5, h= 15, offset= 0.1)
    dapi_nmask_1 <- opening(dapi_nmask_1, makeBrush(1, shape='line'))
    dapi_nmask_1 <- bwlabel(dapi_nmask_1)
    nmask_moments <- data.frame(computeFeatures.moment(dapi_nmask_1))
    dim(dapi_nmask_1)
    dapi_width <- dim(dapi_nmask_1)[1]
    dapi_height <- dim(dapi_nmask_1)[2]
    dapi_xx_2 <-  dapi_width/4
    dapi_xx_2_2 <-  dapi_xx_2/6
    dapi_xx_2_2_2 <-  dapi_xx_2 -  dapi_xx_2_2
    dapi_xx_2_2_2_2 <-  dapi_xx_2 +  dapi_xx_2_2
    dapi_p1xx2 <- dapi_nmask_1[1: dapi_xx_2,]
    dapi_zero_img <- dapi_nmask_1[dapi_xx_2_2_2:dapi_xx_2_2_2_2,]
    dapi_zero_stats <- data.frame(computeFeatures.moment(dapi_zero_img))
    yy_2 <- max(dapi_zero_stats$m.cy)
    yy_1 <- min(dapi_zero_stats$m.cy)
    zero_img <- zero_img[,yy_1:yy_2]
    dif_dapi_zero <- yy_2 - yy_1
    width_dapi_zero <- round((((dif_dapi_zero* (1/(input$mtpr_2))) * 25400) / 50000000))
    zero_mean <- round(mean(zero_img), digits = 3)
    y_2 <- width/2
    y_2_2 <- (y_2 - x_2)/6
    y_2_2_2 <- y_2 - y_2_2
    y_2_2_2_2 <- y_2 + y_2_2
    p2y2 <- upload_image_6()[x_2:y_2,]
    second_img <- upload_image_6()[y_2_2_2:y_2_2_2_2,]
    dapi_yy_2 <- dapi_width/2
    dapi_yy_2_2 <- (dapi_yy_2 - dapi_xx_2)/6
    dapi_yy_2_2_2 <- dapi_yy_2 - dapi_yy_2_2
    dapi_yy_2_2_2_2 <- dapi_yy_2 + dapi_yy_2_2
    dapi_p2yy2 <- dapi_nmask_1[dapi_xx_2:dapi_yy_2,]
    dapi_second_img <- dapi_nmask_1[dapi_yy_2_2_2:dapi_yy_2_2_2_2,]
    dapi_second_stats <- data.frame(computeFeatures.moment(dapi_second_img))
    yyy_2 <- max(dapi_second_stats$m.cy)
    yyy_1<- min(dapi_second_stats$m.cy)
    dif_dapi_second <- yyy_2 - yyy_1
    width_dapi_second <- round((((dif_dapi_second* (1/(input$mtpr_2))) * 25400) / 50000000))
    second_img <- second_img[,yyy_1:yyy_2]
    second_mean <- round(mean(second_img), digits = 3)
    z_2 <- (3*width/4)
    z_2_2 <- (z_2 - y_2)/6
    z_2_2_2 <- z_2 - z_2_2
    z_2_2_2_2 <- z_2 + z_2_2
    p3z2 <- upload_image_6()[y_2:z_2,]
    third_img <- upload_image_6()[z_2_2_2:z_2_2_2_2,]
    dapi_zz_2 <- (3*dapi_width/4)
    dapi_zz_2_2 <- (dapi_zz_2 - dapi_yy_2)/6
    dapi_zz_2_2_2 <- dapi_zz_2 - dapi_zz_2_2
    dapi_zz_2_2_2_2 <- dapi_zz_2 + dapi_zz_2_2
    dapi_p3zz2 <- dapi_nmask_1[dapi_yy_2:dapi_zz_2,]
    dapi_third_img <- dapi_nmask_1[dapi_zz_2_2_2:dapi_zz_2_2_2_2,]
    dapi_third_stats <- data.frame(computeFeatures.moment(dapi_third_img))
    yyyy_2 <- max(dapi_third_stats$m.cy)
    yyyy_1 <- min(dapi_third_stats$m.cy)
    dif_dapi_third <- yyyy_2 - yyyy_1
    width_dapi_third <- round((((dif_dapi_third* (1/(input$mtpr_2))) * 25400) / 50000000))
    third_img <- third_img[,yyyy_1:yyyy_2]
    third_mean <- round(mean(third_img), digits = 3)
    zz_2 <- (width)
    zz_2_2 <- (zz_2-z_2)/6
    zz_2_2_2 <- zz_2 - zz_2_2
    p4zz2 <- upload_image_6()[z_2:zz_2,]
    fourth_img <- upload_image_6()[zz_2_2_2:zz_2,]
    dapi_zzz_2 <- (dapi_width)
    dapi_zzz_2_2 <- (dapi_zzz_2-dapi_zz_2)/6
    dapi_zzz_2_2_2 <- dapi_zzz_2 - dapi_zzz_2_2
    dapi_p4zzz2 <- dapi_nmask_1[dapi_zz_2:dapi_zzz_2,]
    dapi_fourth_img <- dapi_nmask_1[dapi_zzz_2_2_2:dapi_zzz_2,]
    dapi_fourth_stats <- data.frame(computeFeatures.moment(dapi_fourth_img))
    yyyyy_2 <- max(dapi_fourth_stats$m.cy)
    yyyyy_1 <- min(dapi_fourth_stats$m.cy)
    dif_dapi_fourth <- yyyyy_2 - yyyyy_1
    width_dapi_fourth <- round((((dif_dapi_fourth* (1/(input$mtpr_2))) * 25400) / 50000000))
    fourth_img <- fourth_img[,yyyyy_1:yyyyy_2]
    fourth_mean <- round(mean(fourth_img), digits = 3)
    zero_MFI <- round((zero_mean / width_dapi_zero), digits = 7)
    second_MFI <- round((second_mean / width_dapi_second), digits = 7)
    thrid_MFI <- round((third_mean / width_dapi_third), digits = 7)
    fourth_MFI <- round((fourth_mean / width_dapi_fourth), digits = 7)
    Distance_from_Injury_Site <- c("First_Interval", "Second_Interval", "Third_Interval", "Fourth_Interval")
    Average_MFI_per_Section <- c(zero_mean, second_mean, third_mean, fourth_mean)
    Nerve_Width_in_Micrometers <- c(width_dapi_zero, width_dapi_second, width_dapi_third, width_dapi_fourth) 
    Average_MFI_per_Width_Micrometer <- c(zero_MFI, second_MFI, thrid_MFI, fourth_MFI)
    df <- data.frame(Distance_from_Injury_Site, Average_MFI_per_Section, Nerve_Width_in_Micrometers, Average_MFI_per_Width_Micrometer)
    df
  })
  
  output$results_5 <- renderDataTable({
    validate(need(input$submit_5, "Please upload an image."))
    datatable(
      upload_results_5(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Rbpms Single File Quantification Tab #####################################################################################################################################
  
  #Function for reset button to clear all images and data table output.
  observeEvent(input$reset_2, {
    output$img_2 <- renderPlot({
    })
    output$mask_2 <- renderPlot({
    })
    output$obj_2 <- renderPlot({
    })
    output$results_2 <- renderDataTable({
    })
  })
  
  #Function for the submit button to process and quantify images. 
  observeEvent(input$submit_2, {
   
    #Upload the original image.  
    upload_image_2 <- reactive({
      validate(need(input$file2, "Please upload an image."))
      readImage(input$file2$datapath)
    })
    
    #Create a mask from the original image. 
    upload_mask_2 <- reactive({
      validate(need(input$file2, "Please upload an image."))
      upload_image_2 <- upload_image_2()
      nmask <- upload_image_2
      nmask = thresh(nmask, w=50, h= 50, offset= 0.03)
      nmask = opening(nmask, makeBrush(3, shape='disc'))
      nmask = bwlabel(nmask)
      plot(nmask)
      p5_stats <- data.frame(computeFeatures.shape(nmask))
      p5_stats_2 <- data.frame(computeFeatures.moment(nmask))
      area_line_5 <- data.frame(p5_stats$s.area, p5_stats_2$m.eccentricity)
      sel  <- which(area_line_5[, "p5_stats.s.area"] < 500)
      xe <- rmObjects(nmask, sel)
      plot(xe)
      xe
    })
    
    #Create an object-identified image from the mask. 
    upload_obj_2 <- reactive({
      xe <- upload_mask_2()
      cols = c('black', sample(rainbow(max(xe))))
      zrainbow = Image(cols[1+xe], dim=dim(xe))
      zrainbow
    })
    
    #Create a dataframe with the quantification information.
    upload_results_2 <- reactive({
      xe <- upload_mask_2()
      num_RGCs <- data.frame(computeFeatures.shape(xe))
      num_RGCs <- nrow(num_RGCs)
      Num_RGCs_per_FOV <- num_RGCs
      data.frame(Num_RGCs_per_FOV)
    })
    
    #Output the original image. 
    output$img_2 <- renderPlot({ 
      validate(need(input$submit_2, "Please upload an image."))
      plot(upload_image_2())
    })
    
    #Output the masked image. 
    output$mask_2 <- renderPlot({ 
      validate(need(input$submit_2, "Please upload an image."))
      plot(upload_mask_2())
    })
    
    #Output the object-identified image. 
    output$obj_2 <- renderPlot({ 
      validate(need(input$submit_2, "Please upload an image."))
      plot(upload_obj_2())
    })
    
    #Output the quantification information in a data table for the user. 
    output$results_2 <- renderDataTable({ 
      validate(need(input$submit_2, "Please upload an image."))
      datatable(
        upload_results_2(),
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,  # Enable horizontal scrolling
          pageLength = 4,  # Show 4 rows per page
          autoWidth = FALSE, # Automatically adjust column widths
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          ordering = TRUE,
          dom = 'tB',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )
    })
  })
  
  #Below is output code that is repeated from above for the Shiny to function properly. 
  output$img_2 <- renderPlot({ 
    validate(need(input$submit_2, "Please upload an image."))
    plot(upload_image_2())
  })
  
  output$mask_2 <- renderPlot({ 
    validate(need(input$submit_2, "Please upload an image."))
    plot(upload_mask_2())
  })
  
  output$obj_2 <- renderPlot({ 
    validate(need(input$submit_2, "Please upload an image."))
    plot(upload_obj_2())
  })
  
  output$results_2 <- renderDataTable({ 
    validate(need(input$submit_2, "Please upload an image."))
    datatable(
      upload_results_2(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
  
  #### Rbpms Multiple File Quantification Tab #####################################################################################################################################
  
  #Helper functions
  #Function to upload image and create masked image. 
  upload_mask_rgcs <- function(image_to_process) {
    nmask <- image_to_process
    nmask = thresh(nmask, w=50, h= 50, offset= 0.03)
    nmask = opening(nmask, makeBrush(3, shape='disc'))
    nmask = bwlabel(nmask)
    p5_stats <- data.frame(computeFeatures.shape(nmask))
    p5_stats_2 <- data.frame(computeFeatures.moment(nmask))
    area_line_5 <- data.frame(p5_stats$s.area, p5_stats_2$m.eccentricity)
    sel  <- which(area_line_5[, "p5_stats.s.area"] < 500)
    xe <- rmObjects(nmask, sel)
    return(xe)
  }
  
  #Function to create the object-identified image from the mask.
  upload_obj_rgcs <- function(masked_image) {
    cols = c('black', sample(rainbow(max(masked_image))))
    zrainbow = Image(cols[1+masked_image], dim=dim(masked_image))
    return(zrainbow)
  }
  
  #Function to quantifiy the object-identified image and put results into a data table.
  upload_results_rgcs <- function(masked_image, image_name) {
    num_RGCs <- data.frame(computeFeatures.shape(masked_image))
    num_RGCs <- nrow(num_RGCs)
    Num_RGCs_per_FOV <- num_RGCs
    num_rgcs_to_column <- c(Num_RGCs_per_FOV)
    num_rgcs_to_column_dataframe <- data.frame(value = unlist(num_rgcs_to_column))
    names(num_rgcs_to_column_dataframe) <- c(image_name)
    return(num_rgcs_to_column_dataframe)
  }
  
  #Function for the submit button to process uploaded images. 
  output_table_cj_Rbpms <- eventReactive(input$submit_multiple_Rbpms, {
    req(input$file_multiple_Rbpms)  #Only proceed if files are uploaded.
    
    #Initialize two lists to store the new images and their name.
    new_images <- list()
    new_names <- list()
    
    #Loop through each uploaded file to get the image and its name.
    for (i in seq_along(input$file_multiple_Rbpms$datapath)) {
      #Read each uploaded image and their name.
      new_image <- readImage(input$file_multiple_Rbpms$datapath[i])
      new_name <- input$file_multiple_Rbpms$name[i]
      #Add the image and name to the list of new images and names, respectively.
      new_images[[i]] <- new_image
      new_names[[i]] <- new_name
    }
    
    #Creates the table with all the values of interest.
    images <- new_images
    image_names <- new_names
    
    #Ensures images have been uploaded.
    req(images, length(images) > 0)
    
    #Creates a dataframe with 4 rows to which we will append our data.
    num_rcgs <- c("Num_RGCs_per_FOV")
    result_table <- data.frame(number_of_rgcs_per = num_rcgs)
    
    #Creates a loading bar.
    withProgress(message = 'Processing...', value = 0, {
      
      #Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
      steps <- length(images)
      counter <- as.integer(1)
      
      #Generate table for each image in the list.
      for (image in images) {
        # Increments the progress bar.
        incProgress(1/steps, detail = paste("Image", counter, "of", steps))
        current_image_masked <- upload_mask_rgcs(image)
        results_current <- upload_results_rgcs(current_image_masked, image_names[counter])
        # Add the created table to the final table.
        result_table <- cbind(result_table, results_current)
        counter <- counter + 1
      }
    })
    result_table
  })

  output$results_multiple_Rbpms <- renderDT({
    #Use datatable() to display and make the table interactive.
    datatable(
      ##result_table,
      output_table_cj_Rbpms(),
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,  # Enable horizontal scrolling
        pageLength = 4,  # Show 4 rows per page
        autoWidth = FALSE, # Automatically adjust column widths
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  })
}
