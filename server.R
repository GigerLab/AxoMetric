
#Load necessary packages.
library(shiny)
library(shinyjs)
library(EBImage)
library(jpeg)
library(ggplot2)
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
library(shinycssloaders)
library(plotly)

options(shiny.maxRequestSize=100000*1024^2)

#Server function.
server <- function(input, output) {

#### Placeholders #####################################################################################################################################
    ## All these lines loads example images as placeholder for each page.

    ## RGC Quantification #1 placeholder.
    output$pdf <- renderPlot({
        rbpms <- readImage("RBPMS.tif")
        plot(rbpms)
    })

    ## RGC Quantification #2 placeholder.
    output$pdf_1 <- renderPlot({
        rbpms <- readImage("RBPMS.tif")
        plot(rbpms)
    })

    ## Total Mean Fluorescence Intensity (MFI) Quantification placeholder.
    output$pdf_2 <- renderPlot({
        PNS <- suppressWarnings({readImage("PNS.tif")})
        plot(PNS)
    })

    ## Normalized Mean Fluorescence Intensity (MFI) Quantification -> SCG10 Example placeholder.
    output$pdf_3 <- renderPlot({
        PNS_2 <- suppressWarnings({readImage("PNS_2.tif")})
        plot(PNS_2)
    })

    ## Normalized Mean Fluorescence Intensity (MFI) Quantification -> Hoechst Example placeholder. 
    output$pdf_4 <- renderPlot({
        PNS_3 <- suppressWarnings({readImage("PNS_2_DAPI.tif")})
        plot(PNS_3)
    })

    ## Normalized Mean Fluorescence Intensity (MFI) Quantification -> SCG10 Example placeholder.
    output$pdf_33 <- renderPlot({
        PNS_2 <- suppressWarnings({readImage("PNS_2.tif")})
        plot(PNS_2)
    })

    ## Total Axon Quantification and Total Axon Quantification for Multiple Files placeholder.
    output$pdf_5 <- renderPlot({
        ON <- readImage("Optic_Nerve.tif")
        plot(ON)
    })

    ## Normalized Axon Quantification -> CTB Example placeholder.
    output$pdf_6 <- renderPlot({
        ON_2 <- readImage("Optic_Nerve_2.tif")
        plot(ON_2)
    })

    ## Normalized Axon Quantification -> Hoechst Example placeholder.
    output$pdf_7 <- renderPlot({
        ON_3 <- readImage("Optic_Nerve_2_Hoechst.tif")
        plot(ON_3)
    })

    ## Total Axon Quantification and Total Axon Quantification for Multiple Files #2 placeholder.
    output$pdf_8 <- renderPlot({
        ON <- readImage("Optic_Nerve.tif")
        plot(ON)
    })

#### Total Axon Quantification Tab #####################################################################################################################################

### Helper functions for Axon Quntification
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

    process_image_dapi <- function(image){
        kern <- makeBrush(5, shape='gaussian')
        eidilat <- dilate(image, kern)
        nmask <- thresh(eidilat, w=5, h= 15, offset= 0.1)
        nmask <- opening(nmask, makeBrush(1, shape='line'))
        nmask <- bwlabel(nmask)
        nmask
    }

    ## Function to colorize image.
    colorize_image <- function(mask){
        cols <- c('black', sample(rainbow(max(mask))))
        colored_img <- Image(cols[1 + mask], dim = dim(mask))
        colored_img
    }

    ## Function to imply axons per nerve
    calculate_axons_per_nerve <- function(count) {
        as.integer(round(3.14159 * (0.3/2)^2 * (count / 0.3) / 0.014))
    }

    ## Helper Function for Interval Processing
    process_interval <- function(mask, start_row, end_row) {
        interval_img <- mask[as.integer(start_row):as.integer(end_row), ]
        if (max(interval_img, na.rm = TRUE) == 0) {
            return(list(count = 0))
        }
        stats <- data.frame(computeFeatures.shape(interval_img))
        nrow(stats)
    }

    process_interval_dapi <- function(mask, start_row, end_row, mtpr) {
        interval_img <- mask[as.integer(start_row):as.integer(end_row), ]
        if (max(interval_img, na.rm = TRUE) == 0) {
            return(list(count = 0))
        }
        stats <- data.frame(computeFeatures.moment(interval_img))
        dif_second <- max(stats$m.cy) - min(stats$m.cy)
        round((((dif_second * (1/(mtpr))) * 25400) / 50000000))
    }

###### Reactive value container to hold the list made by observeEvent
###### This is the only thing that needs to be zeroed on the user reset
    processed_data <- reactiveVal(NULL)
    observeEvent(input$reset, {
        processed_data(NULL)
    })

###### Upload image can trigger as soon as the image is there
    ## Reactive to create initial image.
    upload_image <- reactive({
        req(input$file1)
        validate(need(input$file1, "Please upload an image."))
        ext <- tools::file_ext(input$file1$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        img <- suppressWarnings({
            readImage(input$file1$datapath)
        })
        return(img)
    })

###### One reactive to process all plots and final table
###### Run any time the user pushes the input$submit button
    observeEvent(input$submit, {
        validate(need(input$num_intervals > 0, "Please enter a valid number of intervals (1 or more)."))

        ## Create the mask (Only need to do once)
        img <- upload_image()
        mask <- process_image(img)
        width <- dim(mask)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        ## Process Intervals and Aggregate Results
        axon_counts <- mapply(
            FUN = process_interval,
            start_row = interval_starts,
            end_row = interval_ends,
            MoreArgs = list(mask = mask),
            SIMPLIFY = TRUE
        ) %>% unlist()

        axons_per_nerve <- calculate_axons_per_nerve(axon_counts)

        ## Create the final summary table dynamically
        quant_info_df <- data.frame(
            Distance_from_Injury_Site = paste("Interval", 1:n_intervals),
            Number_axons_per_section = axon_counts,
            Number_axons_per_nerve = axons_per_nerve
        )

        ## Put these all into a list to be used for the outputs
        results_list <- list(
            original_image = img,
            full_mask = mask,
            colorized_mask = colorize_image(mask),
            interval_starts = interval_starts,
            interval_ends = interval_ends,
            results_table = quant_info_df
        )
        ## Save the list into the reactive value we set earlier
        processed_data(results_list)
    })

    ## Output to plot entire initial image.
    output$img <- renderPlot({
        req(processed_data())
        plot(processed_data()$original_image)
    })

    ## Output to plot entire initial mask.
    output$mask <- renderPlot({
        req(processed_data())
        plot(processed_data()$full_mask)
    })

    ## Output to plot entire initial object-identified image.
    output$obj <- renderPlot({
        req(processed_data())
        plot(processed_data()$colorized_mask)
    })

    output$obj_with_intervals <- renderPlot({
        req(processed_data())
        data <- processed_data()

        ## Plot the base colorized image
        plot(data$colorized_mask)

        img_dims <- dim(data$colorized_mask)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Output to plot data table with quantification information.
    output$results <- renderDataTable({
        req(processed_data())
        datatable(
            processed_data()$results_table,
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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

    ## Function to quantify multiple images at once at four intervals and create a data table output with the results.
    upload_results <- function(image_to_process, image_name) {
        validate(need(input$num_intervals_2 > 0, "Please enter a valid number of intervals (1 or more)."))

        mask <- process_image(image_to_process)
        width <- dim(mask)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_2

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        ## Process Intervals and Aggregate Results
        axon_counts <- mapply(
            FUN = process_interval,
            start_row = interval_starts,
            end_row = interval_ends,
            MoreArgs = list(mask = mask),
            SIMPLIFY = TRUE
        ) %>% unlist()

        axons_per_nerve <- calculate_axons_per_nerve(axon_counts)

        ## Create the final summary table dynamically
        quant_info_df <- data.frame(
            Number_axons_per_section = axon_counts,
            Number_axons_per_nerve = axons_per_nerve
        )

        return(quant_info_df)
    }

    ## Observe the submit button click event.
    output_table_cj <- eventReactive(input$submit_multiple_files, {
        validate(need(input$num_intervals_2 > 0, "Please enter a valid number of intervals (1 or more)."))
        req(input$list_multiple_files_axon_quantification)  # Only proceed if files are uploaded.
        ext <- tools::file_ext(input$list_multiple_files_axon_quantification$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        ## Initialize two lists to store the new images and their name.
        new_images <- list()
        new_names <- list()

        ##Loop through each uploaded file to get the image and its name.
        for (i in seq_along(input$list_multiple_files_axon_quantification$datapath)) {
            ##Read each uploaded image and their name.
            new_image <- readImage(input$list_multiple_files_axon_quantification$datapath[i])
            new_name <- input$list_multiple_files_axon_quantification$name[i]
            ##Add the image and name to the list of new images and names, respectively.
            new_images[[i]] <- new_image
            new_names[[i]] <- new_name
        }

        ##Creates the table with all the values of interest.
        images <- new_images
        image_names <- new_names

        ##Ensures images have been uploaded.
        req(images, length(images) > 0)

        ## Creates a dataframe with rows to which we will append our data.
        n_intervals <- input$num_intervals_2
        Distance_from_Injury_Site <- paste("Interval", 1:n_intervals)
        result_table <- data.frame(distance_injury_site = Distance_from_Injury_Site)

        ## Creates a loading bar.
        withProgress(message = 'Processing...', value = 0, {
            ## Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
            steps <- length(images)
            counter <- as.integer(1)

            ## Generate table for each image in the list.
            for (image in images) {
                ## Increments the progress bar.
                incProgress(1/steps, detail = paste("Image", counter, "of", steps))
                ## Processes the image and creates the table.
                results_current <- upload_results(image, image_names[counter])
                ## Add the created table to the final table.
                result_table <- cbind(result_table, results_current)
                counter <- counter + 1
            }
        })
        result_table
    })
    output$results_multiple_files <- renderDT({
        ## Use datatable() to display and make the table interactive.
        datatable(
            output_table_cj(),
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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

###### Now that we have all the plot info in a list it is only that list we need to zero out to reset.
###### This is a new reactive value container to hold that list made by the observeEvent
    Norm_Axon_Quant_data <- reactiveVal(NULL)

    observeEvent(input$reset_4, {
        Norm_Axon_Quant_data(NULL)
    })

    upload_image_4 <- reactive({
        req(input$file4)
        validate(need(input$file4, "Please upload an image."))
        ext <- tools::file_ext(input$file4$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        img <- suppressWarnings({
            readImage(input$file4$datapath)
        })
        return(img)
    })

###### One reactive to process all plots and final table
###### Run any time the user pushes the input$submit_4 button
    observeEvent(input$submit_4, {

        img <- upload_image_4()
        mask <- process_image(img)
        width <- dim(mask)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_4

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        ## Put these all into a list to be used for the outputs
        results_list <- list(
            img_4 = img,
            mask_4 = mask,
            colorized_mask = colorize_image(mask),
            interval_starts = interval_starts,
            interval_ends = interval_ends
        )

        ## Save the list into the reactive value we set earlier
        Norm_Axon_Quant_data(results_list)
    })

    ## Output the original image.
    output$img_4 <- renderPlot({
        req(Norm_Axon_Quant_data())
        plot(Norm_Axon_Quant_data()$img_4)
    })

    ## Output the masked image.
    output$mask_4 <- renderPlot({
        req(Norm_Axon_Quant_data())
        plot(Norm_Axon_Quant_data()$mask_4)
    })

    ## Output the original image.
    output$obj_4 <- renderPlot({
        req(Norm_Axon_Quant_data())
        plot(Norm_Axon_Quant_data()$colorized_mask)
    })

    output$obj_with_intervals_4 <- renderPlot({
        req(Norm_Axon_Quant_data())
        data <- Norm_Axon_Quant_data()

        ## Plot the base colorized image
        plot(data$colorized_mask)

        img_dims <- dim(data$colorized_mask)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Upload the original nuclear-stained image.
    Nuc_Stain_data <- reactiveVal(NULL)

    observeEvent(input$reset_4, {
        Nuc_Stain_data(NULL)
    })

    upload_image_5 <- reactive({
        req(input$file5)
        validate(need(input$file5, "Please upload an image."))
        ext <- tools::file_ext(input$file5$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        readImage(input$file5$datapath)
    })

    observeEvent(input$submit_4, {

        img <- upload_image_5()
        mask <- process_image_dapi(img)
        width <- dim(mask)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_4

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        results_list <- list(
            img_5 = img,
            mask_5 = mask,
            colorized_mask = colorize_image(mask),
            interval_starts = interval_starts,
            interval_ends = interval_ends
            )

        ## Save the list into the reactive value we set earlier
        Nuc_Stain_data(results_list)
    })

    ## Output the original nuclear-stained image.
    output$img_5 <- renderPlot({
        req(Nuc_Stain_data())
        plot(Nuc_Stain_data()$img_5)
    })

    ## Output the masked nuclear-stained image.
    output$mask_5 <- renderPlot({
        req(Nuc_Stain_data())
        plot(Nuc_Stain_data()$mask_5)
    })

    ## Output the object-identified nuclear-stained image.
    output$obj_5 <- renderPlot({
        req(Nuc_Stain_data())
        plot(Nuc_Stain_data()$colorized_mask)
    })

    output$obj_with_intervals_5 <- renderPlot({
        req(Nuc_Stain_data())
        data <- Nuc_Stain_data()

        ## Plot the base colorized image
        plot(data$colorized_mask)

        img_dims <- dim(data$colorized_mask)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Reactive function to quantify the number of axons at each interval and normalize this to the nerve width, and then create a data output table with the results.
    upload_results_4 <- reactive({

        validate(need(input$submit_4, "Please upload an image."))
        req(Norm_Axon_Quant_data())
        mask <- Norm_Axon_Quant_data()$mask_4
        interval_starts <- Norm_Axon_Quant_data()$interval_starts
        interval_ends <- Norm_Axon_Quant_data()$interval_ends

        axon_counts <- mapply(
            FUN = process_interval,
            start_row = interval_starts,
            end_row = interval_ends,
            MoreArgs = list(mask = mask),
            SIMPLIFY = TRUE
        ) %>% unlist()

        axons_per_nerve <- calculate_axons_per_nerve(axon_counts)

        ## Upload original image and create object-identified image.
        ## Need the new intervals because of stiching differences?
        dapi_mask <- Nuc_Stain_data()$mask_5

        interval_starts <- Nuc_Stain_data()$interval_starts
        interval_ends <- Nuc_Stain_data()$interval_ends

        nerve_width <- mapply(
            FUN = process_interval_dapi,
            start_row = interval_starts,
            end_row = interval_ends,
            mtpr = input$mtpr,
            MoreArgs = list(mask = dapi_mask),
            SIMPLIFY = TRUE
        ) %>% unlist()

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_4

        ## Create the final summary table dynamically
        quant_info_df <- data.frame(
            Distance_from_Injury_Site = paste("Interval", 1:n_intervals),
            Number_axons_per_section = axon_counts,
            Number_axons_per_nerve = axons_per_nerve,
            Nerve_Width_in_Micrometers = nerve_width,
            Number_Axons_per_Width_Micrometers = round(axon_counts / nerve_width, 4)
        )
        quant_info_df
    })

    ## Output quantification information results into data table for user.
    output$results_4 <- renderDataTable({
        validate(need(input$submit_4, "Please upload an image."))
        datatable(
            upload_results_4(),
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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
#####################################################################################################################################################################################################
#### Total Mean Fluorescence Intensity (MFI) Quantification Tab #####################################################################################################################################
#####################################################################################################################################################################################################
    process_mfi_interval <- function(mask, start_row, end_row) {
        interval_img <- mask[as.integer(start_row):as.integer(end_row), ]
        if (max(interval_img, na.rm = TRUE) == 0) {
            return(list(count = 0))
        }
        mean(interval_img)
    }

###### Reactive value container to hold the list made by observeEvent
###### This is the only thing that needs to be zeroed on the user reset
    MFI_Quant_data <- reactiveVal(NULL)
    observeEvent(input$reset_3, {
        MFI_Quant_data(NULL)
    })

###### Upload image can trigger as soon as the image is there
    ##Reactive to create initial image.

    upload_image_3 <- reactive({
        req(input$file3)
        validate(need(input$file3, "Please upload an image."))
        ext <- tools::file_ext(input$file3$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        img <- suppressWarnings({
            readImage(input$file3$datapath)
        })
        return(img)
    })

    ## Function for submit button to process uploaded images for quantification.
    observeEvent(input$submit_3, {

        img <- upload_image_3()
        width <- dim(img)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_3

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        average_MFI <- mapply(
            FUN = process_mfi_interval,
            start_row = interval_starts,
            end_row = interval_ends,
            MoreArgs = list(mask = img),
            SIMPLIFY = TRUE
        ) %>% unlist()
        quant_info_df <- data.frame(
            Distance_from_Injury_Site = paste("Interval", 1:n_intervals),
            Average_MFI = round(average_MFI, 8)
        )

        ## Put these all into a list to be used for the outputs
        results_list <- list(
            img_3 = img,
            interval_starts = interval_starts,
            interval_ends = interval_ends,
            results_table = quant_info_df
        )

        ## Save the list into the reactive value we set earlier
        MFI_Quant_data(results_list)
    })

    ## Output the original image.
    output$img_3 <- renderPlot({
        req(MFI_Quant_data())
        plot(MFI_Quant_data()$img_3)
    })

    output$obj_with_intervals_3 <- renderPlot({
        req(MFI_Quant_data())
        data <- MFI_Quant_data()

        ## Plot the base colorized image
        plot(data$img_3)

        img_dims <- dim(data$img_3)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Output the quantification information into a data table for the user.
    output$results_3 <- renderDataTable({
        validate(need(input$submit_3, "Please upload an image."))
        datatable(
            MFI_Quant_data()$results_table,
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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

    ## Helper function
    ## Function to process and quantify each image at four intervals.
    upload_results_mfi <- function(image_to_process, image_name) {
        ## ## Create the first interval of original image and calculate the MFI.
        img <- image_to_process
        width <- dim(img)[1]

        n_intervals <- input$num_intervals_multi_mfi

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]

        average_MFI <- mapply(
            FUN = process_mfi_interval,
            start_row = interval_starts,
            end_row = interval_ends,
            MoreArgs = list(mask = img),
            SIMPLIFY = TRUE
        ) %>% unlist()

        data.frame(
            Average_MFI = round(average_MFI, 8)
        )
    }

    ## Observe the submit button click event.
    output_table_cj_MFI <- eventReactive(input$submit_multiple_files_mfi , {
        req(input$list_multiple_mfi)  # Only proceed if files are uploaded.
        ext <- tools::file_ext(input$list_multiple_mfi$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        ## Initialize two lists to store the new images and their name.
        new_images <- list()
        new_names <- list()

        ## Loop through each uploaded file to get the image and its name.
        for (i in seq_along(input$list_multiple_mfi$datapath)) {
            ## Read each uploaded image and their name.
            new_image <- readImage(input$list_multiple_mfi$datapath[i])
            new_name <- input$list_multiple_mfi$name[i]
            ## Add the image and name to the list of new images and names, respectively.
            new_images[[i]] <- new_image
            new_names[[i]] <- new_name
        }

        ## Creates the table with all the values of interest.
        images <- new_images
        image_names <- new_names

        ## Ensures images have been uploaded.
        req(images, length(images) > 0)

        n_intervals <- input$num_intervals_multi_mfi

        ## Creates a dataframe with 4 rows to which we will append our data.
        ## Distance_from_Injury_Site <- c("First Interval", "Second Interval", "Third Interval", "Fourth Interval")
        result_table <- data.frame(Distance_from_Injury_Site = paste("Interval", 1:n_intervals))

        ## Creates a loading bar.
        withProgress(message = 'Processing...', value = 0, {

            ## Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
            steps <- length(images)
            counter <- as.integer(1)

            ## Generate table for each image in the list.
            for (image in images) {
                ## Increments the progress bar.
                incProgress(1/steps, detail = paste("Image", counter, "of", steps))
                ## Processes the image and creates the table.
                results_current <- upload_results_mfi(image, image_names[counter])
                ## Add the created table to the final table.
                result_table <- cbind(result_table, results_current)
                counter <- counter + 1
            }
        })
        result_table
    })

    output$results_multiple_files_mfi <- renderDT({
        ## Use datatable() to display and make the table interactive.
        datatable(
            output_table_cj_MFI(),
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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

###### This is a new reactive value container to hold that list made by the observeEvent
    Norm_MFI_Quant_data <- reactiveVal(NULL)

    observeEvent(input$reset_5, {
        Norm_MFI_Quant_data(NULL)
    })

    upload_image_6 <- reactive({
        req(input$file6)
        validate(need(input$file6, "Please upload an image."))
        ext <- tools::file_ext(input$file6$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        ## Suppress the specific TIFF tag warnings during read
        img <- suppressWarnings({
            readImage(input$file6$datapath)
        })
        return(img)

    })

###### One reactive to process all plots and final table
###### Run any time the user pushes the input$submit_5 button
  observeEvent(input$submit_5, {
      mask <- upload_image_6()
      width <- dim(mask)[1]

      n_intervals <- input$num_intervals_6

      ## Define Interval Boundaries Dynamically
      ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
      end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
      slice_width <- (width / n_intervals) / 3
      start_rows <- end_rows - slice_width
      ## The start of each interval is the first to the second-to-last break point.
      interval_starts <- start_rows[2:(n_intervals + 1)]
      ## The end of each interval is the second to the last break point.
      interval_ends <- end_rows[2:(n_intervals + 1)]

      results_list <- list(
          img_6 = mask,
          interval_starts = interval_starts,
          interval_ends = interval_ends
      )
      ## Save the list into the reactive value we set earlier
      Norm_MFI_Quant_data(results_list)
  })

    ## Output the original image.
    output$img_6 <- renderPlot({
        req(Norm_MFI_Quant_data())
        plot(Norm_MFI_Quant_data()$img_6)
    })

    output$obj_with_intervals_6 <- renderPlot({
        req(Norm_MFI_Quant_data())
        data <- Norm_MFI_Quant_data()

        ## Plot the base colorized image
        plot(data$img_6)

        img_dims <- dim(data$img_6)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Upload the original nuclear-stained image.
    Nuc_Stain_MFI_data <- reactiveVal(NULL)

    observeEvent(input$reset_5, {
        Nuc_Stain_MFI_data(NULL)
    })

    ## Reactive function to upload the nuclear-stained image.
    upload_image_7 <- reactive({
        req(input$file7)
        validate(need(input$file7, "Please upload an image."))
        ext <- tools::file_ext(input$file7$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        ## Suppress the specific TIFF tag warnings during read
        img <- suppressWarnings({
            readImage(input$file7$datapath)
        })
        return(img)
    })

    observeEvent(input$submit_5, {
        ## Process and quantify the first interval of the nuclear-stained image.
        img <- upload_image_7()
        mask <- process_image_dapi(img)
        width <- dim(mask)[1]

        ## Get the number of intervals from the UI
        n_intervals <- input$num_intervals_6

        ## Define Interval Boundaries Dynamically
        ## Create n+1 equally spaced points that will serve as the ends of our n intervals.
        end_rows <- seq(from = 1, to = width, length.out = n_intervals + 1)
        slice_width <- (width / n_intervals) / 3
        start_rows <- end_rows - slice_width
        ## The start of each interval is the first to the second-to-last break point.
        interval_starts <- start_rows[2:(n_intervals + 1)]
        ## The end of each interval is the second to the last break point.
        interval_ends <- end_rows[2:(n_intervals + 1)]
        results_list <- list(
            nmask_7 = mask,
            interval_starts = interval_starts,
            interval_ends = interval_ends
        )
        Nuc_Stain_MFI_data(results_list)
    })

    ## Output the original nuclear-stained image.
    output$img_7 <- renderPlot({
        req(Nuc_Stain_MFI_data())
        plot(Nuc_Stain_MFI_data()$nmask_7)
    })

    output$obj_with_intervals_7 <- renderPlot({
        req(Nuc_Stain_MFI_data())
        data <- Nuc_Stain_MFI_data()

        img <- data$nmask_7
        img_moments <- data.frame(computeFeatures.moment(img))
        yy_2 <- max(img_moments$m.cy)
        yy_1 <- min(img_moments$m.cy)
        img <- colorize_image(img[,yy_1:yy_2])
        ## Plot the base colorized image
        plot(img)

        img_dims <- dim(img)
        B <- img_dims[2]

        ## Loop through the stored intervals and draw a rectangle for each one
        for (i in 1:length(data$interval_starts)) {
            rect(
                xleft = data$interval_starts[i],
                ybottom = 1,
                xright = data$interval_ends[i],
                ytop = B,
                border = "yellow",              # A highly visible color for the border
                lwd = 2,                        # Line width
                col = rgb(1, 1, 0, alpha = 0.1) # Add a slightly transparent yellow fill
            )
        }
    })

    ## Process and quantify both the original and nuclear-stained image at each of the four intervals.
    upload_results_5 <- reactive({

        validate(need(input$submit_5, "Please upload an image."))
        req(input$num_intervals_6 > 0)
        req(Norm_MFI_Quant_data())
        req(Nuc_Stain_MFI_data())

        n_intervals <- input$num_intervals_6

        img <- Norm_MFI_Quant_data()$img_6
        interval_starts <- Norm_MFI_Quant_data()$interval_starts
        interval_ends <- Norm_MFI_Quant_data()$interval_ends

        dapi_img <- Nuc_Stain_MFI_data()$nmask_7
        dapi_interval_starts <- Nuc_Stain_MFI_data()$interval_starts
        dapi_interval_ends <- Nuc_Stain_MFI_data()$interval_ends
        kern <- makeBrush(5, shape='gaussian')
        dapi_mask <- dilate(dapi_img, kern)
        dapi_mask <- thresh(dapi_mask, w=5, h= 15, offset= 0.1)
        dapi_mask <- opening(dapi_mask, makeBrush(1, shape='line'))
        dapi_mask <- bwlabel(dapi_mask)

        ### Cannot Use the MFI calculator from the Absolute MFI tab because this one uses the nuclear stain to determine the region to calculate in
        process_mfi_interval_with_dapi_limits <- function(mask, start_row, end_row, dapi_min, dapi_max) {
            interval_img <- mask[as.integer(start_row):as.integer(end_row), ]
            if (max(interval_img, na.rm = TRUE) == 0) {
                return(list(count = 0))
            }
            interval_img <- interval_img[,dapi_min:dapi_max]
            mean(interval_img)
        }
        process_dapi_limits <- function(mask, start_row, end_row, dapi_min, dapi_max) {
            interval_img <- mask[as.integer(start_row):as.integer(end_row), ]
            if (max(interval_img, na.rm = TRUE) == 0) {
                return(list(count = 0))
            }
            stats <- data.frame(computeFeatures.moment(interval_img))
            c(min(stats$m.cy), max(stats$m.cy))
        }

        dapi_limits <- mapply(
            FUN = process_dapi_limits,
            start_row = dapi_interval_starts,
            end_row = dapi_interval_ends,
            MoreArgs = list(mask = dapi_mask),
            SIMPLIFY = FALSE
        )

        mean_MFIs <- NULL
        nerve_width <- NULL
        avg_nerve <- NULL
        for(i in 1:n_intervals){
            ## Average_MFI_per_Section
            avg <- process_mfi_interval_with_dapi_limits(img,
                                                         interval_starts[i],
                                                         interval_ends[i],
                                                         dapi_limits[[i]][1],
                                                         dapi_limits[[i]][2])
            mean_MFIs <- c(mean_MFIs, round(avg, 7))
            ## Nerve_Width_in_Micrometers
            width <- dapi_limits[[i]][2] - dapi_limits[[i]][1]
            width_in_micrometers <- round((((width * (1/(.004))) * 25400) / 50000000))
            nerve_width <- c(nerve_width, width_in_micrometers)
            ## Average_MFI_per_Width_Micrometer
            per_nerve <- round(avg / width_in_micrometers, 7)
            avg_nerve <- c(avg_nerve, per_nerve)
        }

        Distance_from_Injury_Site = paste("Interval", 1:n_intervals)
        Average_MFI_per_Section <- mean_MFIs
        Nerve_Width_in_Micrometers <- nerve_width
        Average_MFI_per_Width_Micrometer <- avg_nerve
        df <- data.frame(Distance_from_Injury_Site, Average_MFI_per_Section, Nerve_Width_in_Micrometers, Average_MFI_per_Width_Micrometer)
        df
    })

    ## Output a data table with the quantification information to the user.
    output$results_5 <- renderDataTable({
        validate(need(input$submit_5, "Please upload an image."))
        datatable(
            upload_results_5(),
            extensions = 'Buttons',
            options = list(
                scrollX = TRUE,  # Enable horizontal scrolling
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
    Rbpms_data <- reactiveVal(NULL)

    observeEvent(input$reset_2, {
        Rbpms_data(NULL)
    })

    ## Upload the original image.
    upload_image_2 <- reactive({
        req(input$file2)
        validate(need(input$file2, "Please upload an image."))
        ext <- tools::file_ext(input$file2$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )
        readImage(input$file2$datapath)
    })

    ## Function for the submit button to process and quantify images.
    observeEvent(input$submit_2, {
        ## Create a mask from the original image.
        nmask <- upload_image_2()
        nmask = thresh(nmask, w=50, h= 50, offset= 0.03)
        nmask = opening(nmask, makeBrush(3, shape='disc'))
        nmask = bwlabel(nmask)
        p5_stats <- data.frame(computeFeatures.shape(nmask))
        p5_stats_2 <- data.frame(computeFeatures.moment(nmask))
        area_line_5 <- data.frame(p5_stats$s.area, p5_stats_2$m.eccentricity)
        sel  <- which(area_line_5[, "p5_stats.s.area"] < 500)
        xe <- rmObjects(nmask, sel)

        ## Create an object-identified image from the mask.
        cols = c('black', sample(rainbow(max(xe))))
        zrainbow = Image(cols[1+xe], dim=dim(xe))

        ## Create a dataframe with the quantification information.
        num_RGCs <- data.frame(computeFeatures.shape(xe))
        num_RGCs <- nrow(num_RGCs)

        results_list <- list(
            img_2 = upload_image_2(),
            mask_2 = nmask,
            obj_2 = zrainbow,
            num_RGCs = data.frame(num_RGCs)
        )
        ## Save the list into the reactive value we set earlier
        Rbpms_data(results_list)
    })

    ## Output the original image.
    output$img_2 <- renderPlot({
        req(Rbpms_data())
        plot(Rbpms_data()$img_2)
    })

    ## Output the masked image.
    output$mask_2 <- renderPlot({
        req(Rbpms_data())
        plot(Rbpms_data()$mask_2)
    })

    ## Output the object-identified image.
    output$obj_2 <- renderPlot({
        req(Rbpms_data())
        plot(Rbpms_data()$obj_2)
    })

    ## Output the quantification information in a data table for the user.
    output$results_2 <- renderDataTable({
        validate(need(input$submit_2, "Please upload an image."))
        datatable(
            Rbpms_data()$num_RGCs,
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
    ## Helper functions
    ## Function to upload image and create masked image.
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

    ## Function to create the object-identified image from the mask.
    upload_obj_rgcs <- function(masked_image) {
        cols = c('black', sample(rainbow(max(masked_image))))
        zrainbow = Image(cols[1+masked_image], dim=dim(masked_image))
        return(zrainbow)
    }

    ## Function to quantifiy the object-identified image and put results into a data table.
    upload_results_rgcs <- function(masked_image, image_name) {
        num_RGCs <- data.frame(computeFeatures.shape(masked_image))
        num_RGCs <- nrow(num_RGCs)
        Num_RGCs_per_FOV <- num_RGCs
        num_rgcs_to_column <- c(Num_RGCs_per_FOV)
        num_rgcs_to_column_dataframe <- data.frame(value = unlist(num_rgcs_to_column))
        names(num_rgcs_to_column_dataframe) <- c(image_name)
        return(num_rgcs_to_column_dataframe)
    }

    ##Function for the submit button to process uploaded images.
    output_table_cj_Rbpms <- eventReactive(input$submit_multiple_Rbpms, {
        req(input$file_multiple_Rbpms)  #Only proceed if files are uploaded.
        ext <- tools::file_ext(input$file_multiple_Rbpms$name)
        message(paste0('Uploaded file extensions: ', paste(ext, collapse = ", ")))
        validate(
            need(all(tolower(ext) %in% c('tif', 'tiff')), "Upload failed. Please ensure all uploaded files are TIFFs.")
        )

        ## Initialize two lists to store the new images and their name.
        new_images <- list()
        new_names <- list()

        ## Loop through each uploaded file to get the image and its name.
        for (i in seq_along(input$file_multiple_Rbpms$datapath)) {
            ## Read each uploaded image and their name.
            new_image <- readImage(input$file_multiple_Rbpms$datapath[i])
            new_name <- input$file_multiple_Rbpms$name[i]
            ## Add the image and name to the list of new images and names, respectively.
            new_images[[i]] <- new_image
            new_names[[i]] <- new_name
        }

        ## Creates the table with all the values of interest.
        images <- new_images
        image_names <- new_names

        ## Ensures images have been uploaded.
        req(images, length(images) > 0)

        ## Creates a dataframe with 4 rows to which we will append our data.
        num_rcgs <- c("Num_RGCs_per_FOV")
        result_table <- data.frame(number_of_rgcs_per = num_rcgs)

        ## Creates a loading bar.
        withProgress(message = 'Processing...', value = 0, {

            ## Steps stores the number of images uploaded and counter stores the iteration at which we are in the for loop.
            steps <- length(images)
            counter <- as.integer(1)

            ## Generate table for each image in the list.
            for (image in images) {
                ## Increments the progress bar.
                incProgress(1/steps, detail = paste("Image", counter, "of", steps))
                current_image_masked <- upload_mask_rgcs(image)
                results_current <- upload_results_rgcs(current_image_masked, image_names[counter])
                ## Add the created table to the final table.
                result_table <- cbind(result_table, results_current)
                counter <- counter + 1
            }
        })
        result_table
    })

    output$results_multiple_Rbpms <- renderDT({
        ## Use datatable() to display and make the table interactive.
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
