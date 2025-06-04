
#Source the respective user interface and server R files.
source("UI.R")
source("server.R")

#Load in necessary packages.
library(shiny)
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

#Loading screen.
loading_screen <- function(){
  waiting_screen <- tagList(
    spin_loaders(12),
    h4("Loading AxoMetric...")
  )
  return(waiting_screen)
}

#Allocate memory to run.
memory.limit(200000)
options(shiny.maxRequestSize=300*1024^2)

#Load waiting screen and user interface.
waiting_screen <- loading_screen()
ui <- site_setup()

#Run Shiny App.
shinyApp(ui , server)
