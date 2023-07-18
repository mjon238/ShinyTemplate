library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(janitor)
library(openxlsx)
library(rrapply)
library(lubridate)
library(stringr)
library(plotly)
library(magrittr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(DT)
library(googlesheets4)

#Read all Data in

# Data Loading and Sorting ------------------------------------------------------------

load("Data/shiny_app_data.Rmd")
