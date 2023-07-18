library(tidyverse)
library(janitor)
library(openxlsx)
library(rrapply)
library(lubridate)

setwd("~/Current Uni Stuff/Work/Project_1_Shiny/Data")

df <- list(Taranaki22 = list(addressChange = NULL,
                             jobSupport = NULL,
                             foodGrants = NULL,
                             foodGrantsCost = NULL),
           
           SelectLocalities = list(jobSupport = NULL,
                                   foodGrants = NULL,
                                   foodGrantsCost = NULL,
                                   accomSup = NULL,
                                   accomSupCost = NULL),
           
           WairoaData = list(addressChange = NULL,
                             jobSupport = NULL,
                             foodGrants = NULL,
                             foodGrantsCost = NULL,
                             accomSup = NULL,
                             accomSupCost = NULL),
           
           WhanganuiData = list(addressChange = NULL,
                                jobSupport = NULL,
                                foodGrants = NULL,
                                foodGrantsCost = NULL,
                                accomSup = NULL,
                                accomSupCost = NULL)
)




# Part 1 Loading: Taranaki 2022-2023 --------------------------------------
#Read in the data
df$Taranaki22$addressChange <- readxl::read_xlsx("SLTA_Jan22_Jan23/INTERNAL_BIIM-2398_Change_of_Address_by_ethnicity_by_TLA.xlsx", range = "A17:L535")

df$Taranaki22$jobSupport <- readxl::read_xlsx("SLTA_Jan22_Jan23/INTERNAL_BIIM-2398_JS_Clients_by_TLA.xlsx", range = "A17:L617")
names(df$Taranaki22$jobSupport)[names(df$Taranaki22$jobSupport) == 'Age'] <- 'Age Group'
names(df$Taranaki22$jobSupport)[names(df$Taranaki22$jobSupport) == 'Total clients'] <- 'Total Clients'

df$Taranaki22$foodGrants <- readxl::read_xlsx("SLTA_Jan22_Jan23/INTERNAL_BIIM-2398_SNG_food_5_years_by_TLA.xlsx", range = "A19:N586")
names(df$Taranaki22$foodGrants)[names(df$Taranaki22$foodGrants) == 'Age'] <- 'Age Group'
names(df$Taranaki22$foodGrants)[names(df$Taranaki22$foodGrants) == 'Total Distinct Clients'] <- 'Total Clients'
df$Taranaki22$foodGrants$`Total Clients`[df$Taranaki22$foodGrants$`Total Clients` == "."] <- 1

df$Taranaki22$foodGrantsCost <- df$Taranaki22$foodGrants%>%
  mutate(averageGrant = `Total Granted`/`Total Grants`)%>%
  select(-c(`Total Clients`))%>%
  rename("Total Clients" = `Total Granted`)



# Part 2 Loading Select Localities ----------------------------------------

df$SelectLocalities$jobSupport <- readxl::read_xlsx("SLTA_Oct21_Sep22/INTERNAL_BIIM-1841_JS_5_years_by_TLA.xlsx", range = "A17:L849")
names(df$SelectLocalities$jobSupport)[names(df$SelectLocalities$jobSupport) == 'Total clients'] <- 'Total Clients'


df$SelectLocalities$foodGrants <- readxl::read_xlsx("SLTA_Oct21_Sep22/INTERNAL_BIIM-1894_SNG_food_5_years_by_TLA.xlsx", range = "A19:N873")
names(df$SelectLocalities$foodGrants)[names(df$SelectLocalities$foodGrants) == 'Total Distinct Clients'] <- 'Total Clients'

df$SelectLocalities$foodGrantsCost <- df$SelectLocalities$foodGrants%>%
  mutate(averageGrant = `Total Granted`/`Total Grants`)%>%
  select(-c(`Total Clients`))%>%
  rename("Total Clients" = `Total Granted`)

df$SelectLocalities$accomSup <- readxl::read_xlsx("SLTA_Oct21_Sep22/INTERNAL_BIIM-1893_current_AS_5_years_by_TLA.xlsx", range = "A17:M857")
names(df$SelectLocalities$accomSup)[names(df$SelectLocalities$accomSup) == 'Total clients'] <- 'Total Clients'

df$SelectLocalities$accomSupCost <- df$SelectLocalities$accomSup%>%
  mutate(`Monthly Accommodation Supplement` = `Weekly Accommodation Supplement`*(52/12))%>%
  mutate(averageSup = `Monthly Accommodation Supplement`/`Total Clients`)%>%
  select(-c(`Total Clients`, `Weekly Accommodation Supplement`))%>%
  rename("Total Clients" = `Monthly Accommodation Supplement`)



# Part 3 Loading Wairoa Data Jan 18 to Jan 23 -----------------------------

df$WairoaData$addressChange <- readxl::read_xlsx("Wairoa_Data_Jan18_23/INTERNAL_BIIM-2705_Change_of_Address_by_ethnicity.xlsx", range = "A17:L1778")

df$WairoaData$jobSupport <- readxl::read_xlsx("Wairoa_Data_Jan18_23/INTERNAL_BIIM-2705_JS_Clients.xlsx", range = "A17:L2627")
names(df$WairoaData$jobSupport)[names(df$WairoaData$jobSupport) == 'Total clients'] <- 'Total Clients'

df$WairoaData$foodGrants <- readxl::read_xlsx("Wairoa_Data_Jan18_23/INTERNAL_BIIM-2705_SNG_food_5_years.xlsx", range = "A19:N1772")

names(df$WairoaData$foodGrants)[names(df$WairoaData$foodGrants) == 'Total Distinct Clients'] <- 'Total Clients'

#Remove Incomplete Observations
nas <- which(df$WairoaData$foodGrants$`Total Clients` == ".")
df$WairoaData$foodGrants <- df$WairoaData$foodGrants[-nas,]

df$WairoaData$foodGrantsCost <- df$WairoaData$foodGrants%>%
  mutate(averageGrant = `Total Granted`/`Total Grants`)%>%
  select(-c(`Total Clients`))%>%
  rename("Total Clients" = `Total Granted`)

df$WairoaData$accomSup <- readxl::read_xlsx("Wairoa_Data_Jan18_23/INTERNAL_BIIM-2705_current_AS_5_years.xlsx", range = "A17:M2461")
names(df$WairoaData$accomSup)[names(df$WairoaData$accomSup) == 'Total clients'] <- 'Total Clients'

df$WairoaData$accomSupCost <- df$WairoaData$accomSup%>%
  mutate(`Monthly Accommodation Supplement` = `AS Weekly`*(52/12))%>%
  mutate(averageSup = `Monthly Accommodation Supplement`/`Total Clients`)%>%
  select(-c(`Total Clients`, `AS Weekly`))%>%
  rename("Total Clients" = `Monthly Accommodation Supplement`)



# Part 4 Loading Whanganui Data -------------------------------------------

df$WhanganuiData$addressChange <- readxl::read_xlsx("Whanganui_Data_Jan18_23/INTERNAL_BIIM-2710_Change_of_Address_by_ethnicity.xlsx", range = "A17:L3640")

df$WhanganuiData$jobSupport <- readxl::read_xlsx("Whanganui_Data_Jan18_23/INTERNAL_BIIM-2710_JS_Clients.xlsx", range = "A17:L4240")
names(df$WhanganuiData$jobSupport)[names(df$WhanganuiData$jobSupport) == 'Total clients'] <- 'Total Clients'

df$WhanganuiData$foodGrants <- readxl::read_xlsx("Whanganui_Data_Jan18_23/INTERNAL_BIIM-2710_SNG_food_5_years.xlsx", range = "A19:N3441")
names(df$WhanganuiData$foodGrants)[names(df$WhanganuiData$foodGrants) == 'Total Distinct Clients'] <- 'Total Clients'

#Remove Incomplete Observations
nas <- which(df$WhanganuiData$foodGrants$`Total Clients` == ".")
df$WhanganuiData$foodGrants <- df$WhanganuiData$foodGrants[-nas,]

df$WhanganuiData$foodGrantsCost <- df$WhanganuiData$foodGrants%>%
  mutate(averageGrant = `Total Granted`/`Total Grants`)%>%
  select(-c(`Total Clients`))%>%
  rename("Total Clients" = `Total Granted`)

df$WhanganuiData$accomSup <- readxl::read_xlsx("Whanganui_Data_Jan18_23/INTERNAL_BIIM-2710_current_AS_5_years.xlsx", range = "A17:M4505")
names(df$WhanganuiData$accomSup)[names(df$WhanganuiData$accomSup) == 'Total clients'] <- 'Total Clients'

df$WhanganuiData$accomSupCost <- df$WhanganuiData$accomSup%>%
  mutate(`Monthly Accommodation Supplement` = `AS Weekly`*(52/12))%>%
  mutate(averageSup = `Monthly Accommodation Supplement`/`Total Clients`)%>%
  select(-c(`Total Clients`, `AS Weekly`))%>%
  rename("Total Clients" = `Monthly Accommodation Supplement`)


# Part 5: Add Averages and Final Clean ----------------------------------------------------

df <- rrapply(df, condition = function(x, .xname) is.data.frame(x),f = function(x) {
  x$Gender[x$Gender == "Gender Diverse"] <- "Unspecified"
  x$`Age Group`[x$Gender == "Gender Diverse"] <- "Unspecified"
  names(x)[names(x) == "Month End"] <- "Month"
  x
}, classes = "data.frame")


save(df, file = "shiny_app_data.Rmd")

