function(input, output) {
  
  #Start Server
  
  #ADDED: I also create a reactive select input
  #This changes the months options depending on the sheet you pick
  #Everything else is the same
  
  output$monthInput <- renderUI({
    
    allMonths <- unique(df[[input$folder]][[input$sheets]]$Month)
    
    selectInput(inputId = "month",
                label = "Month",
                choices = c("Overall", allMonths),
                selected = "Overall",
                selectize = F)
  })
  
  output$title <- renderUI({
    label <- switch(input$sheets,
                    "addressChange" = "Address Change",
                    "jobSupport" = "Job Support",
                    "foodGrants" = "Food Grants",
                    "accomSup" = "Accommodation Supplements"
    )
    
    title <- HTML("<div style='font-size: 18px;'> <b>Summary Tables of ", label)
    
    title
  })
  
  places <- reactive({
    
    places <- c("Total", unique(df[[input$folder]][[input$sheets]]$`Territorial Local Authority`))
    
    places
  })
  
  
  dfClean <- reactive({
    
    #Edit category varialbe to include more raw data
    #Generalise Data
    ageSum <- data.frame(Month = df[[input$folder]][[input$sheets]]$Month,
                         `Age Group` = df[[input$folder]][[input$sheets]]$`Age Group`,
                         "Territorial Local Authority" = 
                           df[[input$folder]][[input$sheets]]$`Territorial Local Authority`,
                         "Gender" = df[[input$folder]][[input$sheets]]$Gender,
                         "Maori" = df[[input$folder]][[input$sheets]]$Maori,
                         "European" = df[[input$folder]][[input$sheets]]$European,
                         "Other" = df[[input$folder]][[input$sheets]]$`Pacific Peoples` + 
                                      df[[input$folder]][[input$sheets]]$Asian + df[[input$folder]][[input$sheets]]$MELAA + 
                                      df[[input$folder]][[input$sheets]]$Other + 
                                      df[[input$folder]][[input$sheets]]$Unknown,
                         "Total.Clients" = as.numeric(df[[input$folder]][[input$sheets]]$`Total Clients`)) %>%
      mutate(`Age.Group` = replace(`Age.Group`,
                                   `Age.Group` %in% c("<16", "16", "17",
                                                      "18-19", "18", "19"),
                                   "Under 20"))%>%
      mutate(`Age.Group` = replace(`Age.Group`,
                                   `Age.Group` %in% c("20-24", "25-29"),
                                   "20-29"))%>%
      mutate(`Age.Group` = replace(`Age.Group`,
                                   `Age.Group` %in% c("30-34", "35-39"),
                                   "30-39"))%>%
      mutate(`Age.Group` = replace(`Age.Group`,
                                   `Age.Group` %in% c("40-44", "45-49"),
                                   "40-49"))%>%
      mutate(`Age.Group` = replace(`Age.Group`,
                                   `Age.Group` %in% c("50-54", "55-59", "60-64"),
                                   "50-64"))%>%
      complete(Month, Age.Group, Gender, `Territorial.Local.Authority`,
               fill = list(Maori = 0, Non.Maori = 0, 
                           European = 0, Other = 0,
                           Total.Clients = 0))
    
    ageSum
    
    
  })
  Data <- reactive({
    
    setNames(lapply(places(), function (i){
        
        ageSum <- dfClean()

        totals <- ageSum%>%
          filter(Age.Group != "Unspecified",
                 if(input$month != "Overall")Month == input$month else TRUE,
                 if(i != "Total") Territorial.Local.Authority == i else TRUE)%>%
          group_by(Age.Group)%>%
          summarise(across(c(Maori, European, Other, `Total.Clients`), sum))%>%
          mutate(Gender = "Total")
        
        ageSum <- ageSum%>%
          filter(Age.Group != "Unspecified",
                 if(input$month != "Overall")Month == input$month else TRUE,
                 if(i != "Total") Territorial.Local.Authority == i else TRUE)%>%
          group_by(`Age.Group`, `Gender`)%>%
          summarise(across(c(Maori, European, Other, `Total.Clients`), sum))%>%
          bind_rows(totals)%>%
          arrange(factor(`Gender`,
                         levels = c("Male", "Female", "Total")))%>%
          arrange(factor(`Age.Group`, levels = c("Under 20", "20-29",
                                                 "30-39", "40-49", "50-64",
                                                 "65+", "Unspecified")))%>%
          filter(Gender == input$gender)
        


      }), places())
    
  })
  

  
  output$tableTitleAC1 <- output$tableTitleJS1 <- 
    output$tableTitleFG1 <- output$tableTitleAS1 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[1]])
    })
  output$tableTitleAC2 <- output$tableTitleJS2 <- 
    output$tableTitleFG2 <- output$tableTitleAS2 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[2]])
    }) 
  output$tableTitleAC3 <- output$tableTitleJS3 <- 
    output$tableTitleFG3 <- output$tableTitleAS3 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[3]])
    }) 
  output$tableTitleAC4 <- output$tableTitleJS4 <- 
    output$tableTitleFG4 <- output$tableTitleAS4 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[4]])
    })
  output$tableTitleAC5 <- output$tableTitleJS5 <- 
    output$tableTitleFG5 <- output$tableTitleAS5 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[5]])
    })
  output$tableTitleAC6 <- output$tableTitleJS6 <- 
    output$tableTitleFG6 <- output$tableTitleAS6 <- renderUI({
      HTML("<div style='font-size: 14px;'> <b>", places()[[6]])
    })
    
    
    
    output$summaryAC1 <- output$summaryJS1 <- 
    output$summaryFG1 <- output$summaryAS1 <- renderDataTable({
      
      datatable(Data()[[places()[1]]],
                options = list(scrollX = TRUE,
                               pageLength = 25,
                               dom = 't'))
    })
  
  output$summaryAC2 <- output$summaryJS2 <- 
    output$summaryFG2 <- output$summaryAS2 <- renderDataTable({
      
      datatable(Data()[[places()[2]]],
                options = list(scrollX = TRUE,
                               pageLength = 25,
                               dom = 't'))
    })
  
  output$summaryAC3 <- output$summaryJS3 <- 
    output$summaryFG3 <- output$summaryAS3 <- renderDataTable({
      
      datatable(Data()[[places()[3]]],
                options = list(scrollX = TRUE,
                               pageLength = 25,
                               dom = 't'))
    })
  
  output$summaryAC4 <- output$summaryJS4 <- 
    output$summaryFG4 <- output$summaryAS4 <- renderDataTable({
      
      datatable(Data()[[places()[4]]],
                options = list(scrollX = TRUE,
                               pageLength = 25,
                               dom = 't'))
    })
  
  output$summaryAC5 <- output$summaryJS5 <- 
    output$summaryFG5 <- output$summaryAS5 <- renderDataTable({
      
      datatable(Data()[[places()[5]]],
                options = list(scrollX = TRUE,
                               pageLength = 25,
                               dom = 't'))
    })
  output$summaryAC6 <- output$summaryJS6 <- 
    output$summaryFG6 <- output$summaryAS6 <- renderDataTable({
      
      datatable(Data()[[places()[6]]],
                options = list(scrollX = TRUE,
                               pageLength = 26,
                               dom = 't'))
    })
  
}
