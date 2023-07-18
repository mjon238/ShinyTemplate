#Start Page ----
dashboardPage(
  title = "",
  dashboardHeader(
    title = "Template"
  ),
  
  #Add Sidebar Inputs
  dashboardSidebar(
    #SideBar Inputs
    sidebarMenu(
      id = "folder",
      menuItem("Wairoa Data", tabName = "WairoaData"),
      menuItem("Taranaki", tabName = "Taranaki22"),
      menuItem("Select Local Territories", tabName = "SelectLocalities"),
      menuItem("Whanganui Data", tabName = "WhanganuiData")
    ),
    
    #Radio Group Buttons
    radioGroupButtons(inputId = "gender",
                      label = "Gender",
                      choices = c("Total", "Male", "Female"),
                      selected = "Total",
                      justified = TRUE),
    
    #SEE SERVER: I created added a reactive select input
    uiOutput("monthInput")
  ),
  
  #Add Body (Outputs)
  dashboardBody(
    uiOutput("title"),

  br(),
tabsetPanel(id = "sheets", selected = "addressChange",
            tabPanel(title = "Address Change", value = "addressChange", br(),
                     fluidRow(column(6, uiOutput("tableTitleAC1"),
                                          DTOutput("summaryAC1"),
                                        uiOutput("tableTitleAC2"),
                                          DTOutput("summaryAC2"),
                                     conditionalPanel(condition = "input.folder == 'WairoaData' |
                                                      input.folder == 'WhanganuiData'",
                                        uiOutput("tableTitleAC5"),
                                          DTOutput("summaryAC5"))),
                              column(6, uiOutput("tableTitleAC3"),
                                     DTOutput("summaryAC3"),
                                     uiOutput("tableTitleAC4"),
                                      DTOutput("summaryAC4"),
                                      conditionalPanel(condition = "input.folder == 'WhanganuiData'",
                                                       uiOutput("tableTitleAC6"),
                                                       DTOutput("summaryAC6"))))
                     ),
            tabPanel(title = "Job Support", value = "jobSupport", br(),
                     fluidRow(column(6, uiOutput("tableTitleJS1"),
                                     DTOutput("summaryJS1"),
                                     uiOutput("tableTitleJS2"),
                                     DTOutput("summaryJS2"),
                                     conditionalPanel(condition = "input.folder == 'WairoaData' |
                                                      input.folder == 'WhanganuiData'",
                                                      uiOutput("tableTitleJS5"),
                                                      DTOutput("summaryJS5"))),
                              column(6, uiOutput("tableTitleJS3"),
                                     DTOutput("summaryJS3"),
                                     uiOutput("tableTitleJS4"),
                                     DTOutput("summaryJS4"),
                                     conditionalPanel(condition = "input.folder == 'WhanganuiData'",
                                                      uiOutput("tableTitleJS6"),
                                                      DTOutput("summaryJS6"))))
                     ),
            tabPanel(title = "Food Grants", value = "foodGrants", br(),
                     fluidRow(column(6, uiOutput("tableTitleFG1"),
                                     DTOutput("summaryFG1"),
                                     uiOutput("tableTitleFG2"),
                                     DTOutput("summaryFG2"),
                                     conditionalPanel(condition = "input.folder == 'WairoaData' |
                                                      input.folder == 'WhanganuiData'",
                                                      uiOutput("tableTitleFG5"),
                                                      DTOutput("summaryFG5"))),
                              column(6, uiOutput("tableTitleFG3"),
                                     DTOutput("summaryFG3"),
                                     uiOutput("tableTitleFG4"),
                                     DTOutput("summaryFG4"),
                                     conditionalPanel(condition = "input.folder == 'WhanganuiData'",
                                                      uiOutput("tableTitleFG6"),
                                                      DTOutput("summaryFG6"))))
                              ),
            tabPanel(title = "Accomodation Supplements", value = "accomSup", br(),
                     fluidRow(column(6, uiOutput("tableTitleAS1"),
                                      DTOutput("summaryAS1"),
                                      uiOutput("tableTitleAS2"),
                                      DTOutput("summaryAS2"),
                                     conditionalPanel(condition = "input.folder == 'WairoaData' |
                                                      input.folder == 'WhanganuiData'",
                                                      uiOutput("tableTitleAS5"),
                                                      DTOutput("summaryAS5"))),
                               column(6, uiOutput("tableTitleAS3"),
                                      DTOutput("summaryAS3"),
                                      uiOutput("tableTitleAS4"),
                                      DTOutput("summaryAS4"),
                                      conditionalPanel(condition = "input.folder == 'WhanganuiData'",
                                                       uiOutput("tableTitleAS6"),
                                                       DTOutput("summaryAS6"))))
                            )
            )
)
)


