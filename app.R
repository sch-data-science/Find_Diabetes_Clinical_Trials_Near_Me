
library(shiny)
library(shinyWidgets)
library(DT)
library(sf)
library(leaflet)
library(stringr)
library(ggplot2)
library(dplyr)
library(bslib)
library(bsicons)
library(shinyBS)

trials <- readRDS("trials.RDS")

#DIABETES


SiteStatus <- c(sort(unique(trials$STATUS)))
Phase <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4") 
Condition <- c("Type1","Type2","Gestational","Prediabetes","Neuropathy","Hyperglycemia","Hypoglycemia","Hypertension")
names(Condition) = c("Type 1 Diabetes","Type 2 Diabetes","Gestational Diabetes","Prediabetes","Neuropathic Complications","Hyperglycemia","Hypoglycemia","Hypertension")
StudyState <- c(sort(unique(trials$STATE)))
StudyCity <- c(sort(unique(trials$CITYSTATE)))

ui <- fluidPage(
  
  tags$style(HTML(".dataTables_wrapper .dataTables_filter {
                     float: left;
                     padding-left: 50px;}
                  .dataTables_wrapper .dataTables_filter input{
                      width: 500px;}"
  )
  ),
  
  titlePanel(
    fluidRow(
      column(2, HTML('<a target="_blank" rel="noopener noreferrer" href="https://www.seattlechildrens.org/"><img src = "logo.jpg" height = 100></a>')),
      column(10,HTML("Diabetes-related Clinical Trials Near Me<br><small>Interventional studies in the United States of America with at least one site/location currently recuriting.<br>
                     Data last refreshed ",
                     format(as.Date(trials$LATEST_REFRESH[1]),format = "%m/%d/%Y"),"</small>"))),windowTitle="Diabetes-related Clinical Trials Near Me"),
  
  # Create a new Row in the UI for selectInputs
  tabsetPanel(type="tabs",
              tabPanel("Main",
                       fluidRow(
                         column(2,
                                bsTooltip("Age_input_a",
                                          "Age in years",
                                          placement = "right", trigger = "hover"),
                                
                                numericInput(
                                  "Age_input",
                                  label=tagList(
                                    "Enter Participant Age",
                                    tags$span(
                                      icon("question-circle"),
                                      id = "Age_input_a",
                                      style = "margin-left: 5px; cursor: pointer; color: #1c9ed8;"
                                    )
                                  ),
                                  value=18
                                ),
                                
                                bsTooltip("SiteStatus_input_a",
                                          "Not all sites for a Clinical Trial are actively recruiting",
                                          placement = "right", trigger = "hover"),
                                
                                pickerInput("SiteStatus_input", 
                                            label=tagList(
                                              "Site Status",
                                              tags$span(
                                                icon("question-circle"),
                                                id = "SiteStatus_input_a",
                                                style = "margin-left: 5px; cursor: pointer; color: #1c9ed8;"
                                              )
                                            ),
                                            SiteStatus, selected="RECRUITING",
                                            multiple=TRUE,
                                            options = list(
                                              "title" = 'Click to see options',
                                              'actions-box'= TRUE
                                            )
                                ),
                                
                                bsTooltip("Phase_input_a",
                                          title=HTML("<ul><li>Phase 1 trials focus on safety and dosage.</li><li>Phase 2 trials focus on effectiveness, side effects, and dosage.</li><li>Phase 3 trials compare effectiveness to standard treatments.</li><li>Phase 4 trials examine long-term benefits and side effects.</li></ul>"),
                                          placement = "right", trigger = "hover"),
                                
                                pickerInput("Phase_input", 
                                            label=tagList(
                                              "Phase",
                                              tags$span(
                                                icon("question-circle"),
                                                id = "Phase_input_a",
                                                style = "margin-left: 5px; cursor: pointer; color: #1c9ed8;"
                                              )
                                            ),
                                            Phase, selected=Phase,
                                            multiple=TRUE,
                                            options = list(
                                              "title" = 'Click to see options',
                                              'actions-box'= TRUE
                                            )
                                ),
                                HTML("<br>"),
                                textOutput("NLocations"),
                                textOutput("NStudies")
                                
                         ),
                         column(3,
                                
                                bsTooltip("Condition_input_a",
                                          "See Read Me tab for more information about the drop-down options",
                                          placement = "right", trigger = "hover"),
                                
                                
                                pickerInput("Condition_input", 
                                            label=tagList(
                                              "Condition Selector",
                                              tags$span(
                                                icon("question-circle"),
                                                id = "Condition_input_a",
                                                style = "margin-left: 5px; cursor: pointer; color: #1c9ed8;"
                                              )
                                            ),
                                            Condition, selected=Condition,
                                            multiple=TRUE,
                                            options = list(
                                              "title" = 'Click to see options',
                                              'actions-box'= TRUE
                                            )
                                ),
                                bsTooltip("Condition_search_a",
                                          "Select the Clinical Trials whose Brief Title, Brief Summary, Keyword, and Conditions information, when combined, contain ALL the words entered below",
                                          placement = "right", trigger = "hover"),
                                
                                textInput("Condition_search",
                                          
                                          label=tagList(
                                            "Search Specific Words",
                                            tags$span(
                                              icon("question-circle"),
                                              id = "Condition_search_a",
                                              style = "margin-left: 5px; cursor: pointer; color: #1c9ed8;"
                                            )
                                          ))
 
                         ),
                         column(2,
                                pickerInput("State_input", "State Select",
                                            StudyState, selected=StudyState,
                                            multiple=TRUE,
                                            options = list(
                                              "title" = 'Click to see options',
                                              'actions-box'= TRUE
                                            )
                                ),
                                pickerInput("City_input", "City Select",
                                            StudyCity, selected="All",
                                            multiple=TRUE,
                                            options = list(
                                              "title" = 'Click to see options',
                                              'actions-box'= TRUE
                                            )
                                ),
                                
                         ),
                         column(5,HTML("<b>Drag and Zoom to further refine your search</b>")),
                         column(5,leafletOutput('map'))
                       ),
                       
                       
                       # Create a new row for the table.
                       HTML("Please click the 'Study URL' link below for more details and contact information for that particular study"),
                       HTML("<br>"),
                       div(DT::DTOutput("table"), style = "font-size:80%")
              ),
              tabPanel("Read Me",
                       HTML("<br>"),
                       HTML("This app is based on the work of Andrew Cooper, Gayle Garson, Arpit Jain, Tyler Ketterl, Amy Wilcox at <a target='_blank' rel='noopener noreferrer' href='https://www.seattlechildrens.org/'>Seattle Children's Hospital</a>."),
                       HTML("<br>"),
                       HTML("<br>"),
                       HTML("The information provided on this dashboard is intended for general informational purposes only. It is designed to give an overview of clinical trial opportunities available within the US. While we strive to ensure the accuracy and timeliness of the information, it is important to discuss any clinical trial options with your healthcare provider before making any decisions. Participation in clinical trials is voluntary, and eligibility may vary based on individual health conditions and medical history. Please consult with your medical team for personalized advice regarding clinical trial opportunities. Except for minor geo-coding edits, data presented here is as-is from <a target='_blank' rel='noopener noreferrer' href='http://clinicaltrials.gov'>Clinicaltrials.gov</a>"),
                       HTML("<br>"),
                       HTML("<br>"),
                       HTML("Data source: <a target='_blank' rel='noopener noreferrer' href='http://clinicaltrials.gov'>Clinicaltrials.gov</a> API.<br>"),
                       HTML("<br>"),
                       HTML("Map provided by <a target='_blank' rel='noopener noreferrer' href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> under their  Creative Commons Attribution-ShareAlike 2.0 license (CC BY-SA 2.0).<br>
                            Geocoding provided by <a target='_blank' rel='noopener noreferrer' href='https://docs.mapbox.com/api/search/geocoding/'>Mapbox Geocoding API</a>. <br>To contact Mapbox and OpenStreetMap to suggest improvements to the map, itself, <strong><a href='https://apps.mapbox.com/feedback/' target='_blank'>Please click here</a></strong>"),
                       HTML("<br>"),
                       HTML("<br>"),
                       HTML("The customized Condition Selector searches Brief Summary, Keyword, and Conditions fields in the data provided by ClinicalTrials.gov for the following combinations.  When more than one option is checked, Clinical Trials are filtered to include ANY trials that contain the options.
                       Please see the <a href = 'https://github.com/sch-data-science/Find_Diabetes_Clinical_Trials_Near_Me/blob/main/getdata.R'>code repository</a> for the actual coding statements. <br>
                       
                       <ul>
                            <li> 'Type 1 Diabetes' looks for various permuations of 'type' and '1' or 'I', with or without spaces or hyphens, along with 't1d' or 't1dm', and with various capitalizaitons.</li>
                            <li> 'Type 2 Diabetes' looks for various permuations of 'type' and '2' or 'II', with or without spaces or hyphens, along with 't2d' or 't2dm', and with various capitalizaitons.</li>
                            <li> 'Gestational Diabetes' looks for the words 'gestational' or 'gdn', with various capitalizations. </li>
                            <li> 'Prediabetes' looks for the pattern 'prediabet' or the combination of 'pre' and 'diabet' with spaces or hyphens between them, with various capitalizations.  This allows us to detect permutations of both prediabetes and prediabetic.</li>
                            <li> 'Neuropathic Complications' looks for the pattern 'neuropath' to capture both 'neuropathic' and neuropathy, along with the words 'foot', 'ulcer', 'weakness', or 'amputation', all with various capitalizations. </li>
                            <li> 'Hyperglycemia' looks for the pattern 'hyperglycem' to capture both 'hyperglycemia' and 'hyperglycemic', with various capitalizations.</li>
                            <li> 'Hypoglycemia' looks for the pattern 'hypoglycem' to capture both 'hypoglycemia' and 'hypoglycemic', with various capitalizations.</li>
                            <li> 'Hypertension' looks for the word 'hypertension' with various capitalizations. </li>
                          </ul>"),
                       HTML("<br>"),
                       HTML("For technical questions about this page, please contact:  Andrew.Cooper@SeattleChildrens.Org"),
                       HTML("<br>"),
                       HTML("For information regarding Seattle Children's Hospital, please visit <a target='_blank' rel='noopener noreferrer' href='https://www.seattlechildrens.org/'>our web page</a>."),
                       HTML("<br>"),
                       HTML("<br>"),
                       HTML("Code for this app can be downloaded or forked from the <a target='_blank' rel='noopener noreferrer' href='https://github.com/sch-data-science/'>Seattle Children's Hospital Data Science GitHub page</a>.")
              )))

server <- function(input, output,session) {
  
  #session <- sessionInfo()
  #version <- paste0(session$R.version$major,".",session$R.version$minor)
  
  observeEvent(input$State_input,{
    xx <- trials %>% filter(STATE %in% input$State_input) %>% dplyr::select(CITYSTATE) %>% unique()  %>% data.frame()
    
    # Can also set the label and select items
    updatePickerInput(session=session, inputId = "City_input",
                      choices = c(sort(xx$CITYSTATE)),
                      selected = xx$CITYSTATE
    )
  }) 
  
  MyData <- reactive({
    data <- trials
    
    data <- data[data$STATUS %in% input$SiteStatus_input & 
                   
                   str_detect(data$PHASE,paste(input$Phase_input, collapse = "|")) == TRUE &
                   data$STATE %in% input$State_input &
                   data$CITYSTATE %in% input$City_input
                 ,]
    
    data$NConditions = rowSums(data[,c("DUMMY","DUMMY",toupper(input$Condition_input))]/1)
    data <- data %>% filter(NConditions>0 & input$Age_input >= MINAGE & input$Age_input<=MAXAGE)
    
    
    if(!(input$Condition_search %in% c(NULL, NA, " ",""))) {
      data <- data %>% filter(rowSums(sapply(
        unlist(str_split(trimws(input$Condition_search,which="right"),pattern=" ")),
        str_detect,
        string = tolower(paste(CONDITION,KEYWORD,BRIEFSUMMARY,BRIEFTITLE)))) == 
          length(unlist(str_split(trimws(input$Condition_search,which="right"),pattern=" "))))
    }
    
    
    
    data <- data %>% mutate(AGE_RANGE = paste0(MINAGE,"-",MAXAGE),
                            STUDYURL = paste0("<a target='_blank' rel='noopener noreferrer' href = '",STUDYURL,"'>",STUDYURL,"</a>"))
    
    data
  })
  
  
  MapData = reactive({
    
    MyData() %>% group_by(CITY,GEOPOINT.LAT,GEOPOINT.LON) %>% summarize(ntrials = n()) 
    
    
  })
  
  output$map = renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircleMarkers(MapData()$GEOPOINT.LON,MapData()$GEOPOINT.LAT,radius=3,
                       popup=paste0(MapData()$CITY,", ",MapData()$ntrials," trials")) 
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(MyData(),
           GEOPOINT.LAT >= latRng[1] & GEOPOINT.LAT <= latRng[2] &
             GEOPOINT.LON >= lngRng[1] & GEOPOINT.LON <= lngRng[2])
  })
  
  
  output$NLocations <- renderText({
    paste0("Sites Selected: ",length(unique(dataInBounds()$FACILITY)))})
  
  output$NStudies <- renderText({
    paste0("Studies Selected: ",length(unique(dataInBounds()$ORG_STUDY_ID)))})
  
  output$table <- DT::renderDataTable(DT::datatable({
    temp <- dataInBounds() 
    temp <- temp %>% dplyr::select(ORG_STUDY_ID,BRIEFTITLE,FACILITYLOC,STATUS,PHASE,STUDYURL,CONTACT,
                                   #PRINCIPAL_INVESTIGATOR,
                                   AGE_RANGE,CONDITION)
    names(temp) <- c("Study ID", "Study Title","Study Site","Site Status","Phase","Study URL",
                     "Contact Name | Email | Phone",
                     #"Principal Investigator", 
                     "Age Range","Condition(s)")
    temp
  }, escape = FALSE ,options = list(dom = 'ltp')
  
  ))
}


shinyApp(ui = ui, server = server)