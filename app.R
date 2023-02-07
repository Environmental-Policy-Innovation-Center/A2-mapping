# setwd("~/Documents/GitHub/A2-mapping")
load("working_progress.RData")
library(shiny)
library(leaflet)
library(dplyr)
library(googlesheets4)
#library(leaflet.extras)

# gs4_deauth()
# URL <- "https://docs.google.com/spreadsheets/d/1rr6ZKZ76nstJXZHqo1QhtZlttXp2kFb7pJrJeiGdt-g/edit#gid=0"
# location = read_sheet(URL)
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      style = "position: fixed; height: 100%;width: 350px; overflow-y: auto; margin-left: +95px;", 
      div(style = "display:inline-block; float:left; margin-bottom: 20px"),
      uiOutput("SidebarTitle"), 
      selectInput(inputId = "Issue",
                  label = "Select a climate/environmental issue:",
                  choices = c("All","Flooding","Air Pollution","Water Contamination", "Drought","Heat")),
      selectInput(inputId = "Strategy",
                  label = "Select a strategy:",
                  choices = c("Affordable Housing",
                              "Art Activism",
                              "Community Farm/Gardens",
                              "Community Land Trusts/Land Conservation",
                              "Community Science",
                              "Direct Relief and Aid",
                              "Elevation or Relocation of Homes",
                              "Fighting Industrial Contamination",
                              "Green Infrastructure",
                              "Halting Bad Development",
                              "Nature-Based Solutions",
                              "Policy Reform",
                              "Renewable Energy",
                              "Rights of Nature")),
      
      uiOutput("SidebarSelect"), 
      
      uiOutput("SidebarDescContent", style = "max-height: 350px; overflow-y: scroll; margin-right: -10px;  margin-bottom: 10px; margin-top:10px;"),
      uiOutput("video"),
      uiOutput("SidebarHeaderContent"), 
      uiOutput("logo"),
      # output for different service area
      
      # uiOutput("ComponentSelect"),  
      # uiOutput("SidebarTableContent"),
      # actionButton("showInfo", " Info", style = "display:inline-block; float:left; margin-top: 30px"),
      # img(src = 'epic-logo-transparent.png', height = '50px', width = '200px', style = "display:inline-block; float:center; margin-top: 20px; margin-left: 10px"),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("Map", height = "100vh"),
      style = "margin-left: -30px;",
      width = 9),
    
    
    position = c("left"), fluid = FALSE)
)


server <- function(input, output) {
    map_data <- reactive({
      ifelse(input$Issue == "All", location,
      location %>%
       filter(grepl(input$Issue,`Which of the following climate or environmental impacts affects your community?`) )
      )
      # sf_states_lite %>%
      #   # mutate(net_funding = total_funding * (100 - input$pct_survey)/100) %>%
      #   # mutate(net_funding = total_funding ) %>% # net funding no longer includes sutraction of survey funding
      #   mutate(number_of_pipes_replaced = total_funding / input$Pb_cost) %>%
      #   mutate(pct_pipe_replaced = number_of_pipes_replaced/pb_pipes*100) %>%
      #   mutate(pct_pipe_replaced = ifelse(pct_pipe_replaced>100, 100, pct_pipe_replaced) )


    })
  ### Main Map Body ###
  output$Map <- renderLeaflet({
    
    # LayerList$df <- AllDataReactive$df %>% pull(NAME) %>% unique()
    # geo_data <- map_data()
    leaflet() %>%
      addProviderTiles(providers$CartoDB.VoyagerNoLabels, group = "Voyager") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addLayersControl(baseGroups = c("Voyager","Topo","Toner Lite", "World Imagery")) %>%
      addMarkers( data = location,
                 label = ~`Name of group`,
                 clusterOptions = markerClusterOptions(),
                 popup = ~paste("<b>Name of the member:</b>",`Name of group`,
                                "<br>",
                                "<b>Point of contact:</b>",`Leaders name`,
                                "<br>",
                                "<b>Issues:</b>",`Which of the following climate or environmental impacts affects your community?`,
                                "<br>",
                                "<b>Communities served:</b>",`Town`,
                                "<br>",
                                "<b>State:</b>",`State`,
                                "<br>"
                                )
      ) %>% 
      setView(lat = 41.62531, lng = -97.71755, zoom = 5)
    
    # leaflet("Map")%>%
    #   addProviderTiles(providers$CartoDB.VoyagerNoLabels)%>%
    #   addMapPane("Cities and Counties", zIndex = 500)%>%
    #   addMapPane("Federal", zIndex = 200) %>%
    #   addMapPane("States", zIndex = 450) %>%
    #   addMapPane("Labels", zIndex = 500) %>%
    #   
    #   addPolygons(data = CitiesCounties, group = "Cities and Counties", layerId = ~NAME,  options = leafletOptions(pane = "Cities and Counties"), fillColor = "#DDCC77", color = "#000000", weight =  1, fillOpacity = .85, label = ~NAME,
    #               highlight = highlightOptions(weight = 2, color = "Black", bringToFront = TRUE),  popup = ~Popup)%>%
    #   
    #   addPolygons(data = States, group = "States", layerId = ~NAME, options = leafletOptions(pane = "States"), fillColor = "#117733", color = "#000000", weight =  .5, fillOpacity = .65, label = ~NAME,
    #               highlight = highlightOptions(weight = 2, color = "Black", bringToFront = TRUE), popup = ~Popup)%>%
    #   
    #   addPolygons(data = Federal, group = "Federal", layerId = ~NAME, options = leafletOptions(pane = "Federal"), fillColor = "#b3b3b3", color = "#000000", weight =  .25, opacity = .5, label = ~NAME,
    #               highlight = highlightOptions(weight = 1, color = "Black", bringToFront = TRUE),  popup = ~Popup)%>%
    #   
    #   addLayersControl(overlayGroups = c("Cities and Counties", "Federal", "States"), options = layersControlOptions(collapsed = FALSE))%>%
    #   addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, options = leafletOptions(pane = "Labels"))%>%
    #   setView(-98.5795, 39.8283, zoom = 5)
  })
  
  
  
  ## Sidebar title 
  output$SidebarTitle <- renderUI({
    h1("A2 membership map")
  })
  
  # ## Sidebar Tabset Panel 
  # output$SidebarSelect <- renderUI({
  #   selectInput("ToolName", 
  #               "Select an EJ Tool:", 
  #               unique(SelectedDataReactive$df$Name))
  # })
  
  ## Sidebar Tabset Panel
  
  output$SidebarHeaderContent <- renderUI({
    
    Link <- paste("<b>", "<a href='https://anthropocenealliance.org/our-communities/'>See profile page for more information</a>", "</b>","<br>")
    # LastUpdated <- paste("<b>", "Last Updated:","</b>", ifelse(!is.na(SelectedTool$Updated_Date), SelectedTool$Updated_Date,SelectedTool$Published), "<br>")
    HTML(Link)
  })
  
  
  output$SidebarDescContent <- renderUI({
    
    
    Description <- paste("<b>", 
                         "[First sentence describes Community X is from X city, Y county, Z states.] 
                         [Second sentence describes who funded the community at Year because of cause] 
                         [The community is helping ____ issue] [The community achievement, funding received] 
                         [History/projects with A2] [Collaborations through A2] [Primary contact]",
                         "</b>", "<br>" , "<br>","<br>")
   
    # EJDef <- paste("<b>", "Environmental Justice Definition:","</b>", "<br>", ifelse(is.na(SelectedTool$EJ_Def_Det), "No Definition", SelectedTool$EJ_Def_Det), "<br>","<br>")
    HTML(Description)
  })
  output$video <- renderUI({
  Videolink <- paste("<b>Video:</b>",
                     "<br>",
                     '<iframe width="300" height="169" src="https://www.youtube.com/embed/YftutAJeXAU?showinfo=0" frameborder="0" allowfullscreen></iframe>',
                     "<br>", 
                     "<br>" )
  HTML(Videolink)
  })
  output$logo <- renderUI({
    img  = c("<img src='https://thrivingearthexchange.org/wp-content/uploads/2021/08/Anthropocene-Alliance-Logo.jpg' width = 150 />")
    HTML(img)
    })
  ## Sidebar Content
  # output$SidebarTableContent <- renderUI({
  #   req(input$ToolName)
  #   req(SelectedDataReactive$df)
  #   
  #   No <- "<font color=\"#c75300\">"
  #   Yes <-  "<font color=\"#1d6e9f\">"
  #   
  #   SelectedTool <- SelectedDataReactive$df %>%
  #     filter(Name == input$ToolName)
  #   
  #   StateData  <- paste("<b>", "State Data Sources:", ifelse(SelectedTool$State_Data_Sources == "Yes", Yes, No), SelectedTool$State_Data_Sources,"</b></font>", "<br>")
  #   LocalData  <- paste("<b>", "Local Data Sources:",ifelse(SelectedTool$Local_Data_Sources == "Yes", Yes, No), SelectedTool$Local_Data_Sources, "</b></font>", "<br>")
  #   ThirdPartyData <- paste("<b>", "Third Party Data Sources:", ifelse(SelectedTool$Third_Party_Data_Sources == "Yes", Yes, No), SelectedTool$Third_Party_Data_Sources, "</b></font>", "<br>")
  #   RaceEthn <- paste("<b>", "Race and Ethnicity Data:", ifelse(SelectedTool$Race_Ethnicity == "Yes", Yes, No), SelectedTool$Race_Ethnicity, "</b></font>", "<br>")
  #   RaceEthnDet <- paste("<b>", "Detailed Race and Ethnicity:",ifelse(SelectedTool$Race_Ethnicity_Detailed == "Yes", Yes, No), SelectedTool$Race_Ethnicity_Detailed, "</b></font>", "<br>")
  #   Air <- paste("<b>", "Air Quality Data:", ifelse(SelectedTool$Air_Quality == "Yes", Yes, No), SelectedTool$Air_Quality, "</b></font>", "<br>")
  #   Water <- paste("<b>", "Water Quality Data:",ifelse(SelectedTool$Water_Quality == "Yes", Yes, No), SelectedTool$Water_Quality, "</b></font>", "<br>")
  #   Climate <- paste("<b>", "Climate and Natural Hazards Data:", ifelse(SelectedTool$Climate_NatHaz == "Yes", Yes, No), SelectedTool$Climate_NatHaz, "</b></font>", "<br>")
  #   Pollutants <- paste("<b>", "Other Pollutant Data:",ifelse(SelectedTool$Other_Pollutants == "Yes", Yes, No), SelectedTool$Other_Pollutants, "</b></font>", "<br>")
  #   Health <- paste("<b>", "Health Data:",ifelse(SelectedTool$Physical_Health == "Yes", Yes, No), SelectedTool$Physical_Health, "</b></font>", "<br>")
  #   
  #   tagList(
  #     ## State Data
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Utilizing state data sources improves a tool accuracy and relevany to local EJ issues."),
  #     HTML(StateData),
  #     HTML("<br>"),
  #     ## Local Data
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Just like state data sources, local data improves an EJ tool\\'s accuracy and relevancy."),
  #     HTML(LocalData),
  #     HTML("<br>"),
  #     ## 3rd Party Data Sources 
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Examples include local institutions or private data providers."),
  #     HTML(ThirdPartyData),
  #     HTML("<br>"),
  #     ## RaceEthn
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Including race and ethnicity data is an important component of EJ tools as it it often correlates with a community\\'s susceptibility and resiliency to pollution."),
  #     HTML(RaceEthn),
  #     HTML("<br>"),
  #     ## Race Ethn Det
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title =  "Detailed race and ethnicity includes demographic breakdowns beyond minority and non-minority."),
  #     HTML(RaceEthnDet),
  #     HTML("<br>"),
  #     ## Air
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Including data on air quality data like PM2.5 and ozone pollution improves a tools accuracy, particularly for urban communities."),
  #     HTML(Air),
  #     HTML("<br>"),
  #     ## Water 
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Water quality data like proximity to wastewater discharging facilities or groundwater contamination is an important component."),
  #     HTML(Water),
  #     HTML("<br>"),
  #     ## Climate
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Climate change and natural hazards like hurricanes and wildfires are increasing in frequency and severity."),
  #     HTML(Climate),
  #     HTML("<br>"),
  #     ## Pollutants
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "Proximity to Superfund sites and RMP (facilities storing or transporting hazardous chemicals) can pose significant risks to communities."),
  #     HTML(Pollutants),
  #     HTML("<br>"),
  #     ## Health 
  #     tipify(el = icon(name = "info-circle", lib = "font-awesome"), placement = "right", 
  #            title = "A communities  health can indicate its resiliency to pollutants or indicate impacts from pollution."),
  #     HTML(Health)
  #   )
  # })
  # 
  # 
  # ### Help info Dialog ### 
  # InfoModal <- modalDialog(
  #   title = HTML("<b> Environmental Policy Innovation Center's BETA EJ Tools Map </b>"),
  #   HTML("<b> About: </b>"),
  #   HTML("<br>"),
  #   HTML("This map features environmental justice tools or 'EJ Tools' at the federal, state, and local level. EJ Tools are used to help identify and support disadvantaged communities.
  #      However, not all EJ Tools are created equal, some include local and state data sources, disaggregated race data, and incorprate significant community input in their development. Others lack local knowledge
  #      data layers necessary for supporting disadvantaged communities. Use this map to explore the different EJ Tools and their components. This tool is intended to help those developing new tools and inform the general public."),
  #   HTML("<br>"),
  #   HTML("<br>"),
  #   HTML("<b> How to use the map: </b>"),
  #   HTML("<br>"),
  #   HTML("Click on a region to see the available tools and learn more. Federal tools cover the entire United States. You can also filter by region and tool in the sidebar. Scroll through their details in the sidebar and check out the full tool via the link"),
  #   HTML("<br>"),
  #   HTML("<br>"),
  #   HTML("<b> Credits: </b>"),
  #   HTML("<br>"),
  #   HTML("Built by "),
  #   tags$a(href="https://www.policyinnovation.org/", "Environmental Policy Innovation Center (EPIC).",  target="_blank"),
  #   HTML("The tool utilizes data collected by"),
  #   tags$a(href="Urban.org", "Urban Institute.",  target="_blank"),
  #   HTML("To learn more about EJ Tools,"),
  #   tags$a(href="https://www.urban.org/research/publication/screening-environmental-justice-framework-comparing-national-state-and-local", "see here.",  target="_blank"),
  #   HTML("To leave feedback on this beta map, please leave a comment"),
  #   tags$a(href="https://docs.google.com/document/d/1hrINNpAO9YzM8ZRNBGRU6fILVW8_OfwFUbZrkIlxIw4/edit", "in this Google Document.",  target="_blank"),
  #   easyClose = FALSE,
  #   footer = modalButton("Close"),
  # )
  # 
  # observeEvent(input$showInfo, ignoreNULL = FALSE,
  #              {
  #                showModal(InfoModal)
  #              })
  # 
  # 
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
