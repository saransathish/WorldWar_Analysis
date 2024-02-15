# Load R packages
#install.packages('shinydashboard')
#install.packages('tidyr')
#install.packages('dplyr')
#install.packages('highcharter')
#install.packages('quantmod')
#install.packages('shinyWidgets')
#install.packages("highcharter")
library(dplyr)
#library(tidyr)
#library(shiny)
#install.packages("stringi")
library(shinydashboard)
#library(highcharter)
library(ggplot2)
library(shinythemes)
library(shiny)
library(plotly)
library(shinyWidgets)
library(highcharter)
library(quantmod)
library(shinycssloaders)
library(leaflet)
library(maps) 
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating
library(leaflet.extras)
library(rvest)

time1=read.csv("data/war1.csv")
time2=read.csv("data/war2.csv")
map1=read.csv("data/military.csv")
war1map=read.csv("data/map1war1.csv")
war2map=read.csv("data/map2war2.csv")
event1=read.csv("data/Events1.csv")
event2=read.csv("data/Events2.csv")
death1=read.csv("data/death1.csv")
death2=read.csv("data/death2.csv")
time3=read.csv("data/us death2.csv")
map1=map1%>%select("latitude","longitude","country","area","growthRate","density","densityMi","Rank","activeDuty","reserves","paramilitary","total","rank")
time3=time3%>%select("Nationality","StartDate","EndDate","MinDeaths","MaxDeaths","DeathsFinal","AirForce")
time4=time3%>%select("Nationality","StartDate","EndDate","DeathsFinal")
c2 = map1 %>% 
  select("densityMi", "activeDuty","reserves","paramilitary","total") %>% 
  names()
c1=death1%>% select("Deaths","death_per_popu")%>% names()
c3=death2%>% select("Deaths")%>% names()
ui<-dashboardPage(
  #defines header
  skin = "green",
  dashboardHeader(
    title="World Wars Analysis" ,
    dropdownMenu()
  ),
  
  
  #defines sidebar
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("About", tabName = "about", icon = icon("globe")),
      menuItem("First World War Analysis",tabName="war1",icon=icon("gun")),
      menuItem("Second World War Analysis",tabName="war2",icon=icon("jet-fighter-up")),
      menuItem("Country military strengths", tabName = "map", icon = icon("person-rifle")),
      menuItem("Datasets", tabName = "data", icon = icon("th"))
      
      
    )
  ),
 
  
  #defines bodys
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
     tabItem(tabName = "data",
             tabBox(id="t1",width=100,
                    tabPanel("Table",h2("Year wise death of all country during world war1",align="center"),dataTableOutput("death1tab"),
                             h2("Year wise death of all country during world war2",align="center"),dataTableOutput("death2tab")
                             ),tabPanel("Struture of data",verbatimTextOutput("structure"),verbatimTextOutput("structure1")),
                    tabPanel("summary of data",verbatimTextOutput("summary"),verbatimTextOutput("summary1"))))
               ,
      
              
             
      
      
      #second tab menu- ABOUT
      tabItem(tabName="about",
              h1("World Wars",style="text-align:center"),
              br(),
              br(),
              p(style="font-size:20px","    The World Wars were two major global conflicts that took place in the 20th century." ),
              br(),
              p(style="font-size:20px","    A ",strong("World War")," is an international conflict which involves all or most of the world's major powers. Conventionally, the term is reserved for two major international conflicts that occurred during the first half of the 20th century, World War I (1914–1918) and World War II (1939–1945), although historians have also described other global conflicts as world wars, such as the Nine Years' War, the War of the Spanish Succession, the Seven Years' War, the French Revolutionary and Napoleonic Wars, the Cold War, and the War on Terror."),
              p(style="font-size:20px","For a quick walkthrough have a look a the video below"),
              
              includeMarkdown("www/abouthome.md")
              
              ,
              h2("Etymology",style="text-align:left"),  
              p(style="font-size:20px",'    The term "first world war" was first used in September 1914 by German biologist and philosopher Ernst Haeckel, who claimed that "there is no doubt that the course and character of the feared European War ... will become the first world war in the full sense of the word", citing a wire service report in The Indianapolis Star on 20 September 1914. In English, the term "First World War" had been used by Lt-Col. Charles à Court Repington, as a title for his memoirs (published in 1920); he had noted his discussion on the matter with a Major Johnstone of Harvard University in his diary entry of September 10, 1918.
              '),
              br(),
              p(style="font-size:20px",'    The term "World War I" was coined by Time magazine on page 28b of its June 12, 1939 issue. In the same article, on page 32, the term "World War II" was first used speculatively to describe the upcoming war. The first use for the actual war came in its issue of September 11, 1939.[7] One week earlier, on September 4, the day after France and the United Kingdom declared war on Germany, the Danish newspaper Kristeligt Dagblad used the term on its front page, saying "The Second World War broke out yesterday at 11 a.m."') ,
              br(),
              
              h2("First World War",style="text-align:left"),
              p(style="font-size:20px",'    World War I, also known as the Great War, was fought from 1914 to 1918, primarily in Europe. It was triggered by the assassination of Archduke Franz Ferdinand of Austria-Hungary, and involved the Central Powers (led by Germany, Austria-Hungary, and the Ottoman Empire) against the Allied Powers (led by the United Kingdom, France, and Russia). The war resulted in the deaths of millions of people, both military and civilian, and ultimately led to the collapse of several empires and the redrawing of the map of Europe.'),
              p(style="font-size:20px",'    The war lasted from July 28, 1914, to November 11, 1918, and involved over 30 countries.
             
              The war was characterized by trench warfare, in which soldiers dug elaborate systems of trenches and fortifications to protect themselves from enemy fire.
             
              Chemical weapons, such as mustard gas and chlorine gas, were used for the first time during this war.
             
              The war ended with the signing of the Treaty of Versailles, which imposed heavy reparations on Germany and Austria-Hungary and led to the dissolution of the Ottoman Empire and the Austro-Hungarian Empire.
             
              The war also contributed to the Russian Revolution, which led to the establishment of the Soviet Union.'),
              br(),
              includeMarkdown("www/aboutwar1.md")
              ,
              
              h2("Second World War",style="text-align:left"),
              p(style="font-size:20px",'    World War II, fought from 1939 to 1945, was even more devastating, with an estimated 70-85 million fatalities. It was triggered by the invasion of Poland by Nazi Germany and eventually involved most of the worlds nations, divided into two opposing military alliances: the Allies (led by the United States, the United Kingdom, and the Soviet Union) and the Axis (led by Germany, Japan, and Italy). The war saw unprecedented destruction and brutality, including the Holocaust, in which millions of Jews and other minorities were systematically murdered by the Nazis.'),
              br(),
              p(style="font-size:20px","    The war lasted from September 1, 1939, to September 2, 1945, and involved nearly every country in the world.

                  The war saw the development and use of more advanced military technology, including tanks, planes, and submarines.

                  The Holocaust, in which six million Jews and millions of other minority groups were systematically murdered by the Nazis, is widely considered one of the greatest atrocities in human history.

                  The war ended with the unconditional surrender of Germany and Japan, and the establishment of the United Nations.

                  The war also resulted in the Cold War, a period of political tension and military rivalry between the United States and the Soviet Union that lasted until the early 1990s."),
              br(),includeMarkdown("www/aboutwar2.md"),
              
              
              h2("Potential Third World War",style="text-align:left"),
              p(style="font-size:20px",'    Since the atomic bombings of Hiroshima and Nagasaki during the Second World War, there has been a widespread and prolonged fear of a potential third World War between nuclear-armed powers.It is often suggested that it would become a nuclear war, and be more devastating and violent than both the First and Second World Wars. Albert Einstein is often quoted as having said in 1947 that "I know not with what weapons World War III will be fought, but World War IV will be fought with sticks and stones." It has been anticipated and planned for by military and civil authorities, and it has also been explored in fiction in many countries. Scenarios have ranged from conventional warfare to limited or total nuclear warfare.[citation needed]

                     Various former government officials, politicians, authors, and military leaders (including James Woolsey, Alexandre de Marenches, Eliot Cohen, and Subcomandante Marcos) have attempted to apply the labels of the "Third World War" and the "Fourth World War" to various past and present global wars since the end of the Second World War, such as the Cold War and the War on Terror respectively. However, none of the wars have commonly been deemed world wars.'),
              br(),includeMarkdown("www/aboutwar3.md"),
      ),
      
      tabItem(tabName = "war1",
              tabBox(
                id="t1",width=100,
                tabPanel("World Map",h2('All 30 Country participatd in ',strong("World War 1"),align="center"),
                         leafletOutput("war1Map") %>% withSpinner(color = "green"),),
                
               
                  tabPanel("Events of War",
              h2("First World War top 30 significant events",align="center") ,
              dataTableOutput("event1")),
              tabPanel("Casualties",h2("Casualties of World War1",align="center"),
              fluidRow(tags$div(align="center", box(tableOutput("top10"), title = textOutput("head3") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                      tags$div(align="center", box(tableOutput("low10"), title = textOutput("head4") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                
              ),selectInput(inputId = "var1" , label ="Select the type" , choices = c1),withSpinner(plotlyOutput("bar1")),
              
              
              )  
              )
            
              
              
              # end row
      ),
      tabItem(tabName = "war2",
              tabBox(
                id="t1",width=100,
                tabPanel("World Map",h2('All  Countries participatd in ',strong("World War 2"),align="center"),
                         leafletOutput("war2Map") %>% withSpinner(color = "green"),),
                tabPanel("Events of War",
              
              h2("second World War top 40 significant events",align="center") ,
              dataTableOutput("event2")),
              tabPanel("Casualties", h2("Casualties of World War2",align="center") ,
              fluidRow(tags$div(align="center", box(tableOutput("top1"), title = textOutput("head5") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
              tags$div(align="center", box(tableOutput("low1"), title = textOutput("head6") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                ),selectInput(inputId = "var3" , label ="Select the type" , choices = c3),withSpinner(plotlyOutput("bar2")),
              fluidPage
              (
                column(12,box(highchartOutput("timeline2")))  ,
                
              )),tabPanel("Data",dataTableOutput("usdeath2"))))
              
              
      ,
      tabItem(
        tabName = "map",
        tabBox( id="t2",width=100,    
          tabPanel("maps" ,h2("All  country military strenght and its Ranking among all country",align="center"),
                   leafletOutput("military") %>% withSpinner(color = "green"), width = 12),
          tabPanel("charts",h2("All  country military strenght ",align="center"),
                   fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                            tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                            
                   ),selectInput(inputId = "var2" , label ="Select the military force type" , choices = c2),withSpinner(plotlyOutput("bar")),
                   ),
          tabPanel("Data",dataTableOutput("militaryd")),)
          )
        
        
        
      
    #end tabitems
    
    
  )#end body
  
))#end dashboard
server <- function(input, output) 
  {
    #output$timeline1=renderPlot({
     # ggplot(time1,aes(x=year,y=deaths))+geom_line() })
    #output$timeline2=renderPlot({
     # ggplot(time2,aes(x=year,y=deaths))+geom_line()  })
  output$event1=renderDataTable(event1)
  output$datat=renderDataTable(time1)
  output$event2=renderDataTable(event2)
  output$militaryd=renderDataTable(map1)
  output$usdeath2=renderDataTable(time3)
  output$death1tab=renderDataTable(death1)
  output$death2tab=renderDataTable(time4)
  output$structure=renderPrint(death1%>% str())
  output$summary=renderPrint(time4%>%summary())
  output$structure1=renderPrint(death2%>% str())
  output$summary1=renderPrint(time4%>%summary())
  
  
  output$timeline1<-renderHighchart(
    {
      hchart(time1, "line",color="#DC270C",hcaes(x=year,y=deaths_per_capita))  %>%
        
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="World war 1 deaths count",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
    }
  )
  output$timeline2<-renderHighchart(
    {
      hchart(time2, "line",color="#DC270C",hcaes(x=year,y=deaths_per_capita))  %>%
        
        hc_exporting(enabled = TRUE) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="World war 2 deaths count",align="center") %>%
        hc_add_theme(hc_theme_elementary())
      
    }
  )
  
  ### Bar Charts - State wise trend
  output$bar <- renderPlotly({
    map1 %>% 
      plot_ly() %>% 
      add_bars(x=~country, y=~get(input$var2)) %>% 
      layout(title = paste("Country wise Strenght", input$var2),
             xaxis = list(title = "country"),
             yaxis = list(title = paste(input$var2, "count") ))
  })
  output$bar1 <- renderPlotly({
    death1 %>% 
      plot_ly() %>% 
      add_bars(x=~Nationality, y=~get(input$var1)) %>% 
      layout(title = paste("Country wise causalities", input$var1),
             xaxis = list(title = "country"),
             yaxis = list(title = paste(input$var1, "count") ))
  })
  output$bar2 <- renderPlotly({
    death2 %>% 
      plot_ly() %>% 
      add_bars(x=~Nationality, y=~get(input$var3)) %>% 
      layout(title = paste("Country wise causalities", input$var3),
             xaxis = list(title = "country"),
             yaxis = list(title = paste(input$var3, "count") ))
  })
  
  # Rendering table with 5 states with high total military strenght
  output$top5 <- renderTable({
    
    map1 %>% 
      select(country, input$var2) %>% 
      arrange(desc(get(input$var2))) %>% 
      head(5)
    
  })
  # Rendering table with 10 with high casualities
  output$top10 <- renderTable({
    
    death1 %>% 
      select(Nationality, input$var1) %>% 
      arrange(desc(get(input$var1))) %>% 
      head(10)
    
  })
  output$top1 <- renderTable({
    
    death2 %>% 
      select(Nationality, input$var3) %>% 
      arrange(desc(get(input$var3))) %>% 
      head(10)
    
  })
  # Rendering table with 5 states with low Active duty military strenght
  output$low5 <- renderTable({
    
    map1 %>% 
      select(country, input$var2) %>% 
      arrange(get(input$var2)) %>% 
      head(5)
    
    
  })
  # Rendering table with 10 states with low casualities
  output$low10 <- renderTable({
    
    death1 %>% 
      select(Nationality, input$var1) %>% 
      arrange(get(input$var1)) %>% 
      head(10)
    
    
  })
  output$low1 <- renderTable({
    
    death2 %>% 
      select(Nationality, input$var3) %>% 
      arrange(get(input$var3)) %>% 
      head(10)
    
    
  })
  # Rendering the box header  
  output$head1 <- renderText(
    paste("5 country with high rate of", input$var2, "militry strenght")
  )
  
  # Rendering the box header 
  output$head2 <- renderText(
    paste("5 country with low rate of", input$var2, "militry strenght")
  )
  # Rendering the box header  
  output$head3 <- renderText(
    paste("10 country with high rate of", input$var1, "casualities")
  )
  
  # Rendering the box header 
  output$head4 <- renderText(
    paste("10 country with low rate of", input$var1, "casualities")
  )
  # Rendering the box header  
  output$head5 <- renderText(
    paste("10 country with high rate of", input$var3, "casualities")
  )
  
  # Rendering the box header 
  output$head6 <- renderText(
    paste("10 country with low rate of", input$var3, "casualities")
  )
  # wordwar1 map
  output$war1Map <- renderLeaflet({
    leaflet(data=war1map) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(
        ~longitude,
        ~latitude,
        icon=icon("location-dot"),
       label=paste("country name:",war1map$country),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery","Stamen Watercolor"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  # wordwar2 map
  output$war2Map <- renderLeaflet({
    leaflet(data=war2map) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(
        ~Longitude,
        ~Latitude,
        icon=icon("location-dot"),
        label=paste("country name:",war2map$Country),
        clusterOptions = markerClusterOptions()
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery","Stamen Watercolor"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  # world country military map
  output$military <- renderLeaflet({
    leaflet(data=map1) %>% addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      addMarkers(
        ~longitude,
        ~latitude,
        icon=icon("location-dot"),
        label=paste(paste("country ",map1$country),paste(" Ranking:",map1$rank)),
        clusterOptions = markerClusterOptions(),

      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery","Stamen Watercolor"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  }
shinyApp(ui=ui,server=server)
