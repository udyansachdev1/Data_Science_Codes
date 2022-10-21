# --> Set working directory 
setwd("C://Users//udyan.sachdev//OneDrive")
# List of Packages required for thi shiny
# --> Install required packages
list.of.pkgs <- c("shiny","shinydashboard","shinythemes","readxl","reshape2",
                  "shinyWidgets","plotly","dplyr","tidyverse","shinyWidgets",
                  "tm","SnowballC","wordcloud","RColorBrewer","ggplot2","stringr","tidytext",
                  "devtools","visNetwork","syuzhet","magrittr","sentimentr","lexRankr","reactable",
                  "shinycssloaders",'collapsibleTree','leaflet')
new.packages <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
lapply(new.packages, library, character.only = T)
lapply(list.of.pkgs, require, character.only=T)

data <- read.csv("C://Users//udyan.sachdev//OneDrive//")

########################### UI ##############################

ui <- navbarPage(windowTitle ="DLT", title = div(img(src = "CDILOGO2.PNG", id = "nav",
                                                               height = "50px",width = "240px",
                                                               style = "position: relative; margin: -15px -14px; display:left-align;")
),

tabPanel("Overview",
                    fluidPage(id = 'test',
                              tags$style('#test {
                             background-color: #ECF1FD;
              }'),
                    fluidRow(
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("industry_box_v")
                                ),
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("company_box")
                                ),
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("talent_box")
                                ),
                                
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("skill_box")
                                ),
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("change_box")
                                ),
                            column(width = 2,style='padding-top:10px;',
                                    valueBoxOutput("data_box")
                                )),
                              
                  
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                            
                          ),
                          tags$style(".small-box.bg-yellow { background-color: #FFFFFF !important; color: #FFFFFF !important; height:60px;width:155px;margin-top: 10px;}"),
                          # tags$style(HTML(".small-box {margin-bottom: -10px;margin-left: -20px;margin-top: -10px;}")),
                          fluidRow(
                              column(width = 8,style='padding-top:20px;',
                                     leafletOutput("map",height = '520px')),
                              column(width = 4,style='padding-top:20px;',
                                     plotlyOutput("respondents",height = '250px'),plotlyOutput('',height = '20px'), plotlyOutput("stacked_bar",height = '250px'))
                              )
                          
                        #dashboardbody ends here
                      )#dashboardpage ends here
                  ),

tabPanel("Audience Analysis",
         dashboardPage(
           dashboardHeader(disable = T),
           dashboardSidebar(
             sidebarMenu(
               prettyRadioButtons(
                 inputId = "Id2",
                 label = "Choose Audience:",
                 choices = unique(data$Audience),
                 icon = icon("user"),
                 # animation = "tada"
               ),
              prettyRadioButtons(
                inputId = "Id1",
                label = "Choose Industry:",
                choices = unique(data$Industry),
                icon = icon("user"),
                # animation = "tada"
              )
                                        
                                          # multiInput("remove22","Remove Keywords",choices = NULL)
                         
             ) #sidebar menu ends here
           ), # dashboard sidebar ends her
           dashboardBody(
             tags$head(
               # Include our custom CSS
               includeCSS("styles.css")
               #includeScript("gomap.js")
             ),
             # tags$head(
             #   tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
             tags$head(
               tags$style(HTML("
                                 /* logo */
                                 .skin-blue .main-header .logo {
                                 background-color: #56007F;
                                 }
                                 /* logo when hovered */
                                 .skin-blue .main-header .logo:hover {
                                 background-color: #56007F;
                                 }
                                 /* navbar (rest of the header) */
                                 .skin-blue .main-header .navbar {
                                 background-color: #56007F;
                                 }
                                 /* main sidebar */
                                 .skin-blue .main-sidebar {
                                 background-color: #8418b3;
                                 }
                                 /* body */
                                .content-wrapper, .right-side {
                                background-color: #ECF1FD;
                                }

                                 .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black}
                                 .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: black}
                                 .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: black}
                                 .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: black}
                                 ")) #HTML, tags$style ends here
             ),
            
                             fluidRow(
                               column(5,plotlyOutput("L1_rating",height = "300px")
                               ),
                             
                               column(7,plotlyOutput("L1_rating_sunburst",height = "300px")
                               )
                             )
                             
              #tabbox ends here
           #dashboard body ends here
         ) #dashboardpage ends here
)
)
)
           
########################## SERVER ##########################
# data <- read.csv("C://Users//udyan.sachdev//OneDrive")

data = data %>% mutate(Ratings =
                         case_when(rating == 'Strongly Agree' ~ 5,
                                   rating == 'Agree'~ 4,
                                   rating == 'Neutral' ~ 3,
                                   rating == 'Disagree' ~ 2,
                                   rating == 'Strongly Disagree' ~ 1
                         ))

###########data analysis for value boxes
names= c('TP','BC','DC','SC')
answer = c()
for ( i in names){
  Tp_data = data[grep(i, data$Updated_QID), ]
  
  Tp_data_CXO = Tp_data[Tp_data$Audience == 'CXO', ]
  Tp_data_CXO_mean = mean(Tp_data_CXO$Ratings)
  Tp_data_CXO_len = count(Tp_data_CXO)
  
  Tp_data_CDO = Tp_data[Tp_data$Audience == 'CDO', ]
  Tp_data_CDO_mean = mean(Tp_data_CDO$Ratings)
  Tp_data_CDO_len = count(Tp_data_CDO)
  
  Tp_data_DC = Tp_data[Tp_data$Audience == 'Data Consumers', ]
  Tp_data_DC_mean = mean(Tp_data_DC$Ratings)
  Tp_data_DC_len = count(Tp_data_DC)
  
  Tp_data_DE =Tp_data[Tp_data$Audience == 'Data Experts', ]
  Tp_data_DE_mean = mean(Tp_data_DE$Ratings)
  Tp_data_DE_len = count(Tp_data_DE)
  
  Average_Scores_of_Pillar_PersonaL_Level_TP<- as.list(c(Tp_data_CXO_mean, Tp_data_CDO_mean,Tp_data_DC_mean,Tp_data_DE_mean))
  Employee_for_each_Personell <- c(Tp_data_CXO_len, Tp_data_CDO_len,Tp_data_DC_len,Tp_data_DE_len)
  
  TP = data.frame(unlist(Average_Scores_of_Pillar_PersonaL_Level_TP),unlist(Employee_for_each_Personell))
  Final_TP_Score = weighted.mean(TP$unlist.Average_Scores_of_Pillar_PersonaL_Level_TP.,TP$unlist.Employee_for_each_Personell.)
  
  answer <- append(answer, Final_TP_Score)
  
}
answer = round(answer, digit = 1)
Final_TP_Score = answer[1]
Final_BC_Score = answer[2]
Final_DC_Score= answer[3]
Final_SC_Score= answer[4]

DLT_Score = round(mean(answer),digit =1)

mrg <- list(l = 50, r = 0,
             b = 50, t = 50,
             pad = 20)
mrg_pillar <- list(l = 180, r = 30,
                   b = 50, t = 50,
                   pad = 20)
mrg_map <- list(l = 0, r = 0,
            b = 0, t = 0,
            pad = -5)

t_1 <- list(
  family = "Graphik",
  size = 11,
  color = 'black')

t <- list(
  family = "Graphik",
  size = 9,
  color = 'black')

t1 <- list(
  family = "Graphik",
  size = 12,
  color = 'black')


t2 <- list(
  family = "Graphik",
  size = 10,
  color = 'black')

###Map
server <- function(input,output){
  
  output$map <- renderLeaflet({
    leaflet() %>%clearShapes() %>%clearMarkers()%>%clearPopups()%>%
      addTiles(
        # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
        # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -9.776, lat = 21.0534, zoom = 2)
  })
  
  observe({
  
  map_data =  data %>% group_by(Country) %>%
    summarise(Avg_Rating = mean(Ratings),.groups = 'drop')
  map_data$Avg_Rating <- round(map_data$Avg_Rating,digit=1) # Round off the column to integer
  
  #####add long and lat
  map_data = map_data %>% mutate(lat =
                                   case_when((Country == 'Australia')~ -25.274398	,
                                             (Country == 'China')~ 35.86166,
                                             (Country == 'France')~ 46.227638,
                                             (Country == 'Germany')~ 51.165691,
                                             (Country == 'India')~ 20.593684,
                                             (Country == 'Japan')~ 36.204824,
                                             (Country == 'Singapore')~ 1.352083	,
                                             (Country == 'United Arab Emirates')~ 23.424076,
                                             (Country == 'United Kingdom')~ 55.378051,
                                             (Country == 'United States')~ 37.09024
                                             
                                   ))
  map_data = map_data %>% mutate(long =
                                   case_when((Country == 'Australia')~ 133.775136,
                                             (Country == 'China')~ 104.195397,
                                             (Country == 'France')~ 2.213749,
                                             (Country == 'Germany')~ 10.451526,
                                             (Country == 'India')~ 78.96288	,
                                             (Country == 'Japan')~ 	138.252924,
                                             (Country == 'Singapore')~ 103.819836,
                                             (Country == 'United Arab Emirates')~ 53.847818,
                                             (Country == 'United Kingdom')~ -3.435973,
                                             (Country == 'United States')~ -95.712891
                                      ))
  
  
  map_data = map_data %>% mutate(Color_Code =
                                           case_when((Avg_Rating >= 1 & Avg_Rating < 3.5)~ '#B948FF',
                                                     (Avg_Rating >= 3.5 & Avg_Rating < 4)~ '#4E26E2',
                                                     (Avg_Rating >= 4 & Avg_Rating <= 5)~ '#950706',
                                           ))
  
  
  req_map_data1 = map_data 
  # req_map_data1=map_data[,c('Country','Longitude','Latitude','ACLI','Color_Code')]
  # req_map_data1$ACLI=round(req_map_data1$ACLI,2)
  req_map_data1$Country <- gsub('United States', 'United States of America', req_map_data1$Country)
  
  req_map_data1=unique(req_map_data1)
  WorldCountry <-geojsonio::geojson_read("countries.json", what = "sp")
  
  data_Map <- WorldCountry[WorldCountry$name %in% req_map_data1$Country, ]
  req_list=c(unique(as.character(req_map_data1$Country)))
  
  req_map_data11<-req_map_data1[req_map_data1$Country %in% req_list,]
  color_code_data=unique(req_map_data11[order(req_map_data11$Country),c('Country','Color_Code')])
  
  pin=makeIcon("map-pin.png",iconWidth = 18,iconHeight = 18)
  leafletProxy("map", data = data_Map) %>%clearMarkers()%>%clearPopups()%>%
    clearShapes() %>% addTiles() %>% addPolygons(stroke=FALSE, color =color_code_data$Color_Code,fillOpacity = 1,opacity = 1
                                                 
    )%>%
    addMarkers(
      icon = pin,
      lng = map_data$long,lat = map_data$lat,
      popup = paste("Country : ",map_data$Country,"<br>",
                    "Average Rating",map_data$Avg_Rating,
                    "</b>","<br>")
    )%>%
    
  addLegend("bottomright", title="Average Rating",
            colors = c("#950706","#4E26E2","#B948FF"),
            labels = c("High","Medium","Low"),
            # labels = c("Extreme: 0 - 2.5","High: 2.5 - 5","Moderate: 5 - 7.5","Low: Greater than 7.5"),
            layerId="colorLegend")
  
})
  
  
  
output$respondents <- renderPlotly({
  
  donut_data <- data %>% group_by(Audience)  %>%
    summarise(Count = n(),.groups = 'drop')
  
  donut_data = donut_data %>% arrange(desc(Count))
  
  fig <- plot_ly(donut_data,labels = ~Audience, values = ~Count,textposition = 'outside',
                 textinfo = 'label+percent',
                 # marker = list(colors = c("#8222D1","#E4C1F9","#D4C1F9","#C7BAEE","#B5C5EA","#EAD9F6","#E6CFF7","#E2B9F0","#EDCFF7","#D1CFF7","#8F286E","#A657EB","#A478BE","#D79CFC","#C57CF2","#C181F5","#F5CBFB","#E9D4F9","#E2C3FB","#D8B1F8","#B8B1F8","#CDB1F8","#EEC4F7","#E9B8F4","#DEB2E8","#D7AAD8","#CA9BCB","#F4CBF5","#F7C3F8","#DCA0DD","#F6BBF7","#E0B5E1","#D1B3D2","#F4ADF6","#F9C0FB","#F5AAF8","#F6D3F7","#F6D3F7","#EED3F7","#ECC3F9","#ECD7F3"))) %>% 
                 marker = list(colors = c("#BD39E5","#3955E5","#4ED6F5","#3E0D91","#1AE729","#F325BE","#E2EF0D","#6411BB","#943CF1","#3C3EF1","#A721CB","#6F1488","#E9209C","#BF20E9","#E2F126","#2659F1"))) %>%
    add_pie(hole = 0.4)  %>%
    layout(title = "", font = t1, showlegend = F, margin = mrg,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
    config(displayModeBar = F)
  
  fig <- fig %>% layout(uniformtext=list(minsize=8, mode='hide'))
  
  fig
  
  
  
})


output$stacked_bar <- renderPlotly({
 
  stackedbar_data = data %>% group_by(Country) %>% summarise(Count=n()) %>% mutate(percent=Count/sum(Count)*100)
  stackedbar_data$percent <- round(stackedbar_data$percent,digit=1)
  
  stackedbar_data = stackedbar_data[order(-stackedbar_data$Count),]
  

  plot_ly(data = stackedbar_data,y = ~Country , x = ~Count, type = 'bar',orientation = 'h',
          hoverinfo = "text",text = stackedbar_data$Count, textposition = "inside",textfont = list(size = 10,color = "black"),
          hovertext = paste("Country :", stackedbar_data$Country,
                            "<br>Percent :",stackedbar_data$percent),
          marker = list(color=c("#9A4FEA","#F7347B","#48C6DE","#000C0E","#1C11D2","#245FA7","#038BF2","#0086E9","#034EF2","#0403F2")
          )) %>%
    layout(title = '',titlefont = list(size = 16),margin = mrg_pillar,xaxis = list(title = "",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
           yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                        title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')),bargap = 0.6,showlegend = FALSE)%>% 
    config(displayModeBar = F)
  

  
})

########tab2 , count of rating by L1
#######Sunburst function
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := " "]
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}


################### bar chart for rating count 
output$L1_rating <- renderPlotly({
  
L1_rating_data =  data[data$Industry==input$Id1,]
L1_rating_data =  L1_rating_data[L1_rating_data$Audience==input$Id2,]

L1_rating_data  = L1_rating_data  %>% group_by(rating) %>%
                  summarise(Count = n(),.groups = 'drop')
L1_rating_data = L1_rating_data[order(-L1_rating_data$Count),]

plot_ly(data = L1_rating_data,y = ~rating , x = ~Count, type = 'bar',orientation = 'h',
        hoverinfo = "text",text = L1_rating_data$Count, textposition = "inside",textfont = list(size = 10,color = "black"),
        hovertext = paste("Rating :", L1_rating_data$rating,
                          "<br>Count :",L1_rating_data$Count),
        marker = list(color=c("#9A4FEA","#F7347B","#48C6DE","#000C0E","#1C11D2","#245FA7","#038BF2","#0086E9","#034EF2","#0403F2","#6203F2","#BC03F2","#F203E3","#F20397")
        )) %>%
  # marker = list(color=c('00ff45','#00ff45','#eda050','#eda050')))%>%
  layout(title = '',titlefont = list(size = 16),margin = mrg_pillar,xaxis = list(title = "",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
         yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                      title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')),bargap = 0.6,showlegend = FALSE)%>% 
  config(displayModeBar = F)

})

###########sunburst for click

output$L1_rating_sunburst <- renderPlotly({

  L1_sunburst_data =  data[data$Industry==input$Id1,]
  L1_sunburst_data =  L1_sunburst_data[L1_sunburst_data$Audience==input$Id2,]

  L1_sunburst_data <- subset(L1_sunburst_data, select = c(L1,L2,L3,Formatted_Questions))
  L1_sunburst_data$Keyword.Frequency <- 1
  sunburstDF <- as.sunburstDF(L1_sunburst_data, value_column = "Keyword.Frequency", add_root = TRUE)
  plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, parents = ~parents,
          values= ~values, type='treemap', branchvalues = 'total',  maxdepth=2) %>%
    config(displayModeBar = F)
  
  

})


output$persona_bar <- renderPlotly({
  
  persona_data  = data  %>% group_by(Audience) %>%
    summarise(Avg_Rating = mean(Ratings),.groups = 'drop')
  persona_data$Avg_Rating <- round(persona_data$Avg_Rating,digit=1)
  
  persona_data = persona_data %>% mutate(Color_Code =
                           case_when((Avg_Rating >= 1 & Avg_Rating < 3.5)~ '#f51717',
                                     (Avg_Rating >= 3.5 & Avg_Rating < 4)~ '#eda050',
                                     (Avg_Rating >= 4 & Avg_Rating <= 5)~ '#00ff45',
                           ))
  
  plot_ly(data = persona_data,x = ~Avg_Rating , y = ~Audience, type = 'bar',orientation = 'h',color = ~Audience,colors = ~Color_Code,
          hoverinfo = "text",text = persona_data$Avg_Rating, textposition = "inside",textfont = list(size = 10,color = "black"),
          hovertext = paste("Audience :", persona_data$Audience,
                            "<br>Rating :",persona_data$Avg_Rating)) %>%
    layout(title = 'Persona',titlefont = list(size = 16),margin = mrg,xaxis = list(title = "",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
           yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                        title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')),bargap = 0.6,showlegend = FALSE)%>% 
    config(displayModeBar = F)
  
  
})

output$country_bar <- renderPlotly({
  
country_data =  data %>% group_by(Country) %>%
  summarise(Avg_Rating = mean(Ratings),.groups = 'drop')
country_data$Avg_Rating <- round(country_data$Avg_Rating,digit=1) # Round off the column to integer

country_data = country_data %>% mutate(Color_Code =
                                         case_when((Avg_Rating >= 1 & Avg_Rating < 3.5)~ '#f51717',
                                                   (Avg_Rating >= 3.5 & Avg_Rating < 4)~ '#eda050',
                                                   (Avg_Rating >= 4 & Avg_Rating <= 5)~ '#00ff45',
                                         ))

plot_ly(data = country_data,x = ~Avg_Rating , y = ~Country, type = 'bar',orientation = 'h',color = ~Country,colors = ~Color_Code,
        hoverinfo = "text",text = country_data$Avg_Rating, textposition = "inside",textfont = list(size = 10,color = "black"),
        hovertext = paste("Audience :", country_data$Country,
                          "<br>Rating :",country_data$Avg_Rating)) %>%
  # marker = list(color=c('00ff45','#00ff45','#eda050','#eda050')))%>%
  layout(title = 'Countries',titlefont = list(size = 16),margin = mrg,xaxis = list(title = "",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
         yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                      title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')),bargap = 0.4,showlegend = FALSE)%>% 
  config(displayModeBar = F)

})
output$pillar_bar <- renderPlotly({
  
  # pillar_data  = data  %>% group_by(L1) %>%
  #   summarise(Avg_Rating = mean(Ratings),.groups = 'drop')
  # pillar_data$Avg_Rating <- round(pillar_data$Avg_Rating,digit=1)
  # 
  
  L1 <- c('Skills and Capabilities', 'Talent Practice', 'Data Culture', 'Business Change & Adoption')
  Avg_Rating <- c(answer[4], answer[1], answer[3], answer[2])
  pillar_data <- data.frame(L1, Avg_Rating)
  
  pillar_data = pillar_data %>% mutate(Color_Code =
                                           case_when((Avg_Rating >= 1 & Avg_Rating < 3.5)~ '#f51717',
                                                     (Avg_Rating >= 3.5 & Avg_Rating < 4)~ '#eda050',
                                                     (Avg_Rating >= 4 & Avg_Rating <= 5)~ '#00ff45',
                                           ))

  plot_ly(data = pillar_data,x = ~Avg_Rating , y = ~L1, type = 'bar',orientation = 'h',color = ~L1,colors = ~Color_Code,
          hoverinfo = "text",text = pillar_data$Avg_Rating, textposition = "inside",textfont = list(size = 10,color = "black"),
          hovertext = paste("Audience :", pillar_data$L1,
                            "<br>Rating :",pillar_data$Avg_Rating)) %>%
          # marker = list(color=c('00ff45','#00ff45','#eda050','#eda050')))%>%
    layout(title = 'Pillars',titlefont = list(size = 16),margin = mrg_pillar,xaxis = list(title = "",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
           yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                        title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')),bargap = 0.6,showlegend = FALSE)%>% 
    config(displayModeBar = F)
  
  
})


output$industry_box_v <- renderValueBox({
  
  valueBox(
    value = tags$p("Industry Score", align = "left",style='margin-left:25px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong('4.1') , align = "left",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style = "color:#00ff45"),color = 'yellow'
  )
})


output$company_box <- renderValueBox({
  if ((DLT_Score >= 1 & DLT_Score < 3.5)) {
    x =  "color:#f51717"
  } else if ( (DLT_Score >= 3.5 & DLT_Score < 4)) {
    x ="color:#eda050"
  } else {
    x = "color:#00ff45"
  }
  valueBox(
    value = tags$p("Company Score", align = "left",style='margin-left:20px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong(DLT_Score) , align = "left",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style = x),color = 'yellow'
  )
})

output$talent_box <- renderValueBox({
  
  if ((Final_TP_Score >= 1 & Final_TP_Score < 3.5)) {
   x =  "color:#f51717"
  } else if ( (Final_TP_Score >= 3.5 & Final_TP_Score < 4)) {
    x ="color:#eda050"
  } else {
    x = "color:#00ff45"
  }
  valueBox(
    value = tags$p("Talent & Practice", align = "left",style='margin-left:15px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong(Final_TP_Score) , align = "left",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style = x),color = 'yellow'
  )
})
output$skill_box <- renderValueBox({
  if ((Final_SC_Score >= 1 & Final_SC_Score < 3.5)) {
    x =  "color:#f51717"
  } else if ( (Final_SC_Score >= 3.5 & Final_SC_Score < 4)) {
    x ="color:#eda050"
  } else {
    x = "color:#00ff45"
  }
  valueBox(
    value = tags$p("Skills & Capabilities", align = "left",style='margin-left:10px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong(Final_SC_Score), align = "left",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style = x),color = 'yellow'
  )
})
output$change_box <- renderValueBox({
  if ((Final_BC_Score >= 1 & Final_BC_Score < 3.5)) {
    x =  "color:#f51717"
  } else if ( (Final_BC_Score >= 3.5 & Final_BC_Score < 4)) {
    x ="color:#eda050"
  } else {
    x = "color:#00ff45"
  }
  valueBox(
    value = tags$p("Change & Adoption", align = "left",style='margin-left:10px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong(Final_BC_Score) , align = "left",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style =x),color = 'yellow'
)
})
output$data_box <- renderValueBox({
  if ((Final_DC_Score >= 1 & Final_DC_Score < 3.5)) {
    x =  "color:#f51717"
  } else if ( (Final_TP_Score >= 3.5 & Final_DC_Score < 4)) {
    x ="color:#eda050"
  } else {
    x = "color:#00ff45"
  }
  valueBox(
    value = tags$p("Data Culture" , align = "left",style='margin-left:25px;',style = "font-size:14px;",style = "font-family: Arial;",style = "color:black"),
    tags$p(strong(Final_DC_Score) , align = "centre",style='margin-left:55px;',style = "font-size:16px;",style = "font-family: Arial;",style = x),color = 'yellow')
})


######tab2 
##Likert analysis 
# Likert Scale PLot DIstribution
output$likert_chart <- renderPlotly({
  
data_survey_1 = data
data_survey_1=data_survey_1[complete.cases(data_survey_1),]
data_survey_1=data_survey_1[data_survey_1$Industry==input$Id1,]
unique(data_survey_1$Industry)
likeart_chart_data=data_survey_1%>%group_by(L1,rating)%>%summarize(Count=n())%>%as.data.frame()

d1<-reshape2::dcast(likeart_chart_data, L1~ rating, value.var="Count")
d1[is.na(d1)] <- 1
d1<-d1 %>% 
  as.data.frame() %>%
  mutate(newSum = select_if(., is.numeric) %>% 
           purrr::reduce(`+`)) %>% 
  mutate_if(is.numeric, list(~ ./newSum*100)) %>% 
  select(-newSum)%>% 
  mutate_if(is.numeric, round,digits=0)

fig <- plot_ly(d1, x = ~`Strongly Disagree`, y = ~L1, type = 'bar',name='Strongly Disagree',
               marker = list(color= '#D048FF', #'#4E26E2',
                             # color = 'rgba(38, 24, 74, 0.8)',
                             line = list(color = 'rgb(248, 248, 249)')))%>%layout(barmode = 'stack') 
fig <- fig %>% add_trace(x = ~`Disagree`,name='Disagree', marker = list(color = '#B948FF' #'#7C2FEC'
                                                                        # 'rgba(71, 58, 131, 0.8)'
)) 
fig <- fig %>% add_trace(x = ~`Neutral`,name='Neutral', marker = list(color='#953DF3'
                                                                      # color = 'rgba(122, 120, 168, 0.8)'
))
fig <- fig %>% add_trace(x = ~`Agree`,name='Agree', marker = list(color= '#7C2FEC'#'#B948FF'
                                                                  # color = 'rgba(190, 192, 213, 1)'
))
fig <- fig %>% add_trace(x = ~`Strongly Agree`,name='Strongly Agree', marker = list(color= '#4E26E2'#'#D048FF'
                                                                                    # color = 'rgba(190, 192, 213, 1)'
))

fig <- fig %>% layout(
  legend=list(
    orientation='h',
    yanchor="center",
    xanchor = "center",  # use center of legend as anchor
    x = 0.5,
    y=-0.0625
    # title=list(text='Response Dirstribution across Pillars')
  ),
  font=t1,
  bargap=0.5,
  # showlegend=F,
  title=paste('<b>','Questionnaire Response Dirstribution across Pillars','</b>'),
  xaxis = list(title = "",
               showgrid = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               zeroline = FALSE,
               domain = c(0.15, 1)),
  yaxis = list(title = "",
               showgrid = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               zeroline = FALSE),
  barmode = 'stack',
  paper_bgcolor = 'rgba(245, 246, 249, 1)',
  plot_bgcolor = 'rgba(245, 246, 249, 1)'
)
# # labeling the y-axis
fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = d1$Business_details,
                               xanchor = 'right',
                               text = paste("<b>",d1$L1,"</b>"),
                               font = list(family = 'Graphik', size = 12,
                                           color = 'rgb(67, 67, 67)'),
                               showarrow = FALSE, align = 'right')

fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = (d1$`Strongly Disagree` / 2), y = d1$L1,
                               text = paste(d1$Disagree, '%'),
                               font = list(family = 'Graphik', size = 7,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE)
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = (d1$`Strongly Disagree`+d1$Disagree/2), y = d1$L1,
                               text = paste(d1$Disagree, '%'),
                               font = list(family = 'Graphik', size = 7,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE)
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = (d1$`Strongly Disagree`+(d1$Disagree)+d1$Neutral/2), y = d1$L1,
                               text = paste(d1$Neutral, '%'),
                               font = list(family = 'Graphik', size = 7,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE)

fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = ((d1$`Strongly Disagree`)+(d1$Disagree)+(d1$Neutral)+d1$Agree/2), y = d1$L1,
                               text = paste(d1$Agree, '%'),
                               font = list(family = 'Helvetica', size = 7,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE)%>% config(displayModeBar = F)
fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                               x = ((d1$`Strongly Disagree`)+(d1$Disagree)+(d1$Neutral)+d1$Agree+(d1$`Strongly Agree`)/2), y = d1$L1,
                               text = paste(d1$`Strongly Agree`, '%'),
                               font = list(family = 'Helvetica', size = 7,
                                           color = 'rgb(248, 248, 255)'),
                               showarrow = FALSE)%>% config(displayModeBar = F)
# labeling the first Likert scale (on the top)
fig
})



# Reponse Across Audience
output$response_audience_chart <- renderPlotly({
  
data_survey_2=data
data_survey_2=data_survey_2[data_survey_2$Industry==input$Id1,]

data_survey_2$rating=ifelse(data_survey_2$rating=="Strongly Disagree",1,
                            ifelse(data_survey_2$rating=="Disagree",2,
                                   ifelse(data_survey_2$rating=="Neutral",3,
                                          ifelse(data_survey_2$rating=="Agree",4,
                                                 5
                                          )
                                   )
                            )
)
polar_chart_data=data_survey_2%>%group_by(Audience,rating)%>%summarize(Count=n())%>%as.data.frame()

polar_chart_data_med=data_survey_2%>%group_by(Audience)%>%summarise(Median=median(rating),Average=mean(rating))%>%as.data.frame()
polar_chart_data=merge(polar_chart_data,polar_chart_data_med)
polar_chart_data$rating=ifelse(polar_chart_data$rating==1,"Strongly Disagree",
                               ifelse(polar_chart_data$rating==2,"Disagree",
                                      ifelse(polar_chart_data$rating==3,"Neutral",
                                             ifelse(polar_chart_data$rating==4,"Agree",
                                                    "Strongly Agree"
                                             )
                                      )
                               )
)


polar_chart_data$color_code=ifelse(polar_chart_data$rating=="Strongly Disagree",'#D048FF',
                                   ifelse(polar_chart_data$rating=="Disagree",'#B948FF',
                                          ifelse(polar_chart_data$rating=="Neutral",'#953DF3',
                                                 ifelse(polar_chart_data$rating=="Agree",'#7C2FEC',
                                                        '#4E26E2'
                                                 )
                                          )
                                   )
)

polar_chart_data_grp=polar_chart_data %>% group_by(Audience) %>% mutate(percent = Count/sum(Count)*100) %>% as.data.frame()
polar_chart_data_grp$percent=round(polar_chart_data_grp$percent,0)


polar_chart_data_grp_uni=polar_chart_data_grp[,c("Audience","Median")]
polar_chart_data_grp_uni=polar_chart_data_grp_uni[!duplicated(polar_chart_data_grp_uni),]

fig<-plot_ly(polar_chart_data_grp_uni, x = ~Audience,
             y = ~Median,
             type = 'bar',
             text=~Median,
             textposition='outside',
             # name=input$dist_D2,
             marker=list(color="#247AF7"))

fig<-fig %>% layout(
  font=t1,
  bargap=0.5,
  margin = list(r = 40),
  title=paste('<b>','','</b>'),
  xaxis = list(title = "",showgrid = FALSE,
               showline = FALSE),
  yaxis = list(title = paste('<b>',"Average Response",'</b>'),showgrid = FALSE,
               showline = FALSE,range=c(0,5)
  ),
  paper_bgcolor = 'rgba(245, 246, 249, 1)',
  plot_bgcolor = 'rgba(245, 246, 249, 1)')%>% config(displayModeBar = F)

peak_line_data=polar_chart_data_grp[polar_chart_data_grp$Audience %in% c("Data Experts","CDO"),]
peak_line_data$anchor=ifelse(peak_line_data$Audience=="CDO","right","left")
# peak_line_data2=polar_chart_data_grp[polar_chart_data_grp$Audience=="CDO",]
fig1 <-plot_ly(polar_chart_data_grp,x=~Audience,y=~percent,type='scatter',mode='lines+markers',
               color=~rating,colors=~color_code
)

fig1<-fig1 %>% layout(
  annotations = list(x = peak_line_data$Audience, 
                     y = peak_line_data$percent, text = peak_line_data$rating, showarrow = F,
                     font=t1,xanchor = peak_line_data$anchor
  ),
  # annotations = list(x = peak_line_data2$Audience, 
  #                    y = peak_line_data2$percent, text = peak_line_data2$rating, showarrow = F,
  #                    font=t1,xanchor = 'right'
  # ),
  legend=list(
    orientation='h',
    yanchor="center",
    xanchor = "center",  # use center of legend as anchor
    x = 0.5,
    y=-0.0625
    # title=list(text='<b> Trend </b>')
  ),
  font=t1,
  barmode = 'group',
  hovermode = "x unified",
  margin = list(r = 40),
  title=paste('<b>','','</b>'),
  xaxis = list(title = "",showgrid = FALSE,
               showline = FALSE),
  yaxis = list(title = paste('<b>',"Response Percentage",'</b>'),showgrid = FALSE,
               showline = FALSE
  ),
  paper_bgcolor = 'rgba(245, 246, 249, 1)',
  plot_bgcolor = 'rgba(245, 246, 249, 1)')%>% config(displayModeBar = F)
fig1

subplot(style(fig, showlegend = F),fig1,heights = c(0.4, 0.6),nrows = 2, shareX = T, shareY = F,titleY = T)

})

# Audience & Pillars

output$audience_pillar_chart <- renderPlotly({
  
data_survey_1 = data
data_survey_1=data_survey_1[data_survey_1$Industry== input$Id3,]

data_sankey=data_survey_1[,c("Audience","L1","rating")]

data_sankey=data_survey_1 %>% group_by(Audience,L1,rating) %>% summarise(Count=n()) %>% mutate(percent=Count/sum(Count)*100)
data_sankey$percent=round(data_sankey$percent,1)

data_sankey$color_code=ifelse(data_sankey$rating=="Strongly Disagree",'#D048FF',
                              ifelse(data_sankey$rating=="Disagree",'#B948FF',
                                     ifelse(data_sankey$rating=="Neutral",'#953DF3',
                                            ifelse(data_sankey$rating=="Agree",'#7C2FEC',
                                                   '#4E26E2'
                                            )
                                     )
                              )
)


cdo_data=data_sankey[data_sankey$Audience== input$Id2,]
fig <- plot_ly(
  x = cdo_data$rating, y = cdo_data$L1,
  z = cdo_data$percent, type = "heatmap",
  # colors=cdo_data$color_code,
  # colors = colorRamp(c('#E200E2','#B102B1','#800080')),
  # colors = colorRamp(c('#AA84FF','#8048FD','#4A00F1','#4704DD')),
  # colors = colorRamp(c('#D048FF','#B948FF','#953DF3','#7C2FEC','#4E26E2')),
  colors = colorRamp(c('#C3A8FF','#7538FE','#4A00F1')),
  hoverinfo='text',
  xgap=15,
  ygap=15,
  showgrid=FALSE,
  text=paste("Response:",cdo_data$rating,"<br>","L1:",cdo_data$L1,"<br>","Percent:",cdo_data$percent)
  # ,showscale = FALSE
  # ,colors = colorRamp(c('#800080','#8D049A','#8D023A')),
)%>%
  add_annotations(x = cdo_data$rating,
                  y = cdo_data$L1,
                  text = paste("<b>",cdo_data$percent,"%","</b>"),
                  showarrow = FALSE,
                  font = list(color = "white"),
                  ax = 20,
                  ay = -20)%>%
  layout(
    
    xaxis = list(title = paste("<b>","Response","</b>"),categoryorder = "array",
                 ticktext = sprintf("<b>%s</b>", levels(factor(cdo_data$rating))),
                 tickvals = levels(factor(cdo_data$rating)),
                 categoryarray = c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")),
    yaxis = list(title = list(text=paste("<b>","L1","</b>"),standoff = 5L
    ),
    ticktext = sprintf("<b>%s</b>", levels(factor(cdo_data$L1))),
    tickvals = levels(factor(cdo_data$L1))
    ),
    title=paste("<b>",input$Id2,"</b>"),
    font=t1,
    paper_bgcolor = 'rgba(245, 246, 249, 1)',
    plot_bgcolor = 'rgba(245, 246, 249, 1)')%>% config(displayModeBar = F)%>% colorbar(title = paste("<b>","Percent","</b>"),len=0.85,thickness=15)

fig
})

##################################################################################
output$collapsable_tree <- renderCollapsibleTree({
data_survey_1 = data
data_survey_1=data_survey_1[data_survey_1$Industry== input$Id5,]

# Collapsable Tree
data_ct=data_survey_1[,c("Audience","rating","L1","L2","L3","Country")]

data_ct=data_ct %>% group_by(Audience,rating,L1,L2,L3,Country) %>% summarise(Count=n()) 

data_ct=data_ct[data_ct$Audience==input$Id4,]
# grp_data=data_ct[data_ct$Audience=="CDO",c("rating","L1","L2","L3","Count","color_code")]
# grp_data$Transactions_t=normalize(grp_data$Count,T)*100



collapsibleTreeSummary(data_ct, hierarchy = c("rating","L1","L2","L3"), 
                       root = input$Id4,
                       fontSize = 12,percentOfParent = T,
                       # nodeSize = "Transactions_t",
                       # inputId = NULL, 
                       attribute = "Count",
                       linkLength = 180,
                       fillFun=colorspace::diverging_hsv,
                       # fillFun=colorspace::terrain_hcl,
                       # fillFun=colorspace::diverge_hcl,
                       # fillFun=colorspace::heat_hcl,
                       # fillFun=colorspace::rainbow_hcl,
                       maxPercent = 35
                       
)
})





}
shinyApp(ui = ui, server = server)
