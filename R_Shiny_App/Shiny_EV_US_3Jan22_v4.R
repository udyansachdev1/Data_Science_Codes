library(plotly)
library(shinythemes)
library(shiny)
library(dplyr)
library(summarytools)
library(RColorBrewer)
 

ui <- navbarPage(title = "EV_Dashboard",
                 theme = shinytheme("superhero"),
                 navbarMenu("Specification Comparison",
                            
                            # tab panel 1
                            tabPanel("Between Different Models",
                                     sidebarLayout(position = "right",
                                                   sidebarPanel(
                                                     selectInput("s", "Select a Specification:  (X axis)",
                                                                 list("AccelSec"='a1', 
                                                                      "TopSpeed_KmH"='b1', 
                                                                      "Range_Km"='c1',
                                                                      "Battery_Pack Kwh" = 'd1',
                                                                      "Efficiency_WhKm" ='e1',
                                                                      "FastCharge_Km/H" = 'f1',
                                                                      "RapidCharge"='g1',
                                                                      "PowerTrain"='h1',
                                                                      "PlugType" = 'i1',
                                                                      "BodyStyle" = 'j1',
                                                                      "Segment" = 'k1',
                                                                      "Seats"= 'l1',
                                                                      "PriceEuro"='m1')), 
                                                     #taking input k using radiobuttons       
                                                     selectInput("k", "Select a Specification: (Y axis)",               
                                                                 list("AccelSec"='a2', 
                                                                      "TopSpeed_KmH"='b2', 
                                                                      "Range_Km"='c2',
                                                                      "Battery_Pack Kwh" = 'd2',
                                                                      "Efficiency_WhKm" ='e2',
                                                                      "FastCharge_Km/H" = 'f2',
                                                                      "RapidCharge"='g2',
                                                                      "PowerTrain"='h2',
                                                                      "PlugType" = 'i2',
                                                                      "BodyStyle" = 'j2',
                                                                      "Segment" = 'k2',
                                                                      "Seats"= 'l2',
                                                                      "PriceEuro"='m2'))
                                                   ),
                                                   mainPanel(
                                                     tabsetPanel(
                                                       tabPanel(plotlyOutput("distPlot"))                       
                                                     )
                                                   ) 
                                     )
                            ),
                            tabPanel("Correlation stats",
                                     plotOutput("mytable8")
                            )
                 ),
#                  # tab panel 
#                  tabPanel("Charger types and stae wise count",
#                       sidebarLayout(position = "right",
#                               sidebarPanel(
#                              selectInput("m", "Select Type of Charger",
#                                   list("EV Level1 EVSE Num"='a3', 
#                                             "EV Level2 EVSE Num"='b3', 
#                                              "EV DC Fast Count"='c3'))),
#                           mainPanel(
#                             tabsetPanel(
#                               tabPanel(plotlyOutput("displot1"))
#                             )
#                           )
#                  )
# ),
# tab panel 


tabPanel("Year wise model sale and models available",
         fluidPage(
           fluidRow(
             column(width = 12,
                    plotlyOutput("displot2")
             )),
             fluidRow(
               column(width = 12,div(style = "height:10px"),
                    plotlyOutput("displot7")))
           )
         
),

tabPanel("Registered EVs and Charging Location",
         fluidPage(
           fluidRow(
             column(width = 6,
                    plotlyOutput("displot3")
             ),
             column(width = 6,div(style = "height:2px"),
                    plotlyOutput("displot4"))
           ),
             sidebarLayout(position = "left",
                         sidebarPanel(
                           selectInput("m", "Select Type of Charger",
                                       list("EV Level1 EVSE Num"='a3', 
                                            "EV Level2 EVSE Num"='b3', 
                                            "EV DC Fast Count"='c3'))),
                         mainPanel(
                           tabsetPanel(
                             tabPanel(plotlyOutput("displot1"))
                           )
                         )
           )
)
),

tabPanel("County Wise sales",
         fluidPage(
           fluidRow(
             column(width = 12,
                    plotlyOutput("displot9")
             )),
           fluidRow(
             column(width = 12,div(style = "height:10px"),
                    plotlyOutput("displot10")))
         )
         
),
             
tabPanel("Registered EVs and Incentives Provided",
         fluidPage(
           fluidRow(
             column(width = 6,
                    plotlyOutput("displot5")
             ),
             column(width = 6,
                    plotlyOutput("displot6"))
           )
         )
),

navbarMenu("Data Summary",
           tabPanel("Model Comparison",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable"))
                      )
                    
                    
           ),
           tabPanel("Charging Density",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable1"))
                      )
                    
                    
           ),
           tabPanel("Laws and Incentives",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable2"))
                      )
                    
                    
           ),
           tabPanel("Model wise sales from 2011-19",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable3"))
                      )
                    
                    
           ),
           tabPanel("ELEC and PHEV models and specs from 1995-2022",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable4"))
                      )
                    
                    
           ),
           tabPanel("Model Availability fuel wise",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable5"))
                      )
                    
                    
           ),
           tabPanel("EV Sales",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable6"))
                      )
                    
                    
           ),
           tabPanel("EV Sales in California",
                      tabsetPanel(
                        tabPanel(dataTableOutput("mytable7"))
                      )
           )
)
)

server <- function(input, output, session) {
  output$distPlot <- renderPlotly({    
    if(input$s=='a1'){ i<-3 }     
    if(input$s=='b1'){ i<-4 }     
    if(input$s=='c1'){ i<-5 } 
    if(input$s=='d1'){ i<-6 }
    if(input$s=='e1'){ i<-7 }
    if(input$s=='f1'){ i<-8 }
    if(input$s=='g1'){ i<-9 }
    if(input$s=='h1'){ i<-10 }
    if(input$s=='i1'){ i<-11 }
    if(input$s=='j1'){ i<-12 }
    if(input$s=='k1'){ i<-13 }
    if(input$s=='l1'){ i<-14 }
    if(input$s=='m1'){ i<-15 }
    if(input$k=='a2'){ j<-3 }     
    if(input$k=='b2'){ j<-4 }     
    if(input$k=='c2'){ j<-5 }
    if(input$k=='d2'){ j<-6 }     
    if(input$k=='e2'){ j<-7 }
    if(input$k=='f2'){ j<-8 }     
    if(input$k=='g2'){ j<-9 }     
    if(input$k=='h2'){ j<-10}     
    if(input$k=='i2'){ j<-11}     
    if(input$k=='j2'){ j<-12}     
    if(input$k=='k2'){ j<-13}     
    if(input$k=='l2'){ j<-14} 
    if(input$k=='m2'){ j<-15}     
    
  
    #read data
    Specification_X_Axis <- Model_Compare [, i]   
    Specification_Y_Axis <- Model_Compare [, j] 
    plot_ly(x = ~Specification_X_Axis,y = ~Specification_Y_Axis, color = ~Model_Compare$Model , type = 'scatter') %>% add_markers(
      text = with(Model_Compare,paste("Brand:", Brand, '<br>', "Model:" ,Model)), hoverinfo = "text") %>%
      layout(title = 'Comparing Models Specification', plot_bgcolor = "#e5ecf6") 
  })
  
  
  
  output$displot1 <- renderPlotly({    
    if(input$m=='a3'){ i<-18 }
    if(input$m=='b3'){ i<-19 }  
    if(input$m=='c3'){ i<-20 } 
    
    Count <- Charge_Density [, i]
    plot_ly (x = Charge_Density$State, y = ~Count ,type ="bar",  text = with(Charge_Density, paste("City:", City)),
             # marker = list(size = 70,color = colorRampPalette(brewer.pal(11,"Spectral"))(5000), 
             marker = list(color = 'rgb(49,130,189)',
              line = list(color = 'black',width = 0.1)))
  })
  
  
  output$displot2 <- renderPlotly({    
    
   # # plot_ly(model_sales.m,x = ~Vehicle, y = ~value, color = ~variable ,type = 'bar') %>% layout(barmode = 'stack')
   #  model_sales <- data.frame(model_sales)
   #  
    reshape2::melt(model_sales, id.vars='Vehicle') %>%
           plot_ly(x = ~Vehicle, y = ~value, type = 'bar', 
            name = ~variable, color = ~variable) %>%
           layout(title ="Year wise model sale" ,yaxis = list(title = 'Count'), barmode = 'stack')
    
  })
  
  output$displot3 <- renderPlotly({ 
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    plot_geo(EV_Sales, locationmode = 'USA-states',source = "plot") %>% add_trace(
      z = ~`Registered EVs`, text = with(EV_Sales, paste("EV Sold",`Registered EVs` , '<br>' , "State:", State)),locations = ~Code,
      color = ~`Registered EVs`,colors = c("#1f77b4", "#ff7f0e","#2ca02c")) %>% colorbar(title = "Ev Sales")    %>% layout(
        title = 'Registered EVs',
        geo = g)
    
  })
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showland = TRUE,
    landcolor = toRGB("grey83"),
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("white"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
  
  output$displot4 <- renderPlotly({
    s <- event_data("plotly_click", source = "plot")
    cd = unique(EV_Sales[EV_Sales$`Registered EVs`== s$z,]$Code)
    filter_data1  = Charge_Density[Charge_Density$State == cd,]
    filter_data1$station_count <- length(filter_data1$`Station Name`)
   
    plot_geo(filter_data1, lat = ~Latitude, lon = ~Longitude) %>% add_markers(
      text = with(filter_data1,paste("State:", State, '<br>', "City:" ,City , '<br>',"Number of stations :",station_count )),
      color = ~State, colorscale = c("#1f77b4", "#ff7f0e","#2ca02c"), hoverinfo = "text") %>% 
      colorbar(title = "Charging Stations") %>% 
      layout(title = 'Density Map of Charging Station <br> as of 2021', 
             geo = g ) 
  
  })
  
  output$displot5 <- renderPlotly({ 
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    plot_geo(EV_Sales, locationmode = 'USA-states',source = "plot1") %>% add_trace(
      z = ~`Registered EVs`, text = with(EV_Sales, paste("EV Sold",`Registered EVs` , '<br>' , "State:", State)),locations = ~Code,
      color = ~`Registered EVs`,colors = c("#1f77b4", "#ff7f0e","#2ca02c")) %>% colorbar(title = "Ev Sales")    %>% layout(
        title = 'Registered EVs',
        geo = g)
    
  })
  
  output$displot6 <- renderPlotly({ 
    s <- event_data("plotly_click", source = "plot1")
    if (length(s)) {
    plot_ly(incentives, x = ~State, y = ~Count_Incentive, type = 'bar')
     
    }
  })
  
  output$displot7 <- renderPlotly({ 
  reshape2::melt(model_by_fuel , id.vars='Fuel_Type') %>%
         plot_ly(x = ~Fuel_Type, y = ~value, type = 'bar', 
         name = ~variable, color = ~variable) %>%
        layout(title = 'Model available year wise for different fuel',yaxis = list(title = 'Count'), barmode = 'stack')
  })
  
  
  url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
  counties <- rjson::fromJSON(file=url)
  ca_ev_registrations = ca_ev_registrations %>%
    group_by(County) %>%
    mutate(unique_types = n_distinct(`Vehicle ID`))
  
  ca_ev_registrations = ca_ev_registrations %>%
    group_by(County) %>%  mutate(ev_model = length(unique(ca_ev_registrations$`Vehicle Name`)))
  
  output$displot9 <- renderPlotly({
    plot_ly(source = "plot2") %>% add_trace(
      type="choroplethmapbox",
      geojson=counties,
      locations=ca_ev_registrations$GEOID,
      z=ca_ev_registrations$unique_types,
      text = c(ca_ev_registrations$County),
     colors = c( "#969696","#17becf", "#393b79","#2ca02c"),
      marker=list(line=list( width=1, opacity=0)) ,
     hovertemplate = paste('<b>EV sold</b>: %{z}<br>',
                           '<b>EV models sold</b>: %{z1}<br>',
                           '<b>%{text}</b>'),
      showlegend = FALSE) %>% layout( title = 'EV Sales in California Counties',
                  mapbox=list(
                    style="stamen-terrain",
                    zoom =4,
                    center=list(lon= -119.417931, lat=36.778259))
    )
  })
  
  output$displot10 <- renderPlotly({
    s <- event_data("plotly_click", source = "plot2")
    print(s)
    ca_ev_registrations <- ca_ev_registrations %>%
      group_by(County) %>%
      mutate(unique_types = n_distinct(`Vehicle ID`))
    cd = ca_ev_registrations[ca_ev_registrations$unique_types == s$z,]
    # filter_data  = ca_ev_registrations[ca_ev_registrations$County == cd,]
    cd <- cd %>% group_by(`Vehicle Name` , County) %>% mutate(Count = n_distinct(`Vehicle ID`))
    cd <- distinct(cd, `Vehicle Name`,Count)
    
    plot_ly(cd ,labels  = ~`Vehicle Name`, values = ~Count, type = 'pie', textinfo = "none") %>%
      layout(title = 'EV Sales by model')
    
  })
    
  output$mytable = renderDataTable({
    as_tibble(dfSummary(Model_Compare))[,c(1,2,3,4,7,8)]
  })
  output$mytable1 = renderDataTable({
    as_tibble(dfSummary(Charge_Density))[,c(1,2,3,4,7,8)]
  })
  output$mytable2 = renderDataTable({
    as_tibble(dfSummary(incentives))[,c(1,2,3,4,7,8)]
  })
  output$mytable3 = renderDataTable({
    as_tibble(dfSummary(model_sales))[,c(1,2,3,4,7,8)]
  })
  output$mytable4 = renderDataTable({
    as_tibble(dfSummary(ELEC_PHEV_1995_2022))[,c(1,2,3,4,7,8)]
  })
  output$mytable5 = renderDataTable({
    as_tibble(dfSummary(model_by_fuel))[,c(1,2,3,4,7,8)]
  })
  output$mytable6 = renderDataTable({
    as_tibble(dfSummary(EV_Sales))[,c(1,2,3,4,7,8)]
  })
  output$mytable7 = renderDataTable({
    as_tibble(dfSummary(ca_ev_registrations))[,c(1,2,3,4,7,8)]
  })
  output$mytable8 = renderPlot({
    res <- cor(Model_Compare[, unlist(lapply(Model_Compare, is.numeric))])
     corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45) 
     })
}

shinyApp(ui, server)
                         