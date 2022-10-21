# importing libraries #
library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(maps)
library(viridis)
library(viridisLite)
library(dplyr)
library(mapproj)
library(devtools)
library(readxl)
require(devtools)
library(wordcloud2)
library(stringr)
library(tidyverse)
library(reactable)
library(htmltools)

# installing packages #
list.of.pkgs <- c("shiny","leaflet","shinydashboard","dplyr","DT","plotly","readxl","zoo","qcc","tidyr","readxlsb","data.table",
                  "reactable",'reshape2','cowplot','forecast',"shinyWidgets","lubridate","packcircles","viridis","ggiraph",
                  "janitor","gganimate","tidyverse","gifski","gapminder","ggplot2","treemap","geosphere","forecast",
                  "fpp2","TTR")
new.packages <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
lapply(new.packages, library, character.only = T)
lapply(list.of.pkgs, require, character.only=T)




#function to give wordcloud2 click interactivity
wc2ClickedWord = function(cloudOutputId, inputId) {
  shiny::tags$script(shiny::HTML(
    sprintf("$(document).on('click', '#%s', function() {", cloudOutputId),
    sprintf('word = document.getElementById("%swcSpan").innerHTML;', cloudOutputId),
    sprintf("Shiny.onInputChange('%s', word);", inputId),
    "});"
  ))
  
}


########################### UI ##############################
ui <- navbarPage(windowTitle ="CSS Dashboard", title = div(img(src = "C_Suite.png", id = "nav",
                                                               height = "50px",width = "240px",
                                                               style = "position: relative; margin: -15px -14px; display:left-align;")
                                                           # " | "
),

tabPanel("Home Template",
         fluidPage(
           div(img(src = "home.png", width = "1250px", height = "640px"),style='margin-left:-25px;margin-right:-35px;display:center-align;')
)
),

navbarMenu('Theme Overview',
tabPanel("Theme Analysis",
         fluidPage(
           fluidRow(
             column(width = 7,
                    h2("Analysis of Topic by Industry/Company", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                    div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
            
             column(width = 5,
                    h2("Source : Earning Transcripts",br(),"Scope : 204 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q3 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
           ),
           
           
           dashboardPage(
             
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE
                            
                              
             ),#dashboardSidebar ends here
             dashboardBody(
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
                 #includeScript("gomap.js")
               ),
               tags$head(
                 tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
               ), # tags$head ends here
               tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}

                                 .box.box-solid.box-primary{background:#222d32}")),
               fluidRow(
                 h2(strong("Detailed View of Key Topics discussed by C-Suite"), align = "center",style = "font-size:18px;",style = "font-family: Arial;",style = "color:black"),
                 
               ),
               fluidRow(
                 div("*All charts act as filter.Click on charts to explore more.",style='margin-top:-10px;', align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:#FC8585"),
               ),
               
               fluidRow(
                 column(width=2),
                 column(width=3,style='padding-top:10px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo5")),
                 ),
                 column(width=3,style='padding-top:10px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt5")),
                 ),
                 column(width=3,style='padding-top:10px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_tf5")),
                 ),
                
                 column(width=1)
               ),
               fluidRow(
                 column(width = 6,
                        h2("Top Themes", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
                 column(width = 6,
                        h2("Emerging Themes", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
                 
                 ),
               fluidRow(
                        column(width = 6,
                               wordcloud2Output("theme_anlysis_top_wc"),
                               wc2ClickedWord("theme_anlysis_top_wc", "theme_analysis_wc_clicked")),
                               column(width = 6,
                                      plotlyOutput("theme_analysis_barplt2"))
                        ),
             
               fluidRow(
                 column(width = 12,
                        h2("Top Keywords", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
                 ),
               fluidRow(
                        column(width = 12,
                               wordcloud2Output("theme_anlysis_wc"),
                               wc2ClickedWord("theme_anlysis_wc", "theme_analysis_wc2_clicked")
                               
                        )),
               fluidRow(
                 column(width = 12,
                        h2("Industry View of Theme", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
               ),
               fluidRow(
                 column(width = 12,
                        plotlyOutput("theme_analysis_barplt"))
               ),
               
               fluidRow(
                 column(width = 12,
                        h2("Company View of Theme", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
               ),
               fluidRow(style='padding-top:10px;',
                        column(width = 12,
                               wordcloud2Output("theme_analysis_cmp_wc")
                               # wc2ClickedWord("comp_wc", "clicked")
                        )),

             )#dashboardbody ends here
           )#dashboardpage ends here
         ) 
),

####################Theme ranking#################
tabPanel("Theme Ranking",
         fluidRow(
           column(width = 7,
                  h2("Theme Ranking", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                  div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
           
           column(width = 5,
                  h2("Source : Earning Transcripts",br(),"Scope : 204 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q4 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
         ),
         
         dashboardPage(
           dashboardHeader(disable = TRUE),
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
             tags$head(
               tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
             ), # tags$head ends here
             tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             fluidRow(
               h2(strong("How Themes's ranks** are changing(QoQ)?"), align = "center",style = "font-size:20px;",style = "font-family: Arial;",style = "color:black")
             ),
             
             fluidRow(
               
               h2(("*Quarters are Accenture Financial Quarter"),style='margin-top:-10px;',align = "center",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585"),
             ),
             fluidRow(
               
               h2(("** Ranking is based on TFIDF scores"),style='margin-top:-10px;',align = "center",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585"),
             ),
             
             fluidRow(
               column(width=2),
               
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo3")),
               ),
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt3")),
               ),
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client3"))
               ),
               column(width=1)
               
             ),
             fluidRow(
               column(width = 10,offset=1,
                      plotlyOutput("theme_ranking_plot", height = "700px"))
             ) 
             
             
             
           )
         )
)


),

tabPanel("Quaterly Analysis",
         fluidPage(
           fluidRow(
             column(width = 7,
                    h2("What's on C-Suite's mind?", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                    div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
             
             column(width = 5,
                    h2("Source : Earning Transcripts",br(),"Scope : 204 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q4 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
           ),
           
           
           dashboardPage(
             
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE,
                              absolutePanel(id="controls",top = 250, left = 270, width = 220,  fixed=FALSE,
                                            draggable = FALSE, height = 445,
                                            
                                            
                                            uiOutput("thmGrwoth")),
                              
                              absolutePanel(id="controls",top = 826, left = 270, width = 220,fixed=FALSE,
                                            draggable = FALSE, height = 445,
                                            uiOutput("KeyImp"),
                                            br(),br(),br(),
                                            uiOutput("keyGrwoth")
                                            
                                            
                              ),
                              
                              absolutePanel(id="controls",top = 1364, left = 270, width = 220,fixed=FALSE,
                                            draggable = FALSE, height = 445,
                                            uiOutput("TopicImp"),
                                            br(),br(),br(),
                                            uiOutput("TopicGrwoth")
                                            
                                            
                              )
                              #sidebarmenu ends here
                              
             ),#dashboardSidebar ends here
             dashboardBody(
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
                 #includeScript("gomap.js")
               ),
               tags$head(
                 tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
               ), # tags$head ends here
               tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}

                                 .box.box-solid.box-primary{background:#222d32}")),
               fluidRow(
                 
                 h2(strong("How C-Suite Signals have evolved?"), align = "center",style = "font-size:18px;",style = "font-family: Arial;",style = "color:black"),
                 
               ),
               fluidRow(
                 div(em("(C-Suite signals are the topics mentioned in the earning reports)"),style='margin-top:-10px;', align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:#FC8585"),
               ),
               fluidRow(
                 column(width=3,style='padding-top:20px;',
                 ),
                 column(width=3,style='padding-top:20px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo")),
                 ),
                 column(width=3,style='padding-top:20px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt")),
                 ),
                 column(width=3,style='padding-top:20px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client"))
                 )
               ) ,
               
               fluidRow(
                 column(width = 6,offset=4,
                        h2(strong("How the themes are evolving (QoQ)?"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                 )),
               fluidRow(
                 column(width = 6,offset=4,
                        div(("*Select a theme to find relevant topics"),style='margin-top:-10px;', align = "center",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585"),
                 )),
               fluidRow(
                 column(width = 10,offset=2,
                        plotlyOutput("plotscatter", height = "400px")),
                 
                 column(width = 6,offset=4,style='padding-top:40px;',
                        h2(strong("Topic Analysis"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                 )),
               fluidRow(
                 column(width = 6,offset=4,style='padding-bottom:20px;',
                        div(("(Detailed View of Themes)"), style='margin-top:-10px;', align = "center",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585"),
                        
                 )),
               fluidRow(
                 column(width = 10,offset=2,style='padding-bottom:20px;',
                        plotlyOutput("plotscatter_keyword", height = "400px")),
               ),
               fluidRow(
                 column(width = 6,offset=4,style='padding-top:40px;',
                        div(("Keyword Analysis"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                        
                 )),
               fluidRow(
                 column(width = 10,offset=2,
                        plotlyOutput("plotscatter_topic", height = "400px")),
               )  #fluidrow ends here
               
             )#dashboardbody ends here
           )#dashboardpage ends here
         ) 
),
# tabpanel Ends here


###################Market Unit view###############
tabPanel("Market Unit View",
         fluidRow(
           column(width = 7,
                  h2("Company Analysis", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                  div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
           #h2("Signals harnessed through Artificial Intelligence & Machine Learning" , align = "left",style = "font-size:12px;",style = "font-family: Arial;",style = "color:black",style='margin-top:-20px;')),
           column(width = 5,
                  h2("Source : Earning Transcripts",br(),"Scope : 204 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q3 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
         ),
         
         dashboardPage(
           dashboardHeader(disable = TRUE),
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
             tags$head(
               tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
             ), # tags$head ends here
             tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
             fluidRow(
               column(width=3),
               column(width=2,style='padding-top:10px;',
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo4")),
               ),
               column(width=2,style='padding-top:10px;',
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt4")),
               ),
               column(width=2,style='padding-top:10px;',
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_tf4")),
               ),
               column(width=3)
               
             ),
             fluidRow(
               
               div(("*All charts act as filter.Click on charts to explore more."), align = "left",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585")),
             
             fluidRow(style='padding-top:30px;',
                      column(12,
                             h2(("Market Analysis"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style='margin-bottom:0px;'))
             ),
             fluidRow(style='padding-top:10px;',
                      column(width = 12,
                             plotlyOutput("comp_map"))
             ),
             fluidRow(style='padding-top:30px;',
                      column(12,
                             h2(("Top Themes by Market Unit"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style='margin-bottom:0px;'))
             ),
             fluidRow(style='padding-top:10px;',
                      column(width = 12,
                             plotlyOutput("comp_donut")
                             
                      )),
             
             fluidRow(style='padding-top:30px;',
                      column(12,
                             h2(("Companies Discussing Selected Theme"), align = "center",style = "font-size:16px;",style = "font-family: Arial;",style='margin-bottom:0px;'))
             ),
             
             fluidRow(style='padding-top:10px;',
                      column(width = 12,
                             wordcloud2Output("comp_wc"),
                             wc2ClickedWord("comp_wc", "clicked")
                             
                      )),
             
             
             fluidRow(style='padding-top:30px;',
                      column(12,
                             h2("Original Text" ,br(),"(from earning report)", align = "center",style = "font-size:14px;",style = "font-family: Arial;",style='margin-bottom:0px;'))
             ),
             
             
             fluidRow(
               box(width = 12,
                      dataTableOutput("comp_Sentences")
                      
               )
             )
             
           )
         )
         
),
#######################Investment##########################
tabPanel("Investments",
         fluidPage(
           
           fluidRow(
             column(width = 7,
                    h2("Overview of Committed Investments", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                    div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
             
             column(width = 5,
                    h2("Source : Earning Transcripts",br(),"Scope : 204 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q3 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
           ),
           dashboardPage(
             
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             dashboardBody(
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
                 #includeScript("gomap.js")
               ),
               tags$head(
                 tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
               ), # tags$head ends here
               tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}

                                 .box.box-solid.box-primary{background:#222d32}")),
               fluidRow(
                 
                 h2(strong("Who is investing When & Where?"), align = "center",style = "font-size:18px;",style = "font-family: Arial;",style = "color:black"),
                 
               ),
               fluidRow(
                 div("* All chart acts as filter. Click on charts to explore more.",style='margin-top:-10px;', align = "center",style = "font-size:14px;",style = "font-family: Arial;",style = "color:#FC8585"),
               ),
               fluidRow(
                 column(width =5),
                 
                 column(width=6,style='padding-top:10px;',
                        div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("timeframe_investment")),
                 )
                 
               ),
               fluidRow(
                 column(width = 12,style='padding-top:10px;',
                        h2("Committed Investments by Themes", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
               ),
               fluidRow(
                 column(width = 12,
                        plotlyOutput("investment_bar", height = "500px")),
                 
                 fluidRow(
                   column(width = 12,style='padding-top:30px;',
                          h2("Investment Overview by Market", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
                 ), 
                 
               ),
               fluidRow(
                 column(width = 12,
                        plotlyOutput("investment_map",height = '500px'))
                 
               ),
               fluidRow(
                 column(width = 12,style='padding-top:30px;',
                        h2("Investment Overview by Companies", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
               ),
               fluidRow(
                 column(width = 12,style = 'height:500px;overflow-y: scroll;5',
                        plotlyOutput("investment_bar2", height = "850px")),
                 
                 
               ),
               fluidRow(
                 column(width = 12,style='padding-top:30px;',
                        h2("Investment Details by Priorities", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
               ),
               fluidRow(
                 box(width = 12,style='padding-top:0px;height:300px;',
                     reactableOutput("investment_Sentences1")
                     
                 )),
               
               fluidRow(
                 column(width = 12,style='margin-top:-25px;',style='padding-top:30px;',
                        h2("Original Text", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
               ),
               fluidRow(
                 column(width = 12,style='margin-top:-25px;',
                        h2("(from Earnings Report)", align = "center",style = "font-size:12px;",style = "font-family: Arial;",style = "color:black"))
               ),
               
               fluidRow(
                 box(width = 12,
                     dataTableOutput("investment_Sentences"), height = "600px")
                 
               )
               
               
               
             )#dashboardbody ends here
           )#dashboardpage ends here
         )#fluidPage ends here  
),# tabpanel Ends here



####################Social Media Theme View#################

navbarMenu("Social Media",

tabPanel("Social Media Theme Analysis",
         fluidRow(
           column(width = 7,
                  h2("Social Media Analysis", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                  div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
           
           column(width = 5,
                  h2("Source :Twitter",br(),"Scope : 146 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q2 2022" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
         ),
         
         dashboardPage(
           dashboardHeader(disable = TRUE),
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
             tags$head(
               tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
             ), # tags$head ends here
             tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
            
             fluidRow(
               column(width=3),

               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo7")),
               ),
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt7")),
               ),
               column(width=3)

             ),
             fluidRow(
               div("*All charts act as filter.Click on charts to explore more.", align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:#FC8585"),
             ),
             
             fluidRow(
               column(width = 6,
                      h2("Theme View", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
               column(width = 6,
                      h2("Market View",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
             ),
             
         
             fluidRow(
               column(width = 6,style='padding-top:10px;',
                      wordcloud2Output("theme_view_wc", height = "350px"),
                      wc2ClickedWord("theme_view_wc", "sm_wc_clicked")),
               column(width = 6,style='padding-top:10px;',
                      plotlyOutput("sm_theme_donut_plt", height = "350px"))
               ),
             fluidRow(
               column(width = 6,
                      h2("Client View", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
               column(width = 6,
                      h2("Topic Analysis",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
             ),
             fluidRow(
               
               column(width = 6,style = 'height:350px;overflow-y: scroll;',style='padding-top:10px;',
                      plotlyOutput("sm_theme_bar_plt", height = "1500px")),
               column(width = 6,style='padding-top:10px;',
                      wordcloud2Output("sm_theme_bar_plt2", height = "350px"))
             ),
             fluidRow(style='padding-top:20px;',
               column(width = 12,
                      h2("Original Tweets", align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:black"))
             ),
             fluidRow(
               box(width = 12,
                 dataTableOutput('sm_datatable'))
             )
             
           )
         )
),

####################Social Media Market View#################
tabPanel("Social Media Market View",
         fluidRow(
           column(width = 7,
                  h2("Social Media Analysis", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                  div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
           
           column(width = 5,
                  h2("Source :Twitter",br(),"Scope : 146 Growth Market G2000 companies (excluding G.China) ",br(),"Time period :Q2 2022" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
         ),
         
         dashboardPage(
           dashboardHeader(disable = TRUE),
           dashboardSidebar(disable = TRUE),
           
           dashboardBody(
             tags$head(
               tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
             ), # tags$head ends here
             tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
          
             fluidRow(
               column(width=3),
               
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo8")),
               ),
               column(width=3,
                      div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt8")),
               ),
               column(width=3)
               
             ),
             fluidRow(
               div("*All charts act as filter.Click on charts to explore more.", align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:#FC8585"),
             ),
             fluidRow(
               column(width = 12,
                      h2("Market Unit Analysis", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
               ),
           
             fluidRow(
               column(width = 12,
                      plotlyOutput("td_map", height = "450px"))
             ),
             fluidRow(
               column(width = 6,
                      h2("Theme View", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
               column(width = 6,
                      h2("Client View",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
             ),
             fluidRow(
               column(width = 6,style='padding-top:10px;',
                      wordcloud2Output("theme_view_wc_2", height = "450px"),
                      wc2ClickedWord("theme_view_wc_2", "sm_mkt_wc_clicked")),
               
               column(width = 6,style='padding-top:10px;',style = 'height:450px;overflow-y: scroll;',
                      plotlyOutput("sm_market_bar_plt", height = "1500px"))
               ),
             fluidRow(
               column(width = 12,
                      h2("Topic Analysis", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
             ),
             fluidRow(
               column(width = 12,style='padding-top:10px;',
                      wordcloud2Output("sm_mrt_wc", height = "450px"))
             ),
             fluidRow(
               column(width = 12,
                      h2("Original Tweets", align = "center",style = "font-size:15px;",style = "font-family: Arial;",style = "color:black"))
             ),
             fluidRow(
               box(width = 12,style='padding-top:10px;',
                      dataTableOutput('sm_market_datatable'))
             )
             
           )
         )
)
),
############Compare 
navbarMenu("Compare",
           tabPanel('Earning Transcript',
                    fluidRow(
                      column(width = 7,
                             h2("Compare Signals", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
                             div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
                      #h2("Signals harnessed through Artificial Intelligence & Machine Learning" , align = "left",style = "font-size:12px;",style = "font-family: Arial;",style = "color:black",style='margin-top:-20px;')),
                      column(width = 5,
                             h2("Source : Earning Transcripts",br(),"Scope : Growth Markets (G2K companies)",br(),"Time period : Q4 FY21 to Q1 FY22" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
                    ),
                    
                    dashboardPage(
                      dashboardHeader(disable = TRUE),
                      dashboardSidebar(disable = TRUE),
                      
                      dashboardBody(
                        tags$head(
                          tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
                        ), # tags$head ends here
                        tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
                        
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        fluidRow(
                          h2(strong("Compare signals among Companies, Market Units & Industries"), align = "center",style = "font-size:20px;",style = "font-family: Arial;",style = "color:black")
                        ),
                        fluidRow(
                          
                          column(width=1,style='padding-top:10px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client9")),
                          ),
                          column(width=1,style='padding-top:10px;',style='margin-left:90px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt9")),
                          ),
                          column(width=1,style='padding-top:10px;',style='margin-left:90px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo9")),
                          ),
                                 
                          column(width=1,style='padding-top:10px;',style='margin-left:140px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client10")),
                          ),
                          column(width=1,style='padding-top:10px;',style='margin-left:90px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt10")),
                          ),
                          column(width=1,style='padding-top:10px;',style='margin-left:90px;',
                                 div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo10")),
                          )
                          
                        ),
                        fluidRow(
                          column(width = 6,
                                 h2("Top Themes", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
                          column(width = 6,
                                 h2("Top Themes",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
                        ),
                        fluidRow(
                          column(width = 6,style='padding-top:10px;',
                                 wordcloud2Output("cmp_et_wc1", height = "450px"),
                                wc2ClickedWord("cmp_et_wc1", "trnspt_wc_clicked")),
                          
                          column(width = 6,style='padding-top:10px;',
                                 wordcloud2Output("cmp_et_wc3", height = "450px"),
                                 wc2ClickedWord("cmp_et_wc3", "trnspt_wc_clicked2"))
                        ),
                        fluidRow(
                          column(width = 6,
                                 h2("Top Keywords", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
                          column(width = 6,
                                 h2("Top Keywords",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
                        ),
                        fluidRow(
                          column(width = 6,style='padding-top:10px;',
                                 wordcloud2Output("cmp_et_wc2", height = "450px")),
                                 
                          
                          column(width = 6,style='padding-top:10px;',
                                 wordcloud2Output("cmp_et_wc4", height = "450px"))
                        )
                      )
                        
                      )
                    ),
                    
        
tabPanel('Social Media',
   fluidRow(
  column(width = 7,
         h2("Compare Signals", align = "left",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"),
         div(("Signals harnessed through Artificial Intelligence & Machine Learning"), align = "left",style='margin-top:-10px;',style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A")),
  #h2("Signals harnessed through Artificial Intelligence & Machine Learning" , align = "left",style = "font-size:12px;",style = "font-family: Arial;",style = "color:black",style='margin-top:-20px;')),
  column(width = 5,
         h2("Source : Earning Transcripts",br(),"Scope : 146 Growth Market G2000 companies (excluding G.China) ",br(),"Time period : Q2 2022" , align = "right",style = "font-size:12px;",style = "font-family: Arial;",style = "color:#7A7A7A"))
),

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(".main-sidebar { font-size: 15px; }.sidebar { font-size: 9px;}"))),
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
                                 background-color: #ECF1FD;
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
    ), # tags$head ends here
    tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {}
                                 .box.box-solid.box-primary{background:#222d32}")),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    fluidRow(
      h2(strong("Compare signals among Companies, Market Units & Industries"), align = "center",style = "font-size:20px;",style = "font-family: Arial;",style = "color:black")
    ),
    fluidRow(
      
      column(width=1,style='padding-top:10px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client11")),
      ),
      column(width=1,style='padding-top:10px;',style='margin-left:90px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt11")),
      ),
      column(width=1,style='padding-top:10px;',style='margin-left:90px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo11")),
      ),
      column(width=1,style='padding-top:10px;',style='margin-left:140px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_client12")),
      ),
      column(width=1,style='padding-top:10px;',style='margin-left:90px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_mkt12")),
      ),
      column(width=1,style='padding-top:10px;',style='margin-left:90px;',
             div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput("var_geo12")),
      )
      
    ),
    fluidRow(
      column(width = 6,
             h2("Top Themes", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
      column(width = 6,
             h2("Top Themes",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
    ),
    fluidRow(
      column(width = 6,style='padding-top:10px;',
             wordcloud2Output("cmp_wc1", height = "450px"),
             wc2ClickedWord("cmp_wc1", "sm_wc_clic")),
      
      column(width = 6,style='padding-top:10px;',
             wordcloud2Output("cmp_wc3", height = "450px"),
             wc2ClickedWord("cmp_wc3", "sm_wc_clicked1"))
      
    ),
    fluidRow(
      column(width = 6,
             h2("Top Keywords", align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black")),
      column(width = 6,
             h2("Top Keywords",align = "center",style = "font-size:16px;",style = "font-family: Arial;",style = "color:black"))
    ),
    fluidRow(
      column(width = 6,style='padding-top:10px;',
             wordcloud2Output("cmp_wc2", height = "450px")),
      column(width = 6,style='padding-top:10px;',
             wordcloud2Output("cmp_wc4", height = "450px"))
    )
  )
  
)
)
),

navbarMenu('Data Detail',
           tabPanel('Transcript Detail',
                    fluidRow(
             column(width = 12,
                    plotlyOutput("trnscpt_hm", height = "450px"))
           )),
           tabPanel('Twitter Detail',
                    fluidRow(
                      column(width = 12,
                             plotlyOutput("sm_hm", height = "450px"))
                    ))
          
  
)
)

########################## SERVER ##########################

# setting directory #
getwd()
setwd("")


# Reading the data
data <- read_excel("CSS_DM_AllClients_AF_14Feb_v3.xlsx")
names(data) <- gsub(" ", ".", names(data))
data["Main.Theme"][data["Main.Theme"] == "M&A"] <- "M & A"
data <- data %>%
  mutate(Main.Theme = str_replace(Main.Theme, "&", "and"))
data['Primary.Market.Unit'] <- lapply(data['Primary.Market.Unit'], toupper)
data <- data[data$Main.Theme != "ROI", ]

tfidf_data <- data
sentiment_data <- data

investments <- read_excel("Css_investment_TM_2Feb22_v1.xlsx")
names(investments) <- gsub(" ", ".", names(investments))

twitter_data <- read_excel("CSS_Twitter_DM_NwMapng_AF_1Feb22_v2.xlsx")
names(twitter_data) <- gsub(" ", ".", names(twitter_data))
twitter_data["Main.Theme"][twitter_data["Main.Theme"] == "M&A"] <- "M & A"
twitter_data <- twitter_data %>%
  mutate(Main.Theme = str_replace(Main.Theme, "&", "and"))
twitter_data['Primary.Market.Unit'] <- lapply(twitter_data['Primary.Market.Unit'], toupper)
twitter_data <- twitter_data[twitter_data$Main.Theme != "ROI", ]

#font variable
# t1 -> title, t2-> subtitle , t3-> labels and legends
t1 <- list(
  family = "Arial",
  size = 14,
  color = 'black')

t2 <- list(
  family = "Graphik",
  size = 9,
  color = 'black')

t3 <- list(
  family = "Helvetica",
  size = 11,
  color = 'black')


# Data Preparation

# select latest 2 quarter data
latest_2_qrtr <- filter(data, Timeframe %in% c("Q4-2021","Q1-2022"))
latest_2_qrtr_FC<-dcast(latest_2_qrtr, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
latest_qtr<- filter(latest_2_qrtr, Timeframe == "Q1-2022")
latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                     by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
# quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
quarter_df1[is.na(quarter_df1)] <- 0

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



# Start of server code
server <- function(input,output){
  
  
  ####################Theme analysis 
  
  ######Reactive
  
  # Side bar filter for market unit #
  geo_list <- reactive({
    sort(unique(data$Primary.Market.Unit))
  }) 
  
  output$var_geo5 <- renderUI({
    pickerInput("variable_geo5",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
                options = list(`actions-box` = TRUE),multiple = T,
                selected=geo_list())
  })
  
  # Side bar filter for industry #
  output$var_mkt5 <- renderUI({
    data_geo <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    mkt_list <- reactive({
      sort(unique(data_geo$Primary.Industry))
    })
    pickerInput("variable_mkt5",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
                options = list(`actions-box` = TRUE),multiple = T,
                selected=mkt_list())
  })
  
  # Side bar filter for timeframe #
  
  
  output$var_tf5 <- renderUI({
    data_geo <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data_time <- filter(data_geo, Primary.Industry %in% input$variable_mkt5)
    mkt_list <- reactive({
      sort(unique(data_time$Timeframe))
    })
    pickerInput("variable_tf5",label = div(style = "font-size:11px;font-family:Helvetica;", "Timeframe") , choices=c('Q3-2021','Q4-2021','Q1-2022'),
                options = list(`actions-box` = TRUE),multiple = T,
                selected='Q1-2022')
  })
  
  output$topkey5 <- renderUI({
    sliderInput("topkey5",label = div(style = "font-size:11px;font-family:Helvetica;", "Top Keyword Count"),ticks = FALSE,
                min =  5, max = 20,
                value = 20
    )
  })
  
  
  ##top themes wc
  top_theme_wc <- reactive ({
   
    data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data <- filter(data, Primary.Industry %in% input$variable_mkt5)
    data <- filter(data, Timeframe %in% input$variable_tf5)
    
    
    
  latest_2_qrtr_FC<-dcast(data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr <- subset(data, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
    summarise(score = sum(Tfidf.Score),
              .groups = 'drop')
  
  ####Min max scaling
  
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  return(scatterplot_df_fltrd)
  
})

output$theme_anlysis_top_wc <- renderWordcloud2({
  
  custColorPal <- colorRampPalette(c("#1737CD","#B44AD5"))
  custColors <- custColorPal(nrow(top_theme_wc()))
  
  wordcloud2(data = top_theme_wc(), size = 0.4, gridSize = 20, shape = 'rectangle',rotateRatio = 0, color=custColors)
  
})
  
  
  
  ###########Emerging themes barplot
  

  
  # reactive function for barplot1
  t1_barplot1 <- reactive({
    
      quarter_df1 <- filter(quarter_df1, Primary.Market.Unit %in% input$variable_geo5)
      quarter_df1 <- filter(quarter_df1, Primary.Industry %in% input$variable_mkt5)
      
      

    

    
      data_t1_p2 = quarter_df1 %>% group_by(Main.Theme) %>%
      summarise(Q1 = sum(`Q1-2022`),
                Q4 = sum(`Q4-2021`),
                .groups = 'drop')
    data_t1_p2['Growth_Index'] <- ((data_t1_p2$Q1 - data_t1_p2$Q4)/data_t1_p2$Q4)
    data_t1_p2['Growth_Index'] <- round(data_t1_p2['Growth_Index'], digits = 2)
    data_t1_p2 <- data_t1_p2[order(data_t1_p2$Growth_Index),]
    #data_t1_p2 <- data_t1_p2[1:input$topkey5,]
    return(data_t1_p2)
  })
  
  # plot2 : Barplot (pos & new)
  output$theme_analysis_barplt2 <- renderPlotly({
    mrg1 <- list(l = 50, r = 50,
                 b = 50, t = 50,
                 pad = 20)
    plot_ly(t1_barplot1(), x = ~Growth_Index, y = ~Main.Theme, type = 'bar', orientation = 'h', color = ~Growth_Index < 0,source = "bar1",
            colors = c("#A301FF", "#950706"), name = ~ifelse(Growth_Index < 0, "< 0", ">= 0"),
            hoverinfo = "text",text = t1_barplot1()$Growth_Index, textposition = "outside",textfont = list(size = 8,color = "black"),
            hovertext = paste("Theme :", t1_barplot1()$Main.Theme,
                              "<br>Themes Freq Change QoQ (%) :", t1_barplot1()$Growth_Index),
            #textposition = 'top right',
            textfont = list(size = 10,color = colors)) %>%
      layout(margin = mrg1) %>%
      layout(xaxis = list(title = "Themes Frequency Change QoQ (%)",dticks = 10,showticklabels = FALSE,showgrid = FALSE,xticks = FALSE,range=c(-0.6,0.6),zeroline = FALSE)) %>%
      layout(yaxis = list(title = "",categoryorder = "array", categoryarray = ~Growth_Index,yticks = FALSE ,zeroline = FALSE)) %>% config(displayModeBar = F)
  })
  
 
  
  
  
############# Theme ananlysis wc
  ## main theme  wc clicked
  
  theme_anly_wc_clicked <- reactive ({
    var1 <- strsplit(input$theme_analysis_wc_clicked, ":")[[1]][1]
    return(var1)
  })
  
  ##Barplot clicked
  
  bar_click <- reactive({
    var <- (event_data("plotly_click", source = "bar1"))$y
    return(var)
  })
  
  
  
  
  
  # observe event reactive function for bubble chart
  values <- reactiveValues(go = NULL, do = NULL, act = NULL )
  
  # wordcloud
  observeEvent(input$theme_analysis_wc_clicked, {
    values$go <- theme_anly_wc_clicked()
    values$do <- NULL
    values$act <- NULL})
  
  # bar chart1
  observe({
    values$go <- NULL
    values$do  <- bar_click()
    values$act <- NULL})
  
  
theme_analysis_wc <- reactive({
  
  theme<- c(values$go ,values$do,values$act)
  print(theme)
  
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  
  if(is.null(theme)){
    data <- data
  }
  else{
    
    data <- filter(data,Main.Theme==theme)
  }
  
  latest_2_qrtr_FC<-dcast(data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr <- subset(data, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Keywords_DM)  %>%
    summarise(score = sum(Tfidf.Score),
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
 
  return(scatterplot_df_fltrd)
  
})
  
output$theme_anlysis_wc <- renderWordcloud2({
  
  custColorPal <- colorRampPalette(c("#B44AD5","#1737CD","#2447EE"))
  
  custColors <- custColorPal(nrow(theme_analysis_wc()))
  
  if(length(theme_analysis_wc()$Keywords_DM) == 1){
    data = theme_analysis_wc() %>% add_row(Keywords_DM = "", score = 0) %>% data.frame()
    data =  data[complete.cases(data),]
    print('aaa')
    print( data)
  }
  else{
    data =  theme_analysis_wc()[complete.cases(theme_analysis_wc()),]
  }
  data = data %>% arrange(desc(score))
  
  wordcloud2(data = data, size = 0.4, gridSize = 15, shape = 'rectangle',rotateRatio = 0, color=custColors) 
 
})
  

##bar plot

###wc2 clicked
theme_anly_wc2_clicked <- reactive ({
  var1 <- strsplit(input$theme_analysis_wc2_clicked, ":")[[1]][1]
  return(var1)
})




values1 <- reactiveValues(go = NULL, do = NULL, act = NULL )

# wordcloud

observeEvent(input$theme_analysis_wc_clicked, {
  values1$go <- theme_anly_wc_clicked()
  values1$do <- NULL
  values1$act <- NULL})

# barchart1
observe({
  values1$go <- NULL
  values1$do  <- bar_click()
  values1$act <- NULL})


theme_analysis_bar <- reactive({

  
  theme<- c(values1$go ,values1$do,values1$act)
  
  if(is.null(theme) & is.null(input$theme_analysis_wc2_clicked)){
    
    data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data <- filter(data, Primary.Industry %in% input$variable_mkt5)
    data <- filter(data, Timeframe %in% input$variable_tf5)
    
    
  }
  else if(!is.null(theme) & is.null(input$theme_analysis_wc2_clicked)){
    data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data <- filter(data, Primary.Industry %in% input$variable_mkt5)
    data <- filter(data, Timeframe %in% input$variable_tf5)
    
    data <- filter(data,Main.Theme==theme)
    
  }
  else if(is.null(theme) & !is.null(input$theme_analysis_wc2_clicked)){
    data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data <- filter(data, Primary.Industry %in% input$variable_mkt5)
    data <- filter(data, Timeframe %in% input$variable_tf5)
    
    data <- filter(data, data$Keywords_DM %in% theme_anly_wc2_clicked())
    
  }
  
  else{
    data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
    data <- filter(data, Primary.Industry %in% input$variable_mkt5)
    data <- filter(data, Timeframe %in% input$variable_tf5)
    
    data <- filter(data,Main.Theme==theme)
    
    data <- filter(data, data$Keywords_DM %in% theme_anly_wc2_clicked())
    
  }
  
  barplot_df = data %>% group_by(Primary.Industry)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
  barplot_df <- subset(barplot_df, select = c('Primary.Industry','Total'))
  
  barplot_df$Primary.Industry <- factor(barplot_df$Primary.Industry, levels = unique(barplot_df$Primary.Industry)[order(barplot_df$Total, decreasing = FALSE)])  
  return(barplot_df)
  
})


output$theme_analysis_barplt <- renderPlotly({
  
  mrg1 <- list(l = 50, r = 50,
               b = 50, t = 50,
               pad = 20)
  
  plot_ly(data = theme_analysis_bar(),x = ~Total , y = ~Primary.Industry, type = 'bar', bargap = 2,orientation = 'h',source = "bar2",
          hoverinfo = "text",text = theme_analysis_bar()$Total, textposition = "outside",textfont = list(size = 10,color = "black"),
          hovertext = paste("Theme :", theme_analysis_bar()$Primary.Industry,
                            "<br>Investment :", theme_analysis_bar()$Total),
          marker = list(color='#C48BFF'))%>%
    layout(titlefont = list(size = 14),margin = mrg1,xaxis = list(title = "Theme Frequency",titlefont = list(size = 14),showgrid = FALSE,showticklabels=TRUE),
           yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                        title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')))%>% 
    config(displayModeBar = F)
  
})
  

###company view wc

##bar click
bar_click2 <- reactive({
  var <- (event_data("plotly_click", source = "bar2"))$y
  return(var)
})


values2 <- reactiveValues(go = NULL, do = NULL, act = NULL )

# wordcloud

observeEvent(input$theme_analysis_wc_clicked, {
  values2$go <- theme_anly_wc_clicked()
  values2$do <- NULL
  values2$act <- NULL})

# barchart1
observe({
  values2$go <- NULL
  values2$do  <- bar_click()
  values2$act <- NULL})

theme_anlysis_cmp_wc <- reactive({
  
theme<- c(values2$go ,values2$do,values2$act)


if(is.null(theme) & is.null(input$theme_analysis_wc2_clicked) & is.null(bar_click2())){
  print('in if block')
  
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
}

else if(!is.null(theme) & is.null(input$theme_analysis_wc2_clicked)  & is.null(bar_click2())){

  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data,Main.Theme==theme)
  
}
else if(is.null(theme) & !is.null(input$theme_analysis_wc2_clicked)  & is.null(bar_click2())){
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data, Keywords_DM %in% theme_anly_wc2_clicked())
  
}
else if(is.null(theme) & is.null(input$theme_analysis_wc2_clicked)  & !is.null(bar_click2())){
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data, Primary.Industry %in% bar_click2())
  
}

else if(!is.null(theme) & !is.null(input$theme_analysis_wc2_clicked)  & is.null(bar_click2())){
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data,Main.Theme==theme)
  data <- filter(data, Keywords_DM %in% theme_anly_wc2_clicked())
  
}
else if(is.null(theme) & !is.null(input$theme_analysis_wc2_clicked)  & !is.null(bar_click2)){
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data, Keywords_DM %in% theme_anly_wc2_clicked())
  data <- filter(data, Primary.Industry %in% bar_click2())
  
}
else if(!is.null(theme) & is.null(input$theme_analysis_wc2_clicked)  & !is.null(bar_click2)){
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data,Main.Theme==theme)
  
  data <- filter(data, Primary.Industry %in% bar_click2())
  
}

else{
  data <- filter(data, Primary.Market.Unit %in% input$variable_geo5)
  data <- filter(data, Primary.Industry %in% input$variable_mkt5)
  data <- filter(data, Timeframe %in% input$variable_tf5)
  
  data <- filter(data,Main.Theme==theme)
  data <- filter(data, Keywords_DM %in% theme_anly_wc2_clicked())
  data <- filter(data, Primary.Industry %in% bar_click2())
  
}


data1 <- data %>% group_by(Client.Name)  %>%
  summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
return(data1)

})

output$theme_analysis_cmp_wc <- renderWordcloud2({
  
  custColorPal <- colorRampPalette(c("#C559E5","#1737CD","#B44AD5"))
  custColors <- custColorPal(nrow(theme_anlysis_cmp_wc()))
  
  if(length(theme_anlysis_cmp_wc()$Client.Name) == 1){
    data = theme_anlysis_cmp_wc() %>% add_row(Client.Name = "", Total = 0) %>% data.frame()
    data =  data[complete.cases(data),]
    
  }
  else{
    data =  theme_anlysis_cmp_wc()[complete.cases(theme_anlysis_cmp_wc()),]
  }
  
  data = data %>% arrange(desc(Total))
  

  wordcloud2(data = data, size = 0.2, gridSize = 5, shape = 'rectangle',rotateRatio = 0, color=custColors) 
  

})






    ########### Quarterly Analysis #########
  
    # Side bar filter for geography #
    geo_list <- reactive({
      sort(unique(data$Primary.Market.Unit))
    }) 
    
    output$var_geo <- renderUI({
      pickerInput("variable_geo",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=geo_list())
    })
    
    # Side bar filter for domain #
    mkt_list <- reactive({
      sort(unique(data$Primary.Industry))
    })
    
    output$var_mkt <- renderUI({
      data_geo <- filter(data, Primary.Market.Unit %in% input$variable_geo)
      mkt_list <- reactive({
        sort(unique(data_geo$Primary.Industry))
      })
      pickerInput("variable_mkt",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=mkt_list())
    })
    
    ##Missing values
    
    output$text <- renderText({sum(is.na(quarter_df1))
      
    })
    
    # Side bar filter for client #
    output$var_client <- renderUI({
      data_client <- filter(data, Primary.Industry %in% input$variable_mkt)
      data_client <- filter(data_client, Primary.Market.Unit %in% input$variable_geo)
      client_list <- reactive({
        sort(unique(data_client$Client.Name))
      })
      pickerInput("variable_client",label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=client_list())
    })
    
    # Side bar Slider for keyword Importance
    slider_list <- reactive({
      as.character(c(unique(data$Tfidf.Score)))
      
    })
    
    output$thmImp <- renderUI({
      
      # data_client1 <- filter(data_client1, Client.Name %in% input$variable_client)
      
      sliderInput("thm_imp",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Theme Score"),ticks = FALSE,
                  # min =  0, max = sum(quarter_df['Tfidf_sum']),
                  min =  0, max = 1,
                  value = c(0,1)
      )
    })
    
    # QoQ Slider
    output$thmGrwoth <- renderUI({
      sliderInput("thm_Growth",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Themes Frequency Change QoQ (%)") ,ticks = FALSE,
                  min =-100, max = 100,
                  value = c(-100,100)
      )
    })
    

    output$KeyImp <- renderUI({
      
      # data_client1 <- filter(data_client1, Client.Name %in% input$variable_client)
      
      sliderInput("Key_imp",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Topic Score"),ticks = FALSE,
                  # min =  0, max = sum(quarter_df['Tfidf_sum']),
                  min =  0, max = 1,
                  value = c(0,1)
      )
    })
    
    # QoQ Slider
    output$keyGrwoth <- renderUI({
      sliderInput("Key_Growth",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Topic Frequency Change QoQ (%)") ,ticks = FALSE,
                  min =-100, max = 500,
                  value = c(-100,500)
      )
      
    })
    output$TopicImp <- renderUI({
      
      # data_client1 <- filter(data_client1, Client.Name %in% input$variable_client)
      
      sliderInput("Topic_imp",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Keyword Score"),ticks = FALSE,
                  # min =  0, max = sum(quarter_df['Tfidf_sum']),
                  min =  0, max = 1,
                  value = c(0,1)
      )
    })
    
    # QoQ Slider
    output$TopicGrwoth <- renderUI({
      sliderInput("Topic_Growth",label = div(style = "font-size:11px;font-family:Helvetica;",style = "color:black", "Keyword Frequency Change QoQ (%)") ,ticks = FALSE,
                  min =-100, max = 500,
                  value = c(-100,500)
      )
    })
    ###reactive function for scatterplot_df_fltrd 
    
    dataInput <- reactive({
      scatterplot_df <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
      scatterplot_df <- filter(scatterplot_df, scatterplot_df$Client.Name %in%  input$variable_client)
      
      scatterplot_df_fltrd = scatterplot_df %>% group_by(Main.Theme)  %>%
        summarise(Q1 = sum(`Q1-2022`),
                  Q4 = sum(`Q4-2021`),
                  score = sum(Tfidf.Score),
                  
                  .groups = 'drop')
      
      ####Min max scaling
      scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
      
      
      
      
      scatterplot_df_fltrd['Growth_Index'] <- ((scatterplot_df_fltrd$Q1 - scatterplot_df_fltrd$Q4)/scatterplot_df_fltrd$Q4)
      scatterplot_df_fltrd['Growth_Index'] <- round(scatterplot_df_fltrd['Growth_Index'], digits = 2)
      #  scatterplot_df_fltrd['Growth_Index'] <- (scatterplot_df_fltrd['Growth_Index'] - min(scatterplot_df_fltrd['Growth_Index']))/(max(scatterplot_df_fltrd['Growth_Index']) - min(scatterplot_df_fltrd['Growth_Index']))*100
      
      scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
      
      
      #scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$score >= input$thm_imp[1] & scatterplot_df_fltrd$score <= input$thm_imp[2])
      
      scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$Growth_Index >= input$thm_Growth[1] & scatterplot_df_fltrd$Growth_Index <= input$thm_Growth[2])
      
      scatterplot_df_fltrd$Capital_txt = paste(
        'Theme:', scatterplot_df_fltrd$Main.Theme,
        '<br>Importance:',scatterplot_df_fltrd$score,
        '<br>Themes Frequency QoQ Change(%):',(scatterplot_df_fltrd$Growth_Index)*100,'%')
      
      return(scatterplot_df_fltrd)
      
    })
    
    
    # Scatter plot at theme level
    
    output$plotscatter <- renderPlotly({
      
    
      data <- dataInput() %>% arrange((Growth_Index))
      
      pal <- colorFactor(c("#1737CD","#2447EE","#B44AD5"), 1:length(data$Growth_Index) )#, domain = scatterplot_df_fltrd$Opportunity.Growth_Index)
      colors <- pal(c(1:length(data$Growth_Index)))
      
      plot_ly(data = data, x = ~Growth_Index, y = ~score,color = ~Growth_Index, colors = colors,
              type = 'scatter', mode = 'markers',
              # text = ~Capital_txt,
              text = ~Main.Theme,
              sizes = c(10, 50),
              size = ~score,
              marker = list(color = colors,opacity = 3,sizemode = 'diameter' , line = list(width = 0, color = '#FFFFFF')),
              #color = ~categories,
              hovertext = ~Capital_txt,
              textposition = 'top right',
              textfont = list(size = 8,color = colors),
              hoverinfo="text",source = "sunSource") %>% colorbar(title = "Themes Frequency<br>Change QoQ (%)",orientation = 'h',len=0.85,thickness=15) %>%
        
        layout( xaxis = list(titlefont = t3, tickfont = list(size = 8),tickformat = "0.0%",title = 'Themes Frequency QoQ Change(%)', zerolinecolor = '#d9d9d9'), 
                yaxis = list(titlefont = t3,
                             title = "Importance",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE
                ) ) %>% 
        config(displayModeBar = F) %>%
        layout(
          annotations = 
            list(
              list(
                # Localization of annotation : adjustment of x & y
                x = -0.05, 
                y = 1, 
                text = paste0("<b>",'High',"</b>"),
                showarrow = F, 
                xref='paper', 
                yref='paper'),
              list(
                x = -0.05, 
                y = 0, 
                text = paste0("<b>",'Low',"</b>"), 
                showarrow = F, 
                xref='paper', 
                yref='paper')
            )
        ) 
      
      
    }) #renderplotly ends here
    
    
    #Scatter plot at topic level
    
    topic_click <- reactive ({
      event <- event_data("plotly_click", source = "sunSource")
      return(event)
    })
    
    dataInput_topic <- reactive({
      
 
      
      if (is.null(topic_click())){
        
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
      }
      
      else {
        main_theme = dataInput()$Main.Theme[dataInput()$Growth_Index == topic_click()$x & dataInput()$score == topic_click()$y]
        
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Main.Theme %in%  main_theme)
        
      }
      scatterplot_df_fltrd = scatterplot_df_key %>% group_by(Keywords)  %>%
        summarise(Q1 = sum(`Q1-2022`),
                  Q4 = sum(`Q4-2021`),
                  score = sum(Tfidf.Score),
                  .groups = 'drop')
      
      ####Min max scaling
      scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
      
      
      scatterplot_df_fltrd['Growth_Index'] <- ((scatterplot_df_fltrd$Q1 - scatterplot_df_fltrd$Q4)/scatterplot_df_fltrd$Q4)
      scatterplot_df_fltrd['Growth_Index'] <- round(scatterplot_df_fltrd['Growth_Index'], digits = 2)
      scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
      
      
      scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$score >= input$Key_imp[1] & scatterplot_df_fltrd$score <= input$Key_imp[2])
      
      scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$Growth_Index >= input$Key_Growth[1] & scatterplot_df_fltrd$Growth_Index <= input$Key_Growth[2])
      
      scatterplot_df_fltrd$Capital_txt = paste(
        'Topic:', scatterplot_df_fltrd$Keywords,
        '<br>Importance:',scatterplot_df_fltrd$score,
        '<br>Topic Frequency QoQ Change(%):',(scatterplot_df_fltrd$Growth_Index)*100,'%')
      
      return(scatterplot_df_fltrd)
      
      
    })
    
    
    
    output$plotscatter_keyword <- renderPlotly({
      
      data <- dataInput_topic() %>% arrange((Growth_Index))
      
    
      pal <- colorFactor(c("#BA0606","#FB8704","#F3F60B","#04471D"), 1:length(data$Growth_Index)) #, domain = df1$Opportunity.Value
      colors = pal(c(1:length(data$Growth_Index)))
      #scatterplot_df_fltrd$score1 <- scatterplot_df_fltrd$score*4
      plot_ly(data = data, x = ~Growth_Index, y = ~score,color = ~Growth_Index, colors = colors,
              type = 'scatter', mode = 'markers',
              # text = ~Capital_txt,
              text = ~Keywords,
              sizes = c(10, 50),
              size = ~score,
              marker = list(color = colors,opacity = 3, sizemode = 'diameter', line = list(width = 0, color = '#FFFFFF')),
              #color = ~categories,
              hovertext = ~Capital_txt,
              textposition = 'top right',
              textfont = list(size = 8,color = colors),
              hoverinfo="text",
              source = "sunSource1")  %>% colorbar(title = "Topic Frequency<br>Change QoQ (%)")  %>%
        # layout(plot_bgcolor='rgb(242, 242, 242)') %>% 
        # layout(paper_bgcolor='rgb(242, 242, 242)') %>% 
        #color = ~Keywords,colors  =c("#5000CC","#DD0064","#0045C8","#4C0087","#FF25AC","#B40572","#E0B0FF","#770737","#CCCCFF","#953553","#7F00FF","#CF9FFF"), marker = list(size = 14)) %>%
        layout( xaxis = list(titlefont = t3, tickfont = list(size = 8),tickformat = "0.0%",title = 'Topic Frequency QoQ Change(%)', zerolinecolor = '#d9d9d9'),
                yaxis = list(titlefont = t3,
                             title = "Importance",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE ) ) %>%      
        config(displayModeBar = F)%>%
        layout(
          annotations = 
            list(
              list(
                # Localization of annotation : adjustment of x & y
                x = -0.06, 
                y = 1, 
                text = paste0("<b>",'High',"</b>"),
                showarrow = F, 
                xref='paper', 
                yref='paper'),
              list(
                x = -0.06, 
                y = 0, 
                text = paste0("<b>",'Low',"</b>"), 
                showarrow = F, 
                xref='paper', 
                yref='paper')
            )
        )
      
    })
    
    #Scatter plot at keyword level
    
    keyword_click  <- reactive ({
      event <- event_data("plotly_click", source = "sunSource1")
      return(event)
      
    })
    dataInput_key <- reactive({
      

      

      if (is.null(keyword_click()) & is.null(topic_click())){
        # main_theme = dataInput()$Main.Theme[dataInput()$Growth_Index == event$x & dataInput()$score == event$y]
        
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
        # scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Main.Theme %in%  main_theme)
        
        }
      
      else if(is.null(keyword_click()) & !is.null(topic_click())){
        main_theme = dataInput()$Main.Theme[dataInput()$Growth_Index == topic_click()$x & dataInput()$score == topic_click()$y]
        
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Main.Theme %in%  main_theme)
        

      }
      
      else if (!is.null(keyword_click()) & is.null(topic_click())){

        main_theme = dataInput_topic()$Keywords[dataInput_topic()$Growth_Index == keyword_click()$x & dataInput_topic()$score == keyword_click()$y]
        
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Keywords %in%  main_theme)

      }
      
      else{
        scatterplot_df_key <- filter(quarter_df1, quarter_df1$Primary.Industry %in%  input$variable_mkt)
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Client.Name %in%  input$variable_client)
        
        main_theme = dataInput()$Main.Theme[dataInput()$Growth_Index == topic_click()$x & dataInput()$score == topic_click()$y]
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Main.Theme %in%  main_theme)
        
        main_theme1 = dataInput_topic()$Keywords[dataInput_topic()$Growth_Index == keyword_click()$x & dataInput_topic()$score == keyword_click()$y]
        scatterplot_df_key <- filter(scatterplot_df_key, scatterplot_df_key$Keywords %in%  main_theme1)
        
      }
      
      scatterplot_df_fltrd = scatterplot_df_key %>% group_by(Keywords_DM)  %>%
        summarise(Q1 = sum(`Q1-2022`),
                  Q4 = sum(`Q4-2021`),
                  score = sum(Tfidf.Score),
                  .groups = 'drop')
      
      ####Min max scaling
      scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
      
      
      scatterplot_df_fltrd['Growth_Index'] <- ((scatterplot_df_fltrd$Q1 - scatterplot_df_fltrd$Q4)/scatterplot_df_fltrd$Q4)
      scatterplot_df_fltrd['Growth_Index'] <- round(scatterplot_df_fltrd['Growth_Index'], digits = 2)
      scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
      
      
      scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$score >= input$Topic_imp[1] & scatterplot_df_fltrd$score <= input$Topic_imp[2])
      
      scatterplot_df_fltrd <- filter(scatterplot_df_fltrd, scatterplot_df_fltrd$Growth_Index >= input$Topic_Growth[1] & scatterplot_df_fltrd$Growth_Index <= input$Topic_Growth[2])
      
      scatterplot_df_fltrd$Capital_txt = paste(
        'Topic:', scatterplot_df_fltrd$Keywords_DM,
        '<br>Importance:',scatterplot_df_fltrd$score,
        '<br>Topic Frequency QoQ Change(%):',(scatterplot_df_fltrd$Growth_Index)*100,'%')
      
      return(scatterplot_df_fltrd)
      
      
    })
    
    output$plotscatter_topic <- renderPlotly({
      
      data <- dataInput_key() %>% arrange((Growth_Index))
      
      
      pal <- colorFactor(c("#BA0606","#FB8704","#F3F60B","#04471D"), 1:length(data$Growth_Index)) #, domain = df1$Opportunity.Value
      colors = pal(c(1:length(data$Growth_Index)))
      plot_ly(data = data, x = ~Growth_Index, y = ~score,color = ~Growth_Index, colors = colors,
              type = 'scatter', mode = 'markers',
              # text = ~Capital_txt,
              text = ~Keywords_DM,
              sizes = c(10, 50),
              size = ~score,
              marker = list(color = colors,opacity = 3, sizemode = 'diameter', line = list(width = 0, color = '#FFFFFF')),
              #color = ~categories,
              hovertext = ~Capital_txt,
              textposition = 'top right',
              textfont = list(size = 8,color = colors),
              hoverinfo="text")  %>% colorbar(title = "Keyword Frequency<br>Change QoQ (%)",orientation = 'h')  %>%
        # layout(plot_bgcolor='rgb(242, 242, 242)') %>% 
        # layout(paper_bgcolor='rgb(242, 242, 242)') %>% 
        #color = ~Keywords,colors  =c("#5000CC","#DD0064","#0045C8","#4C0087","#FF25AC","#B40572","#E0B0FF","#770737","#CCCCFF","#953553","#7F00FF","#CF9FFF"), marker = list(size = 14)) %>%
        layout( xaxis = list(titlefont = t3, tickfont = list(size = 8),tickformat = "0.0%",title = 'Keyword Frequency QoQ Change(%)', zerolinecolor = '#d9d9d9'),
                yaxis = list(titlefont = t3,
                             title = "Importance",
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE ) ) %>%      
        config(displayModeBar = F)%>%
        layout(
          annotations = 
            list(
              list(
                # Localization of annotation : adjustment of x & y
                x = -0.06, 
                y = 1, 
                text = paste0("<b>",'High',"</b>"),
                showarrow = F, 
                xref='paper', 
                yref='paper'),
              list(
                x = -0.06, 
                y = 0, 
                text = paste0("<b>",'Low',"</b>"), 
                showarrow = F, 
                xref='paper', 
                yref='paper')
            )
        )
      
    })
    
    

    
    ########## Investment ##########
    #Timeframe filter
    output$timeframe_investment <- renderUI({
      #data_time <- filter(investments, Timeframe %in% input$variable_geo3)
      time_lst <- reactive({
        sort(unique(investments$Timeframe))
      })
      pickerInput("var_invest_t",label = div(style = "font-size:11px;font-family:Helvetica;", "Timeframe") , choices=c('Q3-2021','Q4-2021','Q1-2022'),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=c('Q3-2021','Q4-2021','Q1-2022'))
    })
    
    
    # investments by themes
    invst_bar_data<- reactive({
      
      investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
      
      barplot_df <- filter(investments, investments$Investments=='Y')
      barplot_df <- filter(barplot_df, barplot_df$Amount > 0)
      barplot_df = barplot_df %>% group_by(Main.Theme)  %>%
        summarise(Total = sum(Amount),.groups = 'drop')
      barplot_df <- subset(barplot_df, select = c('Main.Theme','Total'))
      
      barplot_df['Total'] <- round(barplot_df['Total'], digits = 0)
      
      # barplot_df$newx = str_wrap(barplot_df$Main.Theme, width = 15)
      barplot_df$Main.Theme <- factor(barplot_df$Main.Theme, levels = unique(barplot_df$Main.Theme)[order(barplot_df$Total, decreasing = FALSE)])
      barplot_df$Total1 <- paste0('$',barplot_df$Total,'M')
   return(barplot_df)
      
    })
    
    
    output$investment_bar <- renderPlotly({
      mrg1 <- list(l = 50, r = 50,
                   b = 50, t = 50,
                   pad = 20)
      min_val=min(invst_bar_data()$Total)-min(invst_bar_data()$Total)/2
      max_val=max(invst_bar_data()$Total)+max(invst_bar_data()$Total)/10
   
      plot_ly(data = invst_bar_data(),x = ~Total , y = ~Main.Theme, type = 'bar', bargap = 2,orientation = 'h', source = 'invst_bar_click',
              hoverinfo = "text",text = invst_bar_data()$Total1, textposition = "outside",textfont = list(size = 10,color = "black"),
              hovertext = paste("Theme :", invst_bar_data()$Main.Theme,
                                "<br>Investment :", invst_bar_data()$Total1),
              marker = list(color='#C48BFF'))%>%
        layout(title = "", titlefont = list(size = 14),margin = mrg1,xaxis = list(title = "Investment Amount ($M)",titlefont = list(size = 14),range=c(min_val,max_val),showgrid = FALSE,showticklabels=TRUE),
               yaxis = list(titlefont = list(size = 12), tickfont = list(size = 10),showgrid = FALSE,
                            title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')))%>% 
        config(displayModeBar = F)
      
    })
    
    # investments overview by Market
    
    ##plotly click for bar
    invst_bar_click_1 <- reactive({
      
      event <- event_data("plotly_click", source = "invst_bar_click")
      return(event)
    })
    
    ##data
    invst_map_data <- reactive({

      if (is.null(invst_bar_click_1())){
        
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        investments <- filter(investments, investments$Investments=='Y')
        investments <- filter(investments, investments$Amount > 0)
      }
      
      else{
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        investments <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        
        investments <- filter(investments, investments$Investments=='Y')
        investments <- filter(investments, investments$Amount > 0)
        
      }
      
      df = investments %>% mutate(Code =
                           case_when(Country == 'Australia' ~ "AUS",
                                     Country == 'Argentina'~ "ARG",
                                     Country == 'Brazil' ~ "BRA",
                                     Country == 'Chile' ~ "CHL",
                                     Country == 'Colombia' ~ "COL",
                                     Country == 'India' ~ "IND",
                                     Country == 'Indonesia' ~ "IDN",
                                     Country == 'Japan' ~ "JPN",
                                     Country == 'Malaysia' ~ "MYS",
                                     Country == 'Mexico' ~ "MEX",
                                     Country == 'New Zealand' ~ "NZL",
                                     Country == 'Qatar' ~ "QAT",
                                     Country == 'Singapore' ~ "SGP",
                                     Country == 'South Africa' ~ "ZAF",
                                     Country == 'Thailand' ~ "THA",
                                     Country == 'United Arab Emirates' ~ "ARE"
                           ))
      df <- subset(df, select = c('Primary.Market.Unit','Code','Amount'))
      
      df = df %>% group_by(Primary.Market.Unit,Code) %>%
        summarise(Investment = sum(Amount),.groups = 'drop')
      
      return(df)
      
    })
    
    output$investment_map <- renderPlotly({
      
      mrg <- list(l = 50, r = 50,
                   b = 50, t = 50,
                   pad = 20)
      
      g <- list(scope = 'world',showframe = F,  showland = T,  landcolor = toRGB("grey90"), showlakes = TRUE, lakecolor = toRGB('blue'))
      url <- 'https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json'
      geojson <- rjson::fromJSON(file=url)
      
      plot_ly(invst_map_data(), source = 'invst_mapclick') %>% add_trace(
        type="choroplethmapbox",
        geojson=geojson,
        locations=invst_map_data()$Code,
        z=invst_map_data()$Investment,
        text= invst_map_data()$Primary.Market.Unit,
        colors = c("#EECBF9","#CA51EF","#6C1A85"),
        marker=list(line=list(width=1, opacity=0))) %>%
        layout(mapbox=list(
          style="open-street-map",geo = g,zoom = 0.9,
          center = list(lon = 35 ,lat= -4)),title = '',titlefont = list(size = 14), margin = mrg,showlegend = FALSE) %>%  hide_colorbar() %>%

        config(displayModeBar = F)
      
    
      
    })
    
    # investments by Company
    
    ##plotly click on map
    
    invst_map_click <- reactive({
      event <- event_data("plotly_click", source = "invst_mapclick")
      return(event)
    })
    
    
    
    ##data
    invst_bar2_data <- reactive ({
      
      
      if (is.null(invst_bar_click_1()) & is.null(invst_map_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- investments
        
      }
      
      else if (!is.null(invst_bar_click_1()) & is.null(invst_map_click())){
      
      ##filter for 1st bar
      investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
      main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
      df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
      
      }
      
      else if (is.null(invst_bar_click_1()) & !is.null(invst_map_click())){
      investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
      ##filter for map
      country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
       df <- investments %>% filter(Country == country_click)
      }
      
      else {
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
        
      }
      
      barplot2_df <- filter(df, df$Investments=='Y')
      barplot2_df <- filter(barplot2_df, barplot2_df$Amount > 0)
      barplot2_df = barplot2_df %>% group_by(Client.Name)  %>%
        summarise(Total = sum(Amount),.groups = 'drop')
      barplot2_df <- subset(barplot2_df, select = c('Client.Name','Total'))
      barplot2_df['Total'] <- round(barplot2_df['Total'], digits = 0)
      
      barplot2_df$Total1 <- paste0('$',barplot2_df$Total,'M')
      barplot2_df$newx <- barplot2_df$Client.Name
      barplot2_df$newx <- factor(barplot2_df$newx, levels = unique(barplot2_df$newx)[order(barplot2_df$Total, decreasing = FALSE)])
    
      
      return(barplot2_df)
      
    })
    output$investment_bar2 <- renderPlotly({
      mrg1 <- list(l = 50, r = 50,
                   b = 50, t = 50,
                   pad = 20)
      min_val=min(invst_bar2_data()$Total)-min(invst_bar2_data()$Total)/2
      max_val=max(invst_bar2_data()$Total)+max(invst_bar2_data()$Total)/10
      
      data = invst_bar2_data() %>% arrange(desc(Total))
      
      custColorPal <- colorRampPalette(c("#8420A3","#C658E8","#D964E5","#E195F2"))
      
      custColors <- custColorPal(nrow(data))
      
      plot_ly(data = data,x = ~Total , y = ~newx, type = 'bar', orientation = 'h', source ='invst_bar2',
              hoverinfo = "text",text = data$Total1, textposition = "outside",textfont = list(size = 10,color = "black"),
              hovertext = paste("Theme :", data$newx,
                                "<br> Investment :", data$Total1),
              marker = list(color=custColors, width = 5,height = 5))%>%
        layout(bargap = 0.4,title = "", titlefont = list(size = 14),margin = mrg1,xaxis = list(title = "Investment Amount ($M)",titlefont = list(size = 14),range=c(min_val,max_val),showgrid = FALSE,showticklabels=TRUE),
               yaxis = list(titlefont = list(size = 15), tickfont = list(size = 10),showgrid = FALSE,
                            title=list(text="",standoff = 10L,gridcolor = '#FFFFFF')))%>% 
        config(displayModeBar = F)
      
    })
    
    #Investment Details by Priorities
    
    ##plolty click
    invst_bar2_click <- reactive({
      event <- event_data("plotly_click", source = "invst_bar2")
      return(event)
    })
    
    ##data
    
    invst_react_table <- reactive({

      
      if (is.null(invst_bar_click_1()) & is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- investments
        
      }
      
      else if (!is.null(invst_bar_click_1()) & is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        df <- filter(investments, investments$Main.Theme %in% invst_bar_click_1()$y)
        print('aa')
        print(df)
        
      }
      
      else if (is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- investments %>% filter(Country == country_click)
      }
      
      else if (is.null(invst_bar_click_1()) & is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- filter(investments, investments$Client.Name %in%  invst_bar2_click()$y)  
  
      }
      
      else if (!is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        df <- filter(investments, investments$Main.Theme %in% invst_bar_click_1()$y)  
        
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
      }
      
      else if (!is.null(invst_bar_click_1()) & is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        df <- filter(investments, investments$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(df, df$Client.Name %in%  invst_bar2_click()$y)  
        
      }
      
      else if (is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- filter(investments, investments$Client.Name %in%  invst_bar2_click()$y)  
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
      }
      
      
      else {
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- filter(investments, investments$Main.Theme %in% invst_bar_click_1()$y)  
        
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
        
        df <- filter(df, df$Client.Name %in%  invst_bar2_click()$y)  
        
      }
      
      data <- df
      data <- filter(data, data$Amount > 0)
      data <- subset(data, select = c("Main.Theme","Client.Name","Time","Amount"))
      data = data %>% group_by(Main.Theme,Client.Name,Time)  %>%
        summarise(Amount = sum(Amount),.groups = 'drop')
      data <- data[order(data$Main.Theme,-data$Amount),]
      return(data)
    })
    
    output$investment_Sentences1 <- renderReactable({
      
      bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
      }
      
      reactable(invst_react_table(), groupBy = "Main.Theme",pagination = FALSE, highlight = TRUE, height = 250,minRows = 12,
                columns = list(
                  Client.Name = colDef(name = "Client Name"),
                  Main.Theme = colDef(name = "Main Theme"),
                  
                  Time = colDef(na = "Not Mentioned",name = 'Time (Years)'),
                  Amount = colDef(name= "Amount (Mil USD)",
                                  cell = function(value) {
                                    
                                    width <- paste0(value * 100 / max(invst_react_table()$Amount), "%")
                                    value <- format(round(value,0), big.mark = ",")
                                    # Fix each label using the width of the widest number (incl. thousands separators)
                                    value <- format(value, width = 12, justify = "right")
                                    bar_chart(value, width = width, fill = "#D54ACF")
                                  },
                                  align = "left",
                                  style = list(fontFamily = "monospace", whiteSpace = "pre"))
                  
                ))
      
    })
    
    
    #Investment Sentences
    output$investment_Sentences <-  renderDataTable({ 
      
      
      if (is.null(invst_bar_click_1()) & is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- investments
        
      }
      
      else if (!is.null(invst_bar_click_1()) & is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        
      }
      
      else if (is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- investments %>% filter(Country == country_click)
      }
      
      else if (is.null(invst_bar_click_1()) & is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- filter(investments, investments$Client.Name %in%  invst_bar2_click()$y)  
        
      }
      
      else if (!is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
      }
      
      else if (!is.null(invst_bar_click_1()) & is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        ##filter for 1st bar
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        df <- filter(df, df$Client.Name %in%  invst_bar2_click()$y)  
        
      }
      
      else if (is.null(invst_bar_click_1()) & !is.null(invst_map_click()) & !is.null(invst_bar2_click())){
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        df <- filter(investments, investments$Client.Name %in%  invst_bar2_click()$y)  
        ##filter for map
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
      }
      
      
      else {
        investments <- filter(investments, investments$Timeframe %in%  input$var_invest_t)
        
        main.theme <- filter(invst_bar_data(), invst_bar_data()$Main.Theme %in% invst_bar_click_1()$y)  
        df <- filter(investments, investments$Main.Theme %in% main.theme$Main.Theme)  
        
        country_click = invst_map_data()$Country[invst_map_data()$Investment == invst_map_click()$z]
        df <- df %>% filter(Country == country_click)
        
        df <- filter(df, df$Client.Name %in%  invst_bar2_click()$y)  
        
      }
      
      
      data <- filter(df, df$Investments=='Y')
      data <- filter(data, data$Amount > 0)
      data = data %>% group_by(Keywords,Sentences)  %>%
        summarise(Total = sum(Amount),.groups = 'drop')
      data <- subset(data, select = c("Keywords","Sentences","Total"))
      data <- data[order(-data$Total),]
      data <- subset(data, select = c("Sentences"))
      
      
      iris_upd <- cbind(' ' = '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>', data)
      
      datatable(
        iris_upd, 
        escape = -2,
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0)),
            list(orderable = FALSE, className = 'details-control', targets = 1),
            list(
              targets = 2,
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 6 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 300) + '...</span>' : data;",
                "}")
            )
          ), dom= 't'
          
        ),
        callback = JS("
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return'<p>' + d[2] + '</p>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                  }
                  });"
        ))
      
      
      
    })
    
    
    ############Theme Ranking ##############
    
    Theme_ranking_data <- filter(data, Timeframe %in% c("Q4-2021","Q1-2022"))
    
    
    Theme_ranking_data <- subset(Theme_ranking_data, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Timeframe,Tfidf.Score))
    
    # Theme Ranking Tab Start
    geo_list3 <- reactive({
      sort(unique(Theme_ranking_data$Primary.Market.Unit))
    }) 
    
    output$var_geo3 <- renderUI({
      pickerInput("variable_geo3",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list3(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=geo_list3())
    })
    
    # Side bar filter for industry #
    mkt_list3 <- reactive({
      sort(unique(Theme_ranking_data$Primary.Industry))
    })
    
    output$var_mkt3 <- renderUI({
      data_geo <- filter(Theme_ranking_data, Primary.Market.Unit %in% input$variable_geo3)
      mkt_list3 <- reactive({
        sort(unique(data_geo$Primary.Industry))
      })
      pickerInput("variable_mkt3",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list3(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=mkt_list3())
    })
    
    output$var_client3 <- renderUI({
      data_geo <- filter(Theme_ranking_data, Primary.Market.Unit %in% input$variable_geo3)
      data_geo <- filter(data_geo, Primary.Industry %in% input$variable_mkt3)
      client_list3 <- reactive({
        sort(unique(data_geo$Client.Name))
      })
      pickerInput("variable_client3",label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list3(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=client_list3())
    })
    
    ###function for theme ranking line chart
    
    output$theme_ranking_plot <- renderPlotly({
      
      theme_data <- filter(Theme_ranking_data, Theme_ranking_data$Primary.Market.Unit %in%  input$variable_geo3)
      
      theme_data <- filter(theme_data, theme_data$Primary.Industry %in%  input$variable_mkt3)
      
      theme_data <- filter(theme_data, theme_data$Client.Name %in%  input$variable_client3)
      
      theme_data = theme_data %>% group_by(Main.Theme,Timeframe)  %>%
        summarise(Importance = sum(Tfidf.Score),.groups = 'drop')
      
      # theme_data$Timeframe <- factor(theme_data$Timeframe, labels = c("Q3-2021","Q4-2021","Q1-2022"), ordered=TRUE)
      
      ##giving ranks
      
      theme_data <- theme_data %>% 
        arrange( -Importance) %>%
        group_by(Timeframe) %>%
        mutate(rank=row_number())
      
       theme_data$Timeframe <- factor(theme_data$Timeframe, labels = c("Q4-2021","Q1-2022"), ordered=TRUE)
      
      
    #   plot_ly(data = theme_data, x = ~Timeframe, y = ~rank,color = ~Main.Theme, 
    #           type = 'scatter', mode = 'lines + markers',
    #           text =   ~paste('</br>Main Theme: ' , Main.Theme,
    #                           '</br>TimeFrame: ', Timeframe,
    #                           '</br>Importance: ', Importance),
    #           hoverinfo="text") %>%
    #           # line = list(color = c("#8F286E","#8222D1","#A657EB","#A478BE","#D79CFC","#C57CF2","#C181F5","#F5CBFB","#E9D4F9","#E2C3FB","#D8B1F8","#B8B1F8","#CDB1F8","#EEC4F7","#E9B8F4","#DEB2E8","#D7AAD8","#CA9BCB","#F4CBF5","#F7C3F8")))%>% 
    #     
    #     layout( xaxis = list(side ="top", titlefont = t3, tickfont = list(size = 8),title = '', zerolinecolor = '#d9d9d9' ),
    #             yaxis = list(titlefont = t3,
    #                          title = "",
    #                          zeroline = FALSE,
    #                          showline = FALSE,
    #                          showticklabels = FALSE ,
    #                          annotations = ~Main.Theme) ,
    #             showlegend = FALSE) %>%      
    #     config(displayModeBar = F)
    #   
    # })
    # ################################################33
    # 
    my_theme <- function() {
      
      # Colors
      color.background = "white"
      color.text = "black"
      
      # Begin construction of chart
      theme_bw(base_size=15) +
        
        # Format background colors
        theme(panel.background = element_rect(fill=color.background,
                                              color=color.background)) +
        theme(plot.background  = element_rect(fill=color.background,
                                              color=color.background)) +
        theme(panel.border     = element_rect(color=color.background)) +
        theme(strip.background = element_rect(fill=color.background,
                                              color=color.background)) +
        
        # Format the grid
        theme(panel.grid.major.y = element_blank()) +
        theme(panel.grid.minor.y = element_blank()) +
        theme(axis.ticks = element_blank()) +
        
        # Format the legend
        theme(legend.position = "none") +
        
        # Format title and axis labels
        theme(plot.title       = element_text(color=color.text, size=12, face = "plain")) +
        theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
        theme(axis.title.y     = element_text(size=12, color="black", face = "plain",
                                              vjust=1.25)) +
        theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5,
                                              color = color.text)) +
        theme(axis.text.y      = element_text(size=8, color = color.text)) +
        theme(strip.text       = element_text(face = "bold")) 

        # Plot margins
    }
    
    theme_data['Importance'] <- round(theme_data['Importance'], digits = 0)
    print(theme_data)
    
    gg <- ggplot(theme_data, aes(x = Timeframe, y = rank, group = Main.Theme ,imp =Importance)) +
      geom_line(aes(color = Main.Theme, alpha = 1), size = 0.5) +
      geom_point(aes(color = Main.Theme, alpha = 1), size = 2) +
      # geom_point(color = "#FFFFFF", size = 1) +
      scale_y_reverse(breaks = 1:nrow(theme_data)) +
      scale_x_discrete(breaks = 1:10) +
      theme(legend.position = 'none') +
      geom_text(data = theme_data %>% filter(Timeframe == "Q4-2021"),
                aes(label = Main.Theme, x = 0.6) , hjust = 0.5,
                fontface = "plain", color = "black", size = 3) +
      geom_text(data = theme_data %>% filter(Timeframe == "Q1-2022"),
                aes(label = Main.Theme, x = 2.4) , hjust = 0.5,
                color = "black", size = 3) +
      labs(x = '', y = 'Theme Rank', title = '') +
      scale_color_manual(values = c("blue","blue","blue","blue","purple","blue","blue","blue","blue","blue","blue","blue","blue","blue","purple","purple","blue","blue"))  + 
      my_theme() 



    
    ggplotly(gg,tooltip=c("x", "y","imp")) %>% config(displayModeBar = F)  %>%
      layout( font= t1,
        annotations = 
          list(
            list(
              # Localization of annotation : adjustment of x & y
              x = 0.05, 
              y = 1.03, 
              text = paste0("<b>",'Q4-2021',"</b>"),
              showarrow = F, 
              xref='paper', 
              yref='paper'),
            list(
              x = 0.95, 
              y = 1.03, 
              text = paste0("<b>",'Q1-2022',"</b>"), 
              showarrow = F, 
              xref='paper', 
              yref='paper')
          )
      ) 
    })
    
    
    
    
    
    
    ########################################################
    #############################MarkeT Unit View #########################
    
    ######Reactive

    output$var_geo4 <- renderUI({
      pickerInput("variable_geo4",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=geo_list())
    })
    
    
    output$var_mkt4 <- renderUI({
      data_geo <- filter(data, Primary.Market.Unit %in% input$variable_geo4)
      mkt_list <- reactive({
        sort(unique(data_geo$Primary.Industry))
      })
      pickerInput("variable_mkt4",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=mkt_list())
    })
    
    # Side bar filter for timeframe #
    
   
    output$var_tf4 <- renderUI({
      data_time <- filter(data, Primary.Market.Unit %in% input$variable_geo4)
      data_time <- filter(data_time, Primary.Industry %in% input$variable_mkt4)
      time_list <- reactive({
        sort(unique(data_time$Timeframe))
      })
      pickerInput("variable_tf4",label = div(style = "font-size:11px;font-family:Helvetica;", "Timeframe") , choices=c('Q3-2021','Q4-2021','Q1-2022'),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected= "Q1-2022")
    })
    
    
    
    output$topkey4 <- renderUI({
      sliderInput("topkey4",label = div(style = "font-size:11px;font-family:Helvetica;", "Top Keyword Count"),ticks = FALSE,
                  min =  5, max = 20,
                  value = 20
      )
    })
    
    
    
    ##map
    
    map_data <- reactive ({
      
      data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
      data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
      data <- filter(data, data$Timeframe %in%  input$variable_tf4)
    
      
      df = data %>% mutate(Code =
                             case_when(country == 'Australia' ~ "AUS",
                                       country == 'Argentina'~ "ARG",
                                       country == 'Brazil' ~ "BRA",
                                       country == 'Chile' ~ "CHL",
                                       country == 'Colombia' ~ "COL",
                                       country == 'India' ~ "IND",
                                       country == 'Indonesia' ~ "IDN",
                                       country == 'Japan' ~ "JPN",
                                       country == 'Malaysia' ~ "MYS",
                                       country == 'Mexico' ~ "MEX",
                                       country == 'New Zealand' ~ "NZL",
                                       country == 'Qatar' ~ "QAT",
                                       country == 'Singapore' ~ "SGP",
                                       country == 'South Africa' ~ "ZAF",
                                       country == 'Thailand' ~ "THA",
                                       country == 'United Arab Emirates' ~ "ARE"
                             ))
      
      df = df %>% group_by(Code,Primary.Market.Unit)  %>%
        summarise(Frequency = sum(Keyword.Frequency),.groups = 'drop')
      return(df)
      
    })
    
    output$comp_map <- renderPlotly({
      mrg <- list(l = 50, r = 50,
                  b = 50, t = 50,
                  pad = 20)
      
      url <- 'https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json'
      geojson <- rjson::fromJSON(file=url)
      
      g <- list(scope = 'world',showframe = F,  showland = T,  landcolor = toRGB("grey90"), showlakes = TRUE, lakecolor = toRGB('blue'))
      
      plot_ly(map_data(), source = 'mapclick') %>% add_trace(
        type="choroplethmapbox",
        geojson=geojson,
        locations=map_data()$Code,
        z=map_data()$Frequency,
        text= map_data()$Primary.Market.Unit,
        colors = c("#EECBF9","#CA51EF","#6C1A85"),
        marker=list(line=list(width=1, opacity=0))) %>%
        layout(mapbox=list(
          style="open-street-map",geo = g,zoom = 0.9,
          center = list(lon = 35 ,lat= -4)),title = '',font = t1, margin = mrg,showlegend = FALSE) %>% colorbar(title = "",len=0.85,thickness=15,orientation ='v')  %>% 
        config(displayModeBar = F)
      
    })
    
    #############Donut chart
    
    ##map click reactive
    
    map_click <- reactive({
      
      event <- event_data("plotly_click", source = "mapclick")
      return(event)
      
    })
    
    
    donut_data <- reactive({
      

      if (is.null(map_click())){
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
      }
      
      else {
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- data %>% filter(country == country_click)
      }
      
      df <- data
      data <- data %>% group_by(Main.Theme)  %>%
        summarise(importance = sum(Tfidf.Score),.groups = 'drop')
      
      data$percent <- (data$importance/sum(df$Tfidf.Score))*100

      return(data)
    })
    
    
    output$comp_donut <- renderPlotly({
      
      mrg <- list(l = 0, r = 0,
                  b = 5, t = 5,
                  pad = 0)

      data = donut_data() %>% arrange(desc(percent))
      
      fig <- plot_ly(data,labels = ~Main.Theme, values = ~percent, source = 'donut_click',textposition = 'outside',
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
    
    ####wordcloud
    
    ###reactive click for donut
    
    donut_click <- reactive({
      event <- event_data("plotly_click", source = "donut_click")
      return(event)
      
    })
    
    wordcloud_data <- reactive({
      

      if (is.null(donut_click()) & is.null(map_click())){
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
      }
      
      else if (!is.null(donut_click()) & is.null(map_click())){
        
      data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
      data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
      data <- filter(data, data$Timeframe %in%  input$variable_tf4)
      
      theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
      theme_selected <- theme_selected$Main.Theme
      data <- filter(data, data$Main.Theme %in%  theme_selected)
      }
      
      else if (is.null(donut_click()) & !is.null(map_click())){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
      country_click = map_data()$country[map_data()$Frequency == map_click()$z]
      data <- filter(data, data$country %in% country_click)
      
      }
      
      else{
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- filter(data, data$country %in% country_click)
        
        theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
        theme_selected <- theme_selected$Main.Theme
        data <- filter(data, data$Main.Theme %in%  theme_selected)
      }
      
      
      data_wc = data %>% group_by(Client.Name)  %>%
        summarise(Keyword.Frequency = sum(Keyword.Frequency),
                  .groups = 'drop') %>% as.data.frame()
      
      
      data_wc <-data_wc[order(-data_wc$Keyword.Frequency),]
      
      data_wc <- subset(data_wc, select = c(Client.Name, Keyword.Frequency))
      data_wc =  data_wc[complete.cases(data_wc),]
      
      return(data_wc)
      
    })
    
    output$comp_wc <- renderWordcloud2({
      data <- wordcloud_data()
      custColorPal <- colorRampPalette(c("#C559E5","#1737CD","#B44AD5"))
      custColors <- custColorPal(nrow(data))
      
      wordcloud2(data = data, size = 0.4,minSize = 0.2, gridSize = 20, shape = 'rectangle',rotateRatio = 0, color=custColors) 
      
    })
    
    
    ###sentences
    
    #reactive for wc
    wc_clicked <- reactive ({
      var1 <- strsplit(as.character(input$clicked),split = ":")[[1]][1]
   
      return(var1)
    })
    
    
    output$comp_Sentences <- renderDataTable({ 
      

      
      if (is.null(donut_click()) & is.null(map_click()) & is.null(input$clicked)){
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
      }
      
      else if (!is.null(donut_click()) & is.null(map_click()) & is.null(input$clicked)){

        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
        theme_selected <- theme_selected$Main.Theme
        data <- filter(data, data$Main.Theme %in%  theme_selected)
      }
      
      else if (is.null(donut_click()) & !is.null(map_click()) & is.null(input$clicked)){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- filter(data, data$country %in% country_click)
        
      }
      
      else if (is.null(donut_click()) & is.null(map_click()) & !is.null(input$clicked)){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        data <- filter(data, data$Client.Name %in% wc_clicked())
        }
      
      else if (!is.null(donut_click()) & !is.null(map_click()) & is.null(input$clicked)){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- filter(data, data$country %in% country_click)
        
        theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
        theme_selected <- theme_selected$Main.Theme
        data <- filter(data, data$Main.Theme %in%  theme_selected)
      }
      
      else if (!is.null(donut_click()) & is.null(map_click()) & !is.null(input$clicked)){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        data <- filter(data, data$Client.Name %in% wc_clicked())
   
        theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
        theme_selected <- theme_selected$Main.Theme
        data <- filter(data, data$Main.Theme %in%  theme_selected)
        
      }
      else if (is.null(donut_click()) & !is.null(map_click()) & !is.null(input$clicked)){
        
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        data <- filter(data, data$Client.Name %in% wc_clicked())
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- filter(data, data$country %in% country_click)
     
      }
      
      else {
        data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo4)
        data <- filter(data, data$Primary.Industry %in%  input$variable_mkt4)
        data <- filter(data, data$Timeframe %in%  input$variable_tf4)
        
        country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        data <- filter(data, data$country %in% country_click)
        
        theme_selected <- donut_data() %>% filter(row(donut_data()) == donut_click()$pointNumber+1)
        theme_selected <- theme_selected$Main.Theme
        data <- filter(data, data$Main.Theme %in%  theme_selected)

        list_comp=c(unique(as.character(data$Client.Name)))

        if(wc_clicked()%in% list_comp){
          data <- filter(data, data$Client.Name %in% wc_clicked())
          }
        else{
          data <- data
            }
        
        # data <- filter(data, data$Client.Name %in% wc_clicked())
        
        # country_click = map_data()$country[map_data()$Frequency == map_click()$z]
        # data <- filter(data, data$country %in% country_click)
        
    
      }
      print(data)
      data <- subset(data, select = c("Sentences","Keyword.Frequency"))
      data <- data[order(-data$Keyword.Frequency),]
      
      iris_upd <- cbind(' ' = '<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>', data)
      
      datatable(
        iris_upd, 
        escape = -2,
        options = list(
          columnDefs = list(
            list(visible = FALSE, targets = c(0)),
            list(orderable = FALSE, className = 'details-control', targets = 1),
            list(
              targets = 2,
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 6 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 300) + '...</span>' : data;",
                "}")
            )
          ), dom= 't'
          
        ),
        callback = JS("
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return'<p>' + d[2] + '</p>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_open.png\"/>');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('<img src=\"https://raw.githubusercontent.com/DataTables/DataTables/master/examples/resources/details_close.png\"/>');
                  }
                  });"
        ))
      
      
      
      
    })
    
    
    
#################Social Media Theme analysis########################
    
    mrg <- list(l = 50, r = 50,
                b = 50, t = 50,
                pad = 20)
    
###Global filters
    # Side bar filter for geography #
    geo_list_td <- reactive({
      sort(unique(twitter_data$Primary.Market.Unit))
    }) 
    
    output$var_geo7 <- renderUI({
      pickerInput("variable_geo_td",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list_td(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=geo_list_td())
    })
    
    # Side bar filter for domain #
    mkt_list_td <- reactive({
      sort(unique(twitter_data$Primary.Industry))
    })
    
    output$var_mkt7 <- renderUI({
      data_geo <- filter(twitter_data, Primary.Market.Unit %in% input$variable_geo_td)
      mkt_list_td <- reactive({
        sort(unique(data_geo$Primary.Industry))
      })
      pickerInput("variable_mkt_td",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list_td(),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected=mkt_list_td())
    })
    
    
    
##Theme view wc
    
output$theme_view_wc <- renderWordcloud2({
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
  
    latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
    latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Tfidf.Score", fun.aggregate=sum)
    latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q2-2022")
    latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
    latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
    names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
    quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                         by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
    # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
    quarter_df1[is.na(quarter_df1)] <- 0
    
    scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
     
                summarise(score = sum(Tfidf.Score),
                
                .groups = 'drop')
    
    ####Min max scaling
    scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))

    scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
    
    scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))
    
    print(scatterplot_df_fltrd)

    scatterplot_df_fltrd = scatterplot_df_fltrd %>% arrange(desc(score))
    
    custColorPal <- colorRampPalette(c("#B44AD5","#1737CD"))
    custColors <- custColorPal(nrow(scatterplot_df_fltrd))
    
    wordcloud2(data = scatterplot_df_fltrd, size = 0.4, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color=custColors) 
    
    
})
    
    
    
###Donut 
    
#click on wordcloud
#reactive for wc

sm_wc_click <- reactive ({
    var1 <- strsplit(as.character(input$sm_wc_clicked),split = ":")[[1]][1]
    return(var1)
  })
  


sm__theme_donut <- reactive ({

  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  if(!is.null(input$sm_wc_clicked)){
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
  twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
 
   }
  
  else {

    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
  
  }
  
  df <- twitter_data
  data <- twitter_data %>% group_by(Primary.Market.Unit)  %>%
    summarise(importance = sum(Keyword.Frequency),.groups = 'drop')
  
  data$percent <- (data$importance/sum(df$Keyword.Frequency))*100
  return(data)
  
})
   

output$sm_theme_donut_plt <- renderPlotly({
  
  data = sm__theme_donut() %>% arrange(desc(percent))
  
  
  plot_ly(data,labels = ~Primary.Market.Unit, values = ~percent,source = 'sm_donut',textposition = 'outside',
          textinfo = 'label',marker = list(colors = c("#A122C3","#E32CC2","#A72CE3","#F28032","#3240F2","#A432F2")))%>% 
    add_pie(hole = 0.6) %>% 
    layout(title = "",  showlegend = F, margin = mrg,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
    config(displayModeBar = F)
  
})
    
##Bar plot

##donut is clicked

sm_th_donut_click <- reactive({
  event <- event_data("plotly_click", source = "sm_donut")
  return(event)
  
})



sm__theme_bar2 <- reactive ({
  
  event <- event_data("plotly_click", source = "sm_donut")
  print(event)
  
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  if(is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked)){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
  }
  
  else if (is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked)){
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
  }
  
  else if (!is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked)){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
  }
  else{
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
   
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)

  }
    
  
  data <- twitter_data %>% group_by(Client.Name)  %>%
      summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
  data$Client.Name <- factor(data$Client.Name, levels = unique(data$Client.Name)[order(data$Total, decreasing = FALSE)])
  return(data)
  
})

output$sm_theme_bar_plt <- renderPlotly({
    
data <- sm__theme_bar2()
min_val=min(data$Total)-min(data$Total)/2
max_val=max(data$Total)+max(data$Total)/10

data$Client.Name = str_wrap(data$Client.Name, width = 45)
data$Client.Name <- factor(data$Client.Name, levels = unique(data$Client.Name)[order(data$Total, decreasing = FALSE)])

data = data %>% arrange(desc(Total))

custColorPal <- colorRampPalette(c("#B44AD5","#1737CD"))
custColors <- custColorPal(nrow(data))

plot_ly(data = data,x = ~Total , y = ~Client.Name, type = 'bar',orientation = 'h',source = "sm_bar",
        hoverinfo = "text",text = data$Total, textposition = "outside",textfont = list(size = 10,color = "black"),
        hovertext = paste("Client Name :", data$Client.Name,
                          "<br>Keyword Frequency :", data$Total),
        marker = list(color=custColors))%>%
  layout(title = "", titlefont = list(size = 16),margin = mrg,xaxis = list(title = "",titlefont = list(size = 14),range=c(min_val,max_val),showgrid = FALSE,showticklabels=TRUE),
         yaxis = list(titlefont = list(size = 8), tickfont = list(size = 8),showgrid = FALSE,
                      title=list(text="",standoff = 10L,gridcolor = '#FFFFFF'),bargap = 7))%>% 
  config(displayModeBar = F)

})
    
###########topic analysis wc

##bar is clicked

sm_th_bar_click <- reactive({
  event <- event_data("plotly_click", source = "sm_bar")
  return(event)
  
})

topic_analysis_wc <- reactive ({
  
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  
  if (is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
  
  }
  
  else if (!is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
  }
  else if (is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
  }
  
  else if (is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else if (!is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())

  }
  
  else if (!is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
  
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
    }
  
  else if (is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
    
  }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    print(client)
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    print(twitter_data)
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
    
  }
  
  data <- twitter_data %>% group_by(Keywords)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop') %>% as.data.frame()
  
  data <- subset(data, select = c(Keywords, Total))

  
  if(length(data$Keywords) == 1){
    data = data %>% add_row(Keywords = "", Total = 0) %>% data.frame()
    data =  data[complete.cases(data),]
    
  }
  else{
    data =  data[complete.cases(data),]
  }
  return(data)
  
})

output$sm_theme_bar_plt2 <- renderWordcloud2({
  
  wordcloud2(data = topic_analysis_wc(), size = 0.45, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})




##############data table

output$sm_datatable <- renderDataTable({ 
  
  
  if (is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
  }
  
  else if (!is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
  }
  else if (is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
  }
  
  else if (is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else if (!is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & is.null(sm_th_bar_click())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
  }
  
  else if (!is.null(sm_th_donut_click()) & is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else if (is.null(sm_th_donut_click()) & !is.null(input$sm_wc_clicked) & !is.null(sm_th_bar_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
    
  }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_th_bar_click()$y)  
    print(client)
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    print(twitter_data)
    
    theme_selected <- sm__theme_donut() %>% filter(row(sm__theme_donut()) == sm_th_donut_click()$pointNumber+1)
    theme_selected <- theme_selected$Primary.Market.Unit
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  theme_selected)
    
    
  }
  
  
  data <- twitter_data %>% group_by(Sentences)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
  
  data <- subset(data, select = c("Sentences","Total"))
  data <- data[order(-data$Total),]
  
  datatable(data, options = list(dom = 't'))
  
})



#####################Social Media Market View####################

##Global filters

# Side bar filter for geography #
# geo_list_td <- reactive({
#   sort(unique(twitter_data$Primary.Market.Unit))
# }) 

output$var_geo8 <- renderUI({
  pickerInput("variable_geo_td_2",label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list_td(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=geo_list_td())
})

# Side bar filter for domain #
# mkt_list_td <- reactive({
#   sort(unique(twitter_data$Primary.Industry))
# })

output$var_mkt8 <- renderUI({
  data_geo <- filter(twitter_data, Primary.Market.Unit %in% input$variable_geo_td_2)
  mkt_list_td <- reactive({
    sort(unique(data_geo$Primary.Industry))
  })
  pickerInput("variable_mkt_td_2",label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list_td(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=mkt_list_td())
})

####map



td_map_data <- reactive ({
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
  
df = twitter_data %>% mutate(Code =
                       case_when(country == 'Australia' ~ "AUS",
                                 country == 'Argentina'~ "ARG",
                                 country == 'Brazil' ~ "BRA",
                                 country == 'Chile' ~ "CHL",
                                 country == 'Colombia' ~ "COL",
                                 country == 'India' ~ "IND",
                                 country == 'Indonesia' ~ "IDN",
                                 country == 'Japan' ~ "JPN",
                                 country == 'Malaysia' ~ "MYS",
                                 country == 'Mexico' ~ "MEX",
                                 country == 'New Zealand' ~ "NZL",
                                 country == 'Qatar' ~ "QAT",
                                 country == 'Singapore' ~ "SGP",
                                 country == 'South Africa' ~ "ZAF",
                                 country == 'Thailand' ~ "THA",
                                 country == 'United Arab Emirates' ~ "ARE",
                                 country ==  "Saudi Arabia" ~ 'SAU',
                                 country == "Malaysia"  ~ 'MYS',
                                 country =='Oman' ~ 'OMN',
                                 country == 'Philippines' ~ 'PHL'
                       ))

df = df %>% group_by(Code,Primary.Market.Unit)  %>%
  summarise(Frequency = sum(Keyword.Frequency),.groups = 'drop')
return(df)

})

output$td_map <- renderPlotly({
  mrg <- list(l = 50, r = 50,
              b = 50, t = 50,
              pad = 20)
  
  
  
  url <- 'https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json'
  geojson <- rjson::fromJSON(file=url)
  

 
                      
  
  g <- list(scope = 'world',showframe = F,  showland = T,  landcolor = toRGB("grey90"), showlakes = TRUE, lakecolor = toRGB('blue'))
  
  plot_ly(td_map_data(),source = "sm_mkt_map_clicked") %>% add_trace(
    type="choroplethmapbox",
    geojson=geojson,
    locations=td_map_data()$Code,
    z=td_map_data()$Frequency,
    text= td_map_data()$Primary.Market.Unit,
    colors = c("#EAA8FE","#D870F8","#B133D8","#6C1A85"),
    marker=list(line=list(width=1, opacity=0))) %>%
    layout(mapbox=list(
      style="open-street-map",geo = g,zoom = 0.9,
      center = list(lon = 35 ,lat= -4)),font = t1, margin = mrg,showlegend = FALSE) %>%  hide_colorbar() %>%

    config(displayModeBar = F)
  
})


##################- wordcloud

##map click


sm_mkt_map_click <- reactive ({
  event <- event_data("plotly_click", source = "sm_mkt_map_clicked")
  return(event)
  
})


sm_mkt_wc <- reactive ({
  
  if(is.null(sm_mkt_map_click())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
  }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
  }
  
  
  
  latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
  latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Tfidf.Score", fun.aggregate=sum)
  latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q2-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))
  
  
  return(scatterplot_df_fltrd)
})



output$theme_view_wc_2 <- renderWordcloud2({
  data <- sm_mkt_wc()
  data = data %>% arrange(desc(score))
  
  custColorPal <- colorRampPalette(c("#B44AD5","#1737CD"))
  custColors <- custColorPal(nrow(data))
  
  wordcloud2(data = data, size = 0.5, gridSize = 25, shape = 'rectangle',rotateRatio = 0, color=custColors) 
  
})


##Bar plot
##click on wc

sm_mkt_wc_click <- reactive ({
  var1 <- strsplit(as.character(input$sm_mkt_wc_clicked),split = ":")[[1]][1]
  
  return(var1)
})

sm_market_bar2 <- reactive ({
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  
  if(is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked)){
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
  }
  
  else if (!is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked)){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
  }
  else if (is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked)){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
  }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
  }
  
  data <- twitter_data %>% group_by(Client.Name)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
  data$Client.Name <- factor(data$Client.Name, levels = unique(data$Client.Name)[order(data$Total, decreasing = FALSE)])
  
  return(data)
  
})

output$sm_market_bar_plt <- renderPlotly({
  
  data <- sm_market_bar2()
  min_val=min(data$Total)-min(data$Total)/2
  max_val=max(data$Total)+max(data$Total)/10
  
  data$Client.Name = str_wrap(data$Client.Name, width = 45)
  data$Client.Name <- factor(data$Client.Name, levels = unique(data$Client.Name)[order(data$Total, decreasing = FALSE)])
  
  data = data %>% arrange(desc(Total))
  
  custColorPal <- colorRampPalette(c("#B44AD5","#1737CD"))
  custColors <- custColorPal(nrow(data))
  
 
  plot_ly(data = data,x = ~Total , y = ~Client.Name, type = 'bar',orientation = 'h',source = "sm_mkt_bar_clicked",
          hoverinfo = "text",text = data$Total, textposition = "outside",textfont = list(size = 10,color = "black"),
          hovertext = paste("Client Name :", data$Client.Name,
                            "<br>Keyword Frequency :", data$Total),
          marker = list(color='#C48BFF'))%>%
    layout( titlefont = list(size = 16),margin = mrg,xaxis = list(title = "",titlefont = list(size = 14),range=c(min_val,max_val),showgrid = FALSE,showticklabels=TRUE),
           yaxis = list(titlefont = list(size = 8), tickfont = list(size = 8),showgrid = FALSE,
                        title=list(text="",standoff = 10L,gridcolor = '#FFFFFF'),bargap = 7))%>% 
    config(displayModeBar = F)
  
})

###topic analysis wc

sm_mkt_bar_clk <- reactive ({
  event <- event_data("plotly_click", source = "sm_mkt_bar_clicked")
  return(event)
  })


topic_analysis_wc_market <- reactive ({
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  
  if(is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) & is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
  }
  
  else if (!is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) & is.null(sm_mkt_bar_clk())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
  }
  
  else if (is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
  }
  
  else if (is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else if (!is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
     }
  else if (!is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
  
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
    }
  else if (is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
     }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  data <- twitter_data %>% group_by(Keywords)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop') %>% as.data.frame()
  
  data <- subset(data, select = c(Keywords, Total))
  
  if(length(data$Keywords) == 1){
    data = data %>% add_row(Keywords = "", Total = 0) %>% data.frame()
    data =  data[complete.cases(data),]
    
  }
  else{
    data =  data[complete.cases(data),]
  }
  
  return(data)

})

output$sm_mrt_wc<- renderWordcloud2({
  
  wordcloud2(data = topic_analysis_wc_market(), size = 0.7,minSize = 0.4, gridSize = 20, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})

output$sm_market_datatable <- renderDataTable({ 
  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  
  
  if(is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) & is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
  }
  
  else if (!is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) & is.null(sm_mkt_bar_clk())){
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
  }
  
  else if (is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
  }
  
  else if (is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else if (!is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
  }
  else if (!is.null(sm_mkt_map_click()) & is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  else if (is.null(sm_mkt_map_click()) & !is.null(input$sm_mkt_wc_clicked) &  !is.null(sm_mkt_bar_clk())){
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_td_2)
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_td_2)
    
    country_click = td_map_data()$country[td_map_data()$Frequency == sm_mkt_map_click()$z]
    twitter_data <- filter(twitter_data, twitter_data$country %in% country_click)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in% sm_mkt_wc_click())
    
    client <- filter(twitter_data, twitter_data$Client.Name %in% sm_mkt_bar_clk()$y)  
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in% client$Client.Name)  
    
  }
  
  
  data <- twitter_data %>% group_by(Sentences)  %>%
    summarise(Total = sum(Keyword.Frequency),.groups = 'drop')
  
  data <- subset(data, select = c("Sentences","Total"))
  data <- data[order(-data$Total),]
  
  datatable(data, options = list(dom = 't'))
  
})

###############Compare Earning Transcript

# Side bar filter for client #
output$var_client9 <- renderUI({
  client_list <- reactive({
    sort(unique(data$Client.Name))
  })
  pickerInput("variable_client_cmp",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=client_list())
})

output$var_mkt9 <- renderUI({
  data <- filter(data, Client.Name %in% input$variable_client_cmp)
  mkt_list <- reactive({
    sort(unique(data$Primary.Industry))
  })
  pickerInput("variable_mkt_cmp",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=mkt_list())
})

output$var_geo9 <- renderUI({
  data <- filter(data, Client.Name %in% input$variable_client_cmp)
  data <- filter(data, Primary.Industry %in% input$variable_mkt_cmp)
  
  geo_list <- reactive({
    sort(unique(data$Primary.Market.Unit))
  })
  pickerInput("variable_geo_cmp",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=geo_list())
})

########################################################
# Side bar filter for client #
output$var_client10 <- renderUI({
  client_list <- reactive({
    sort(unique(data$Client.Name))
  })
  pickerInput("variable_client_cmp_1",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=client_list())
})

output$var_mkt10 <- renderUI({
  data <- filter(data, Client.Name %in% input$variable_client_cmp_1)
  mkt_list <- reactive({
    sort(unique(data$Primary.Industry))
  })
  pickerInput("variable_mkt_cmp_1",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=mkt_list())
})

output$var_geo10 <- renderUI({
  data <- filter(data, Client.Name %in% input$variable_client_cmp_1)
  data <- filter(data, Primary.Industry %in% input$variable_mkt_cmp_1)
  
  geo_list <- reactive({
    sort(unique(data$Primary.Market.Unit))
  })
  pickerInput("variable_geo_cmp_1",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=geo_list())
})


###WC Main theme
cmpr_et_wc <- reactive({
  
  data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp)
  
  data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp)
  
  data <- filter(data, data$Client.Name %in%  input$variable_client_cmp)
  
  latest_2_qrtr1 <- filter(data, Timeframe %in% c("Q1-2022","Q4-2021"))
  latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q1-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))
  
  
  return(scatterplot_df_fltrd)
})

output$cmp_et_wc1 <- renderWordcloud2({
  wordcloud2(data = cmpr_et_wc(), size = 0.5, gridSize = 15, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})


transcpt_wc_click <- reactive ({
  var1 <- strsplit(as.character(input$trnspt_wc_clicked),split = ":")[[1]][1]
  return(var1)
})
####### Keywords wc


cmpr_et_wc2 <- reactive({
  
  if(is.null(input$trnspt_wc_clicked)){
  
  data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp)
  
  data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp)
  
  data <- filter(data, data$Client.Name %in%  input$variable_client_cmp)
  }
  
  else{
    data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp)
    
    data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp)
    
    data <- filter(data, data$Client.Name %in%  input$variable_client_cmp)
    
    data <- filter(data, data$Main.Theme %in% transcpt_wc_click())
    
  }
  
  latest_2_qrtr1 <- filter(data, Timeframe %in% c("Q1-2022","Q4-2021"))
  latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q1-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Keywords_DM)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Keywords_DM, score))
  
  
  return(scatterplot_df_fltrd)
  
})

output$cmp_et_wc2 <- renderWordcloud2({
  wordcloud2(data = cmpr_et_wc2(), size = 0.5,minSize = 0.2, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})
#############################################
##########Compare section


###WC Main theme
cmpr_et_wc3 <- reactive({
  
    data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp_1)
    
    data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp_1)
    
    data <- filter(data, data$Client.Name %in%  input$variable_client_cmp_1)
  
  latest_2_qrtr1 <- filter(data, Timeframe %in% c("Q1-2022","Q4-2021"))
  latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q1-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))
  
  
  return(scatterplot_df_fltrd)
})

output$cmp_et_wc3 <- renderWordcloud2({
  wordcloud2(data = cmpr_et_wc3(), size = 0.5, gridSize = 15, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})

####### Keywords wc
transcpt_wc_click2 <- reactive ({
  var1 <- strsplit(as.character(input$trnspt_wc_clicked2),split = ":")[[1]][1]
  return(var1)
})

cmpr_et_wc4 <- reactive({
  
  if(is.null(input$trnspt_wc_clicked2)){
    data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp_1)
    
    data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp_1)
    
    data <- filter(data, data$Client.Name %in%  input$variable_client_cmp_1)
    
  }
  
  else{
  data <- filter(data, data$Primary.Market.Unit %in%  input$variable_geo_cmp_1)
  
  data <- filter(data, data$Primary.Industry %in%  input$variable_mkt_cmp_1)
  
  data <- filter(data, data$Client.Name %in%  input$variable_client_cmp_1)
  
  data <- filter(data, data$Main.Theme %in% transcpt_wc_click2())
  }
  
  
  latest_2_qrtr1 <- filter(data, Timeframe %in% c("Q1-2022","Q4-2021"))
  latest_2_qrtr_FC<-dcast(latest_2_qrtr1, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(latest_2_qrtr1, Timeframe == "Q1-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Keywords_DM)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Keywords_DM, score))
  
  
  return(scatterplot_df_fltrd)
  
})

output$cmp_et_wc4 <- renderWordcloud2({
  wordcloud2(data = cmpr_et_wc4(), size = 0.5,minSize = 0.2, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})
###############Compare Social Media Transcript#####################################################

# Side bar filter for client #
output$var_client11 <- renderUI({
  client_list <- reactive({
    sort(unique(twitter_data$Client.Name))
  })
  pickerInput("variable_client_cmp3",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=client_list())
})

output$var_mkt11 <- renderUI({
  twitter_data <- filter(twitter_data, Client.Name %in% input$variable_client_cmp3)
  mkt_list <- reactive({
    sort(unique(twitter_data$Primary.Industry))
  })
  pickerInput("variable_mkt_cmp3",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=mkt_list())
})

output$var_geo11 <- renderUI({
  twitter_data <- filter(twitter_data, Client.Name %in% input$variable_client_cmp3)
  twitter_data <- filter(twitter_data, Primary.Industry %in% input$variable_mkt_cmp3)
  
  geo_list <- reactive({
    sort(unique(twitter_data$Primary.Market.Unit))
  })
  pickerInput("variable_geo_cmp3",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=geo_list())
})

#######################################################
# Side bar filter for client 
output$var_client12 <- renderUI({
  client_list <- reactive({
    sort(unique(twitter_data$Client.Name))
  })
  pickerInput("variable_client_cmp_2",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Company") , choices=client_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=client_list())
})

output$var_mkt12 <- renderUI({
  twitter_data <- filter(twitter_data, Client.Name %in% input$variable_client_cmp_2)
  mkt_list <- reactive({
    sort(unique(twitter_data$Primary.Industry))
  })
  pickerInput("variable_mkt_cmp_2",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Industry") , choices=mkt_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=mkt_list())
})

output$var_geo12 <- renderUI({
  twitter_data <- filter(twitter_data, Client.Name %in% input$variable_client_cmp_2)
  twitter_data <- filter(twitter_data, Primary.Industry %in% input$variable_mkt_cmp_2)
  
  geo_list <- reactive({
    sort(unique(twitter_data$Primary.Market.Unit))
  })
  pickerInput("variable_geo_cmp_2",width = '180px',label = div(style = "font-size:11px;font-family:Helvetica;", "Market Unit") , choices=geo_list(),
              options = list(`actions-box` = TRUE),multiple = T,
              selected=geo_list())
})


###WC Main theme
cmpr_sm_wc <- reactive({
  

  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp3)

  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp3)

  twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp3)

  twitter_data<- filter(twitter_data, Timeframe == "Q2-2022")
  

#latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
latest_2_qrtr_FC<-dcast(twitter_data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
latest_qtr <- subset(twitter_data, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                     by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
# quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
quarter_df1[is.na(quarter_df1)] <- 0

scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
  
  summarise(score = sum(Tfidf.Score),
            
            .groups = 'drop')

####Min max scaling
scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))

scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)

scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))


return(scatterplot_df_fltrd)
})

output$cmp_wc1 <- renderWordcloud2({
  wordcloud2(data = cmpr_sm_wc(), size = 0.5, gridSize = 15, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})

####### Keywords wc
sm_wc_clicke <- reactive ({
  var1 <- strsplit(as.character(input$sm_wc_clic),split = ":")[[1]][1]
  return(var1)
})

cmpr_sm_wc2 <- reactive({
  
  if(is.null(input$sm_wc_clic)){
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp3)
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp3)
  
  twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp3)
  
  }
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp3)
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp3)
    
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp3)
    
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in%  sm_wc_clicke())
    
    
  }
  #latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
  latest_2_qrtr_FC<-dcast(twitter_data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(twitter_data, Timeframe == "Q2-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Keywords_DM)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Keywords_DM, score))
  
  
  return(scatterplot_df_fltrd)
  
})

output$cmp_wc2 <- renderWordcloud2({
  wordcloud2(data = cmpr_sm_wc2(), size = 0.5,minSize = 0.2, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})
#############################################
##########Compare section

###WC Main theme
cmpr_sm_wc3 <- reactive({
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp_2)
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp_2)
  
  twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp_2)
  
  
  #latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
  latest_2_qrtr_FC<-dcast(twitter_data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(twitter_data, Timeframe == "Q2-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Main.Theme)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Main.Theme, score))
  
  
  return(scatterplot_df_fltrd)
})

output$cmp_wc3 <- renderWordcloud2({
  wordcloud2(data = cmpr_sm_wc3(), size = 0.5,gridSize = 15, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})

####### Keywords wc


sm_wc_click2 <- reactive ({
  var1 <- strsplit(as.character(input$sm_wc_clicked1),split = ":")[[1]][1]
  return(var1)
})


cmpr_sm_wc4 <- reactive({
  if(is.null(input$sm_wc_clicked1)){
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp_2)
  
  twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp_2)
  
  twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp_2)
  
  }
  else{
    twitter_data <- filter(twitter_data, twitter_data$Primary.Market.Unit %in%  input$variable_geo_cmp_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Primary.Industry %in%  input$variable_mkt_cmp_2)
    
    twitter_data <- filter(twitter_data, twitter_data$Client.Name %in%  input$variable_client_cmp_2)
    twitter_data <- filter(twitter_data, twitter_data$Main.Theme %in%  sm_wc_click2())
    
  }
  
  #latest_2_qrtr1 <- filter(twitter_data, Timeframe %in% c("Q1-2022","Q2-2022"))
  latest_2_qrtr_FC<-dcast(twitter_data, Primary.Market.Unit+ Primary.Industry+Client.Name+Main.Theme+Keywords + Keywords_DM ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  latest_qtr<- filter(twitter_data, Timeframe == "Q2-2022")
  latest_qtr <- subset(latest_qtr, select = c(Primary.Market.Unit,Primary.Industry,Client.Name, Main.Theme,Keywords,Keywords_DM,Tfidf.Score))
  latest_qtr<-aggregate(latest_qtr$Tfidf.Score, by=list(latest_qtr$Primary.Market.Unit,latest_qtr$Primary.Industry,latest_qtr$Client.Name,latest_qtr$Main.Theme,latest_qtr$Keywords,latest_qtr$Keywords_DM), FUN=sum)
  names(latest_qtr) <- c('Primary.Market.Unit','Primary.Industry','Client.Name','Main.Theme','Keywords','Keywords_DM','Tfidf.Score')
  quarter_df1 <- merge(latest_2_qrtr_FC,latest_qtr,by.x=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),
                       by.y=c("Primary.Market.Unit","Primary.Industry","Client.Name","Main.Theme","Keywords","Keywords_DM"),all=TRUE)
  # quarter_df <- quarter_df1%>% drop_na(Tfidf.Score)
  quarter_df1[is.na(quarter_df1)] <- 0
  
  scatterplot_df_fltrd = quarter_df1 %>% group_by(Keywords_DM)  %>%
    
    summarise(score = sum(Tfidf.Score),
              
              .groups = 'drop')
  
  ####Min max scaling
  scatterplot_df_fltrd$score <- (scatterplot_df_fltrd$score)/(max(scatterplot_df_fltrd$score))
  
  scatterplot_df_fltrd['score'] <- round(scatterplot_df_fltrd['score'], digits = 2)
  
  scatterplot_df_fltrd <- subset(scatterplot_df_fltrd, select = c(Keywords_DM, score))
  
  
  return(scatterplot_df_fltrd)
  
})

output$cmp_wc4 <- renderWordcloud2({
  wordcloud2(data = cmpr_sm_wc4(), size = 0.5,minSize = 0.2, gridSize = 10, shape = 'rectangle',rotateRatio = 0, color='#6C35CF') 
  
})

################detail for transcript

output$trnscpt_hm <- renderPlotly({
  sm_client <- dcast(data, Client.Name  ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  sm_tweets <- dcast(data, Keywords  ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=sum)
  

  Name <- c("Number of client analysed", "Number of Keywords matched")
  `Q1-2021` <- c(sum(sm_client$`Q1-2021` !=0), sum(sm_tweets$`Q1-2021` !=0))
  `Q2-2021` <- c(sum(sm_client$`Q2-2021` !=0), sum(sm_tweets$`Q2-2021` !=0))
  `Q3-2021` <- c(sum(sm_client$`Q3-2021` !=0), sum(sm_tweets$`Q3-2021` !=0))
  `Q4-2021` <- c(sum(sm_client$`Q4-2021` !=0), sum(sm_tweets$`Q4-2021` !=0))
  `Q1-2022` <- c(sum(sm_client$`Q1-2022` !=0), sum(sm_tweets$`Q1-2022` !=0))
  df <- data.frame(Name,`Q1-2021`,`Q2-2021`,`Q3-2021`,`Q4-2021`,`Q1-2022`)
  df <- melt(df) 
  
  gg <- ggplot(df, aes(variable,Name,fill = value)) + 
    geom_tile(color = "black",
              lwd = 1.5,
              linetype = 2) +
    scale_fill_gradient(low = "#EE9FFD", high = "purple")  +
    geom_text(aes(label = round(value, 1))) + 
    theme( axis.title.x = element_blank(),axis.title.y = element_blank()) + theme(plot.background  = element_rect(fill='white',
                                                                                                                  color='white'))
  ggplotly(gg) %>% 
    layout(plot_bgcolor='white')  %>%   hide_colorbar() %>%
    config(displayModeBar = F)
})


#######################detail for social media 

##heatmap
output$sm_hm <- renderPlotly({
sm_client <- dcast(twitter_data, Client.Name  ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=length)

sm_tweets <- dcast(twitter_data, Sentences  ~ Timeframe, value.var="Keyword.Frequency", fun.aggregate=length)

Name <- c("Number of client analysed", "Number of Tweets analysed")
`Q3-2021` <- c(sum(sm_client$`Q3-2021` !=0), sum(sm_tweets$`Q3-2021`))
`Q4-2021` <- c(sum(sm_client$`Q4-2021` !=0), sum(sm_tweets$`Q4-2021`))
`Q1-2022` <- c(sum(sm_client$`Q1-2022` !=0), sum(sm_tweets$`Q1-2022`))
`Q2-2022` <- c(sum(sm_client$`Q2-2022` !=0), sum(sm_tweets$`Q2-2022`))
df <- data.frame(Name,`Q3-2021`,`Q4-2021`,`Q1-2022`,`Q2-2022`)
df <- melt(df) 

gg <- ggplot(df, aes(variable,Name)) + geom_tile(aes(fill = value)) +
   scale_fill_gradient(low = "#EE9FFD", high = "purple")  +
  geom_text(aes(label = round(value, 1))) + 
  theme( axis.title.x = element_blank(),axis.title.y = element_blank()) + theme(plot.background  = element_rect(fill='white',
                                                                                                              color='white'))
ggplotly(gg) %>% 
  layout(plot_bgcolor='white')  %>% hide_colorbar() %>%
  config(displayModeBar = F) 
})

 } # function ends here
 # shiny server ends here

shinyApp(ui = ui, server = server)

