## Upload Library
library(devtools)
library(Rcpp)
library(rCharts)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reshape2)
library(dplyr)
library(readxl)
library(DT)

#######################################################################
### GLOBAL
## FOR CALCULATING LADD
age <-70
lifeyr <- c(1, 1/age, 2/age, 3/age, 5/age, 5/age, 5/age, 49/age)

## LIFESTAGES TABLE
efh <- c("Birth to < 1 month",
         "1 to < 3 months",
         "3 to < 6 months",
         "6 to < 12 months",
         "1 to < 2 years",
         "2 to < 3 years",
         "3 to < 6 years",
         "6 to < 11 years",
         "11 to < 16 years",
         "16 to < 21 years",
         "21 to < 70 years",
         "Birth to < 70 years")
expokids <- c("Young Infant","Young Infant","Young Infant","Young Infant",
              "Infant","Infant",
              "Young Child",
              "Child",
              "Young Youth",
              "Youth",
              "Adult",
              "Lifetime")
years <- c(1, 1, 1, 1, 2, 2, 3,5,5,5,49,70)

## PLOTTING COLOR PALLETTE 
# colortab <- c("Dust"="#FF6600","Soil"="#660000","Water"="#000099",
#               "Breast Milk"="#660066", "Dairy"="#CC0066","Fish"="#33CCCC",
#               "Meat"="#CC0000", "Grains"="#FFCC00","Vegetables"="#006600",
#               "Fruit"="#66CC33")
# bar graphs/nvd3
colortab1 <- c("#FF6600","#660000","#000099",
               "#660066","#CC0066","#CC0000",
               "#33CCCC","#006600","#66CC33","#FFCC00",
               "#FFFFCC","FF99CC","#00666","#808080") # extra colors
# pie charts/highcharts
colortab2 <-
  c("rgba(102, 0   , 102 ,0.85)", #breast milk
    "rgba(204, 0   , 102 ,0.85)", #dairy
    "rgba(255, 102 , 0   ,0.85)", #dust
    "rgba(51 , 204 , 204 ,0.85)", #fish
    "rgba(102, 204 , 51  ,0.85)", #fruit
    "rgba(255, 204 , 0   ,0.85)", #grains
    "rgba(204, 0   , 0   ,0.85)", #meat
    "rgba(102, 0   , 0   ,0.85)", #soil
    "rgba(0  , 102 , 0   ,0.85)", #veg
    "rgba(0  , 0   , 153 ,0.85)", #water
    
    "rgba(255,255,204,0.85)", # other
    "rgba(229,225,204,0.85)", # other
    "rgba(0  ,102,102,0.85)",# other
    "rgba(128,128,128,0.85)" # other
  )

#######################################################################
#######################################################################
#######################################################################
## UI - generates HTML web pg

# prevent double scroll bar
dashboardBody(tags$head(tags$style(
  HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
)))

ui <- dashboardPage(
  dashboardHeader(title="ExpoKids"),
  dashboardSidebar(
    fileInput(inputId="inFile",
              label="Upload an Excel file.",
              buttonLabel="Upload",
              placeholder="No file loaded.",
              accept=c(".xlsx")),
    sidebarMenu(
      menuItem("ExpoKids Lifestages", tabName= "tab0"),
      menuItem("Uploaded Table",   tabName  = "tab1"),
      menuItem("Lifestage Tables", tabName  = "tab2"),
      menuItem("Lifestage Graphs", tabName  = "graph1"),
      menuItem("Media Graphs",     tabName  = "graph2"),
      menuItem("Summary Graphs",   tabName  = "graph3")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "tab0",
              h2("ExpoKids Lifestages"),
              fluidRow(box(width = 9,
                           tableOutput(outputId = "lstab"),
                           footer="Note: The young infant and infant lifestages are the only lifestages to combine multiple age bins. 
                           The young infant lifestage is assumed to be 100% breastfed."
              ))
              ),
      
      tabItem(tabName = "tab1",
              h2("Uploaded ADD Table (mg/kg-d)"),
              fluidRow(box(width = 12,collapsible = T,
                           tableOutput(outputId = "first_tab"),
                           footer="Children under 1 year old are assumed to be 100% breastfed."
              ))
      ), 
      
      tabItem(tabName = "tab2",
              h2("Lifestage Tables"),
              fluidRow(box(width=10,title="Average Daily Dose (ADD) Table (mg/kg-d])",collapsible=T,
                           tableOutput(outputId="add_tab"),
                           footer="The young infant lifestage is assumed to be 100% breastfed.")),
              fluidRow(box(width=9,title="Lifetime Average Daily Dose (LADD) Table (mg/kg-d)",collapsible=T,
                           tableOutput(outputId="ladd_tab"),
                           footer="The young infant lifestage is assumed to be 100% breastfed."))
      ),
      
      tabItem(tabName = "graph1",
              h2("Lifestage Graphs"),
              fluidRow(box(showOutput("p_ADD","nvd3"),
                           title="ADD by Lifestage Graph",width =9,collapsible=T)),
              fluidRow(box(showOutput("p_LADD","nvd3"),
                           title="LADD by Lifestage Graph",width=9,collapsible=T)),
              fluidRow(box(showOutput("p_perlife","Highcharts"),
                           title="ADD Cumulative Percent by Lifestage",height=2850,width=9,collapsible=T))
      ),
      
      tabItem(tabName = "graph2",
              fluidRow(
                h2("Media Graphs",align="center"),
                box(width=3,
                    checkboxGroupInput("media2", label=h3("Media")),
                    actionButton("select", "Select All", icon = icon("ok-sign", lib = "glyphicon")),
                    actionButton("deselect","Deselect All",icon = icon("ok-circle", lib = "glyphicon")),
                    actionButton("button", "Show Graphs", style = "color: white; background-color:#0a85ab")
                )),
              fluidRow(
                tabBox(width = "12", height = "4300",
                       tabPanel("ADD", 
                                textOutput("text"),textOutput("text2"),textOutput("text3"),
                                uiOutput("p_addmedia")
                       ),
                       tabPanel("LADD",uiOutput("p_laddmedia"))
                ))
      ), 
      
      tabItem(tabName="graph3",
              fluidRow(h2("Summary Graphs",align="center")),
              fluidRow(box(showOutput("p_sum","nvd3"),title="ADD Graph",width=9,collapsible=T)),
              fluidRow(box(showOutput("p_persum","nvd3"),title="Cumulative Percent",width=9,collapsible=T))
      )
      )))