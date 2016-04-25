library(shinythemes)
var.name =c("Inspector","Past_serious","Past_critical","Time_since_last","Age_at_inspection","Consumption_on_premises_incidental_activity",
            "Tobacco_retail_over_counter","TemperatureMax","Burglary","Sanitation","Garbage")

shinyUI(fluidPage(theme = shinytheme("spacelab"),
                  # tags$head(includeScript("googleanalytics.js")),
  navbarPage("Food Inspection Analysis",tabPanel("App",
  sidebarPanel(
    selectizeInput(
      'variables', 'Variables', choices = var.name, multiple = TRUE, selected = var.name),
  selectInput('Model',label="Model",choices=list("Chicago-GLM"=1,"Random Forest"=2,"Logistic Regression"=3),selected = 1)),
  mainPanel(
    plotOutput('plot_crit_viol_cumulative'),
    plotOutput('plot_gini'),
    plotOutput('plot_AUC'),
    plotOutput('plot_pred')
  )),
tabPanel("About" ,
         fluidRow(
           column(10,includeMarkdown("introduction.md"))
         )),
tabPanel("Data" ,
         fluidRow(
           DT::dataTableOutput("mytable1") 
         )),
tabPanel("EDA" ,
         sidebarPanel(
           selectizeInput(
             'variables2', 'Variables', choices = var.name, multiple = FALSE, selected = var.name)),
         mainPanel(
           helpText("This page shows variable distribution as used in the model"),
           plotlyOutput('mjs1')
         )),
tabPanel("Variable Importance" ,
         fluidRow(
           helpText("This page show variable importance for the chosen model"),
           DT::dataTableOutput("mytable2"),
           plotOutput("dotplot")

         ))

) ))