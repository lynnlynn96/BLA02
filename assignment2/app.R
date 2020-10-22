#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(caret)
library(randomForest)

#load the trained models into cls_models and reg_models
courses = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG")
cls_models = vector('list', 7)
reg_models = vector('list', 2)
for (course in courses)
{
    cls_filename = gsub(' ', '', paste('cls_', course, '.rds'))
    print(cls_filename)
    classificationModel = readRDS(file.path("./", cls_filename))
    cls_models[[course]] = classificationModel
    if(course %in% c("CCC", "DDD"))
    {
        reg_filename = gsub(' ', '', paste('reg_', course, '.rds'))
        regressionModel = readRDS(file.path("./", reg_filename))
        reg_models[[course]] = regressionModel
    }
}
ui <- dashboardPage(skin="yellow",
    dashboardHeader(title = "Prediction Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            selectInput("course", "Course", choices = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"), selected = "DDD"),
            h3("VLE Data"),
                # add active days per week input
                numericInput("active_days_per_week", "Active Days Per Week",  min = 0, max = 7, value = 3),
                numericInput("average_daily_clicks", "Average Daily Clicks",  value = 25),
                numericInput("average_elements", "Average Daily Elements",  value = 25),
            h3("Assessment Data"),
                sliderInput("avgScore", "Average Score",  min = 0, max = 100, value = 50),
                numericInput("delivered", "Deliverd Assessments", value = 5),
                numericInput("sumDelays", "Delayed Days",  value = 0)
        )
    ),
    dashboardBody(
            # First tab content
            tabItem(tabName = "classification",
                    fluidRow(
                        box(title="Student Info",
                            radioButtons("gender", "Gender",
                                         choices = list("Male" = "M", "Female" = "F"),selected = "M"),
                            selectInput("region", "Region",
                                        choices = list("East Anglian Region"="East Anglian Region",
                                                       "Yorkshire Region"="Yorkshire Region",
                                                       "East Midlands Region"="East Midlands Region",
                                                       "South East Region"="South East Region",
                                                       "North Western Region"= "North Western Region",
                                                       "Scotland"="Scotland",
                                                       "South West Region"="South West Region",
                                                       "West Midlands Region"="West Midlands Region",
                                                       "Wales"="Wales",
                                                       "Ireland"="Ireland",             
                                                       "South Region"="South Region",
                                                       "London Region"="London Region",
                                                       "North Region"="North Region"),selected = "London Region"),
                            selectInput("highest_education", "Highest Level of Education",
                                        choices =list("A Level or Equivalent",
                                                      "Lower Than A Level",
                                                      "HE Qualification",
                                                      "Post Graduate Qualification",
                                                      "No Formal quals"
                                        ), selected="HE Qualification"),
                            selectInput("imd_band", "IMD Band",
                                        choices =list("0-10%", 
                                                      "20-30%",
                                                      "30-40%",
                                                      "40-50%",
                                                      "50-60%",
                                                      "60-70%",
                                                      "70-80%",
                                                      "80-90%",
                                                      "90-100%"
                                        ), selected="50-60%"),
                            selectInput("age_band", "Age Band",
                                        choices =list("0-35",
                                                      "35-55",
                                                      "55<="
                                        ), selected="0-35"),
                            numericInput("num_of_prev_attempts", "Previous Attempts", value = 0),
                            numericInput("studied_credits", "Studied Credits", value = 60),
                            radioButtons("disability", "Disability",
                                         choices = list("Yes" = "Y", "No" = "N"),selected = "N")
                        ),
                        valueBoxOutput("classificationPrediction"),
                        # put two pages together
                        valueBoxOutput("regressionPrediction")  
                    ),
                ),
    )
)

server <- function(input, output) {
    # Define coursename so that I can retrieve selected course using coursename()
    coursename <- reactive({
        coursename <- input$course
    })
    output$classificationPrediction <- renderValueBox({
        dataset=data.frame("active_pct"= input$active_days_per_week/7,# active_days_per_week / 7
                           "average_daily_clicks"=input$average_daily_clicks,
                           "average_elements" = input$average_elements,
                           "avgScore" = input$avgScore,
                           "delivered" =input$delivered,
                           "sumDelays" = input$sumDelays,
                           "gender"= input$gender,
                           "region"= input$region,
                           "highest_education"= input$highest_education,
                           "imd_band"=input$imd_band,
                           "age_band"=input$age_band,
                           "num_of_prev_attempts"=input$num_of_prev_attempts,
                           "studied_credits"=input$studied_credits,
                           "disability"=input$disability,
                           "final_result"=NA
                           
        )
        print(coursename())
        # Select the classification model for selected course
        cls_model = cls_models[[coursename()]]
        # Predict pass or fail
        predictedValue=predict(cls_model,dataset)
        print(predictedValue)
        valueBox(
            ifelse(predictedValue[1]=="Pass","Pass","Fail"),"Prediction", icon = icon(ifelse(predictedValue[1]=="Pass","check","exclamation")),
            color = ifelse(predictedValue[1]=="Pass","green","red")
        )
    })
    
    output$regressionPrediction <- renderValueBox({
        datasetRegression=data.frame("active_pct"= input$active_days_per_week/7,
                                     "average_daily_clicks"=input$average_daily_clicks,
                                     "average_elements" = input$average_elements,
                                     "avgScore" = input$avgScore,
                                     "delivered" =input$delivered,
                                     "sumDelays" = input$sumDelays,
                                     "score"=NA
                           
        )
        # Use regression models to predict final grade if "CCC" or "DDD" is selected
        if(coursename() %in% c("CCC", "DDD"))
        {
            reg_model = reg_models[[coursename()]]
            value=predict(reg_model,datasetRegression)
            valueBox(
                format(value[1], digits=2, nsmall=2),"Final Grade", icon = icon(ifelse(value[1]>70,"check",ifelse(value[1]>50,"exclamation","times"))),
                color = ifelse(value[1]>70,"green",ifelse(value>50,"yellow","red"))
            )
        }
        # Display NA for final grade prediction if other courses are selected
        else{
            value = NA # Other courses don't have final scores avaliable
            valueBox(
                value,"Final Grade", icon = icon("exclamation"),
                color = "yellow"
            )
        }
        
    })
}

shinyApp(ui, server)