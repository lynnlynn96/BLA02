library(readr)
library(tidyverse)
# Read data
assessments <- read_csv("data/assessments.csv")
courses <- read_csv("data/courses.csv")
studentAssessment <- read_csv("data/studentAssessment.csv")
studentInfo <- read_csv("data/studentInfo.csv")
studentRegistration <- read_csv("data/studentRegistration.csv")
studentVle <- read_csv("data/studentVle.csv")
vle <- read_csv("data/vle.csv")
# Merge student activity data with course general info
studentVleWithCourseLength <- inner_join(studentVle, 
                                         courses, 
                                         by=c('code_module' = 'code_module',
                                              'code_presentation' = 'code_presentation'))
# course -> pass/not pass
getFinalState<-function(course)
{
  studentInfo%>%
    filter(code_module==course) %>%
    filter(final_result!="Withdrawn") %>%
    mutate(final_result=ifelse(final_result=="Distinction","Pass",final_result))%>%
    mutate(final_result=as.factor(final_result))%>%
    select("id_student", "final_result")
}
# course -> dropout 1/0
getDropout<-function(course)
{
  studentInfo%>%
    filter(code_module==course) %>%
    mutate(dropout=ifelse(final_result=="Withdrawn",1,0))%>%
    mutate(dropout=as.factor(dropout))%>%
    select("id_student", "dropout")
}
# course -> studentid: score
getFinalGrade<-function(course)
{
  finalExam<-assessments%>%
    filter(code_module==course)%>%
    filter(assessment_type=="Exam")
  inner_join(studentAssessment,finalExam, by = c("id_assessment" = "id_assessment"))%>%
    select("id_student","score")
}
getAssessmentPredictors<-function(course,days)
{
  courseAssessments<-assessments%>%
    filter(code_module==course)%>%
    filter(date<days)
  studentAssessment%>%
    filter(id_assessment %in% courseAssessments$id_assessment)%>%
    group_by(id_student)%>%
    summarise(avgScore=mean(score),delivered=n())
}

getLateAssessments<-function(course,days)
{
  courseAssessments<-assessments%>% 
    filter(code_module==course)%>%
    filter(date<days)
  fullAssessments<-inner_join(studentAssessment,courseAssessments,by = c("id_assessment" = "id_assessment"))
  fullAssessments%>%
    mutate(delay=ifelse(date<date_submitted,1,0))%>%
    group_by(id_student)%>%
    summarise(sumDelays=sum(delay))
}

getClickInfo<-function(course,days)
{
  studentVle%>%
    filter(code_module==course)%>%
    filter(date<days+1)%>%
    group_by(id_student,date)%>%
    summarise(daily_clicks=sum(sum_click),daily_elements=n())%>%
    group_by(id_student)%>%
    summarise(total_clicks=sum(daily_clicks),total_elements=sum(daily_elements),active_days=n())%>%
    mutate(average_daily_clicks=total_clicks/active_days,average_elements=total_elements/active_days)
}
getStudentActivity<-function(course)
{
  # Mutate vle data into 3 predictors
  # 1. active percent(active_pct): active_days / module_presentation_length
  # 2. average_daliy_clicks
  # 3. average_elements
  studentVleWithCourseLength%>%
    filter((code_module==course))%>%
    group_by(id_student, date, module_presentation_length)%>%
    summarise(daily_clicks=sum(sum_click),daily_elements=n())%>%
    group_by(id_student, module_presentation_length)%>%
    summarise(total_clicks=sum(daily_clicks),total_elements=sum(daily_elements),active_days=n())%>%
    mutate(active_pct=active_days/module_presentation_length,average_daily_clicks=total_clicks/active_days,average_elements=total_elements/active_days)%>%
    # Remove other columns
    select(-c("total_clicks", "total_elements", "active_days", "module_presentation_length"))
}

# Train all the courses
for (course in c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"))
{
    # Set days cutoff to be 100
    days=100
    
    # Get activity predictors
    activities<-getStudentActivity(course)
    # Get assessment predictors
    assessInfo<-getAssessmentPredictors(course,days)
    # Get lateness predictors
    latenessInfo<-getLateAssessments(course,days)
    temp1<-merge(activities,assessInfo,by="id_student")
    predictors<-merge(temp1,latenessInfo,by="id_student")
    predictors<-left_join(predictors,studentInfo,by="id_student")%>%
      select(-c("final_result", "code_module", "code_presentation"))
    
    finalState<-getFinalState(course)
    
    datasetState<-merge(predictors,finalState,by="id_student")%>%
      select(!"id_student")
    
    
    library(caret)
    # set.seed(123)
    # 
    # index <- createDataPartition(datasetState$final_result, p = .7, list = FALSE, times = 1)
    # 
    # trainState <- datasetState[ index,]
    # testState  <- datasetState[-index,]
    # 
    # fitControl <- trainControl(method = "cv", number = 10)
    preProcess = c("center", "scale")
    # modelRF <- train(final_result~., data=trainState, method="rf", trControl=fitControl, preProcess=preProcess, na.action=na.omit)
    # print(modelRF)
    # predict_unseen <-predict(modelRF, testState, na.action = na.omit)
    # confusionMatrix(predict_unseen, na.omit(testState)$final_result)
    # 
    # library(randomForest)
    # varImpPlot(modelRF$finalModel)
    
    # Train the final model with all data 
    modelRFFinal <- train(final_result~., data=datasetState, method="rf", preProcess=preProcess, na.action=na.omit)
    
    # Save cls_<course>.rds, for example cls_AAA.rds
    cls_filename = gsub(' ', '', paste('cls_', course, '.rds'))
    saveRDS(modelRFFinal, file.path("./", cls_filename))
    
    # Only "CCC" and "DDD" have final grade, so only train regression models on these two courses
    if (course %in% c("CCC", "DDD")){
      predictors<-merge(temp1,latenessInfo,by="id_student")
      finalGrade<-getFinalGrade(course)
      datasetGrade<-merge(predictors, finalGrade, by="id_student")%>%
        select(!"id_student")
      # Train final regression model
      modelLMFinal <- train(score ~ ., data = datasetGrade, method = "lm", preProcess=preProcess, na.action=na.omit)
      # Save reg_<course>.rds, for example reg_CCC.rds
      reg_filename = gsub(' ', '', paste('reg_', course, '.rds'))
      saveRDS(modelLMFinal, file.path("./", reg_filename))
    }
}

