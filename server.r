###This is a Shiny App based on the Chicago food prediction model code
#Please see the original code for comments on functionality
#https://github.com/Chicago/food-inspections-evaluation/blob/master/CODE/30_glmnet_model.R
#https://github.com/Chicago/food-inspections-evaluation/blob/master/REPORTS/Metric_Development.Rmd


shinyServer(function(input, output, session) {

###Variable selections for the model
  xmat1 <- reactive({
      if (!is.null(input$variables)){
        Inspection_ID <- dat$Inspection_ID
        temp <- as.data.frame(Inspection_ID)
        temp$criticalFound <-dat$criticalFound}
      if ("Inspector" %in% input$variables) {temp$Inspector_Assigned <- dat$Inspector_Assigned}
        if ("Time_since_last" %in% input$variables) {temp$timeSinceLast <- dat$timeSinceLast}
        if ("Consumption_on_premises_incidental_activity" %in% input$variables) {temp$consumption_on_premises_incidental_activity <- dat$consumption_on_premises_incidental_activity}
        if ("Tobacco_retail_over_counter" %in% input$variables) {temp$tobacco_retail_over_counter <- dat$tobacco_retail_over_counter}
       # if ("Facility_Type" %in% input$variables) {temp$Facility_Type <- dat$Facility_Type}
        if ("TemperatureMax" %in% input$variables) {temp$temperatureMax <- dat$temperatureMax}
        if ("Past_critical" %in% input$variables) {temp$pastCritical = pmin(dat$pastCritical, 1)}
        if ("Past_serious" %in% input$variables) {temp$pastSerious = pmin(dat$pastSerious, 1)}
        if ("Burglary" %in% input$variables) {temp$heat_burglary = pmin(dat$heat_burglary, 70)}
        if ("Sanitation" %in% input$variables) {temp$heat_sanitation = pmin(dat$heat_sanitation, 70)}
        if ("Garbage" %in% input$variables) {temp$heat_garbage = pmin(dat$heat_garbage, 50)}
        if ("Age_at_inspection" %in% input$variables) {temp$ageAtInspection = ifelse(dat$ageAtInspection > 4, 1L, 0L)}
    return(data.table(temp))
    })
  
###Variable selection for the EDA section
  xmat2 <- reactive({
    if (!is.null(input$variables2)){
      Inspection_ID <- dat$Inspection_ID
      temp <- as.data.frame(Inspection_ID)
      temp$criticalFound <-dat$criticalFound}
    if ("Inspector" %in% input$variables2) {temp$Inspector_Assigned <- dat$Inspector_Assigned}
    if ("Time_since_last" %in% input$variables2) {temp$timeSinceLast <- dat$timeSinceLast}
    if ("Consumption_on_premises_incidental_activity" %in% input$variables2) {temp$consumption_on_premises_incidental_activity <- dat$consumption_on_premises_incidental_activity}
    if ("Tobacco_retail_over_counter" %in% input$variables2) {temp$tobacco_retail_over_counter <- dat$tobacco_retail_over_counter}
    # if ("Facility_Type" %in% input$variables) {temp$Facility_Type <- dat$Facility_Type}
    if ("TemperatureMax" %in% input$variables2) {temp$temperatureMax <- dat$temperatureMax}
    if ("Past_critical" %in% input$variables2) {temp$pastCritical = pmin(dat$pastCritical, 1)}
    if ("Past_serious" %in% input$variables2) {temp$pastSerious = pmin(dat$pastSerious, 1)}
    if ("Burglary" %in% input$variables2) {temp$heat_burglary = pmin(dat$heat_burglary, 70)}
    if ("Sanitation" %in% input$variables2) {temp$heat_sanitation = pmin(dat$heat_sanitation, 70)}
    if ("Garbage" %in% input$variables2) {temp$heat_garbage = pmin(dat$heat_garbage, 50)}
    if ("Age_at_inspection" %in% input$variables2) {temp$ageAtInspection = ifelse(dat$ageAtInspection > 4, 1L, 0L)}
    return(data.table(temp))
  })

##Data
mm1 <- reactive({
    xmat <- xmat1()
    test2 <- subset(xmat,select = -c(Inspection_ID))
    m2 <- model.matrix(criticalFound ~ . -1, data=test2)
    mm <- as.data.table(m2)
    mm})

##Models  
model1 <- reactive ({
  mm<-mm1()
  xmat<-xmat1()
  if (input$Model == 1) {
    model <- glmnet(x = as.matrix(mm[iiTrain]),
                  y = xmat[iiTrain, criticalFound],
                  family = "binomial", 
                  alpha = 0)}
  if (input$Model == 2) {
    model <- randomForest(x = as.matrix(mm[iiTrain]),
                    y = as.factor(xmat[iiTrain, criticalFound]),importance=TRUE)
  }
  if (input$Model == 3) {
    train <- (mm[iiTrain])
    model <- glm(as.factor(xmat[iiTrain, criticalFound]) ~ .
                 ,family=binomial(link='logit'),data=train)
  }
  return(model)
})  

##Predictions
pred1 <- reactive ({
  mm<-mm1()
  xmat<-xmat1()
  model <- model1()
  lam <- 0.008
  if (input$Model == 1) {dat$pred <- predict(model, newx=as.matrix(mm), 
                                                 s=lam, 
                                                 type="response")[,1]}
  if (input$Model == 2) {dat$pred <- predict(model, as.matrix(mm),type="prob")[,2]}
  if (input$Model == 3) {dat$pred <- predict(model, mm,type="response")}
  dat$pred
})  



output$plot_pred<- renderPlot({
  mm<-mm1()
  xmat<-xmat1()
  model <- model1()
  dat$pred <- pred1()
  ## Calculate confusion matrix values 
  print (calculate_confusion_values(actual = xmat[iiTest, criticalFound],
                             expected = dat[iiTest, pred], 
                             r = .25))
  
  ## Calculate matrix of confusion matrix values
  confusion_values_test <- t(sapply(seq(0, 1 ,.01), 
                                    calculate_confusion_values,
                                    actual = xmat[iiTest, criticalFound],
                                    expected = dat[iiTest, pred]))
  dat
  ggplot(reshape2::melt(as.data.table(confusion_values_test), 
                        id.vars="r")) + 
    aes(x=r, y=value, colour=variable) + geom_line() + 
    ggtitle("Confusion Values") +
    geom_hline(yintercept = c(0,1))
})

output$plot_gini<- renderPlot({
  mm<-mm1()
  xmat<-xmat1()
  model <- model1()
  dat$pred <- pred1()
  print ("gini")
  print (dat[iiTest, gini(pred, criticalFound, plot=TRUE)])})

output$table1<- renderPrint({
  mm<-mm1()
  xmat<-xmat1()
  model <- model1()
  dat$pred <- pred1()
  
#   ## TEST PERIOD: Date range
#   dat[iiTest, range(Inspection_Date)]
#   ## TEST PERIOD: Total inspections --1637
#   dat[iiTest, .N]
#   ## TEST PERIOD: Critical found --361
#   dat[iiTest, sum(criticalCount)]
#   ## TEST PERIOD: Inspections with any critical violations --258
#   dat[iiTest, sum(criticalFound)]
#   
  ## Subset test period
  datTest <- dat[iiTest]
  ## Identify first period
  datTest[ , period := ifelse(Inspection_Date < median(Inspection_Date),1,2)]
  datTest[, .N, keyby=list(period)]
  ## 1 - 810   2 - 827
  datTest[, .N, keyby=list(Inspection_Date, period)]
  ## Identify top half of scores (which would have been the first period)
  datTest[ , period_modeled := ifelse(pred > median(pred), 1, 2)]
  
  datTest[period == 1, sum(criticalFound)] #141
  datTest[period_modeled == 1, sum(criticalFound)] #178
  
  print (datTest[, list(.N, Violations = sum(criticalFound)), keyby=list(period)])
  print (datTest[, list(.N, Violations = sum(criticalFound)), keyby=list(period_modeled)])

})

output$plot_AUC<- renderPlot({
  mm<-mm1()
  xmat<-xmat1()
  model <- model1()
  dat$pred <- pred1()
  datTest <- dat[iiTest]
ROCRpred = prediction(datTest$pred, datTest$criticalFound) 
AUC <- as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr", "fpr") 
plot(perf,colorize=T,print.cutoffs.at=seq(0,1,by=0.2),main=paste("AUC: ",as.numeric(performance(ROCRpred, "auc")@y.values)))
})

# output$plot_crit_viol_rate<- renderPlot({
#   ## Subset test period
#   dat$glm_pred <- pred1()
#   datTest <- dat[iiTest]
#   ## Identify first period
#   datTest[ , period := ifelse(Inspection_Date < median(Inspection_Date),1,2)]
#   #datTest[, .N, keyby=list(period)]
#   ## 1 - 810   2 - 827
#   #datTest[, .N, keyby=list(Inspection_Date, period)]
#   ## Identify top half of scores (which would have been the first period)
#   datTest[ , period_modeled := ifelse(glm_pred > median(glm_pred), 1, 2)]
#   crit_viol_rate <<- data.table(
#     Regime = c("Business As Usual", 
#                "Data Driven"),
#     values = c(datTest[period==1, sum(criticalFound)/.N],
#                datTest[period_modeled==1, sum(criticalFound)/.N]))
#   
#   ggplot(crit_viol_rate) + 
#     aes(x = Regime, y = values, fill=Regime) + 
#     labs(title=paste0('Percentage of inspections resulting in a critical violation\n',
#                       'during Period 1\n') ) +
#     geom_bar(stat = "identity") + 
#     # guides(fill=FALSE) +
#     scale_y_continuous(labels = percent) +
#     xlab("") + ylab("")
# 
# })
  
  
  
output$plot_crit_viol_cumulative<- renderPlot({
  dat$glm_pred <- pred1()
  datTest <- dat[iiTest]
  datTest[ , period := ifelse(Inspection_Date < median(Inspection_Date),1,2)]
  datTest[ , period_modeled := ifelse(glm_pred > median(glm_pred), 1, 2)]
  crit_viol_cumulative <<- data.table(
    Regime = c("Business As Usual",
               "Data Driven"),
    values = c(datTest[period==1, sum(criticalFound)] / datTest[ , sum(criticalFound)],
               datTest[period_modeled==1, sum(criticalFound)] / datTest[ , sum(criticalFound)]))
  
  comp <- cbind(
    datTest[i = order(Inspection_Date), 
            j = list(Inspection_ID_BAU   = Inspection_ID,
                     Inspection_Date   = Inspection_Date,
                     criticalFound_BAU     = criticalFound)],
    datTest[i = order(-glm_pred), 
            j = list(Inspection_ID_Model   = Inspection_ID,
                     criticalFound_Model   = criticalFound)])
  date_comp <- merge(
    x = comp[i = TRUE,
             j = list (criticalFound_BAU,
                       date_bau = Inspection_Date,
                       id = Inspection_ID_BAU)],
    y = comp[i = TRUE,
             j = list (criticalFound_Model,
                       date_model = Inspection_Date,
                       id = Inspection_ID_Model)],
    by = "id")
  
  daysooner <- as.numeric(date_comp[criticalFound_Model==1,
                               -(date_model - date_bau)])
  daysooneravg <- sum(daysooner)/length(daysooner)
  daystext <- paste("Days sooner a violation would be found: ",daysooneravg)
 # print ("Days sooner a violation would be found")
  #print ( sum(test)/length(test)
  
  ggplot(crit_viol_cumulative) + 
    aes(x = factor(Regime), y = values, fill=Regime) + 
    labs(title=paste0('Critical violations found\n',
                      daystext)) +
    
    geom_bar(stat = "identity") +
    # guides(fill=FALSE) +
    scale_y_continuous(labels = percent) +
    xlab("") + ylab("") + expand_limits(y = 1)

}) 

##EDA
output$mjs1 <- renderPlotly({
  mm <- xmat2()
  mm <- mm[iiTrain,]
  mm <- data.frame(mm)
  p <- plot_ly(mm,x=mm[,3], type = "histogram")
  p
})


  output$mytable1 <- DT::renderDataTable({
   xmat1()
  }, options = list(lengthMenu = c(50, 30, 50), pageLength = 50,searching=FALSE))
  
  ##Variable Importance
  output$mytable2 <- DT::renderDataTable({
      model <- model1()
      if (input$Model == 1) {
      w.lam <- 100
      lam <- model$lambda[w.lam]
      coef <- model$beta[,w.lam]
      temp <- as.data.frame(coef)}
      if (input$Model == 2) {
        temp <- data.frame(variable = names(model$importance[,1]), importance = model$importance[,1])
        }
      #  temp <- importance((model),type=1)
      #  print(temp)
      #   }
      temp
  }, options = list(lengthMenu = c(50, 30, 50), pageLength = 50,searching=FALSE))
 
##VI plot 
  output$dotplot <- renderPlot({
    if (input$Model == 2) {
      plot(model1(), log="y")
      varImpPlot(model1())
     # dotPlot(varImp(model1()$fit), main="Dotplot of variable importance values")
    }
    
  })
  
})