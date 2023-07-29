library(shiny)
library(shinythemes)
library(readr)
library(psych)
library(zoo)
library(lmtest)
library(DAAG)

#-------- INSTALLING GITHUB PACKAGES --------#

# 1. install.packages("devtools")
# 2. library(devtools)
# 3. install_github("JohnHendrickx/Perturb")
library(perturb)

library(fastDummies)
library(MASS)

#raw data
data <- read_table("../data.txt")
names(data) <- c("y","x1","x2","x3","x4")
data$x4 <- as.factor(data$x4)


#data dropped outlier
q1 <- quantile(data$y,0.25)
q3 <- quantile(data$y,0.75)
qrange <- IQR(data$y)
data_no_outlier <- subset(data, data$y > (q1 - 1.5*qrange) & data$y < (q3 + 1.5*qrange))

lModel<-lm(data_no_outlier$y~data_no_outlier$x1+data_no_outlier$x2+data_no_outlier$x3+data_no_outlier$x4)
diag <- ls.diag(lModel)

bp <- bptest(lModel)
dw<- dwtest(lModel)


#Prediction data
new_data <- data_no_outlier
names(new_data) <- c("Y","X1","X2","X3","X4")
attach(new_data)

dums <- dummy_columns(new_data$X4)
new_x42<- dums$.data_2
new_x43<- dums$.data_3

lModel_2 <- lm(new_data$Y~X1+X2+X3+new_x42+new_x43)


#Ridge
ridgeReg <- lm.ridge(data_no_outlier$y~data_no_outlier$x1+data_no_outlier$x2+data_no_outlier$x3+data_no_outlier$x4 ,lambda = seq(0,1,0.05))


ui <- fluidPage(
  theme = shinytheme('cosmo'),
  navbarPage('Mini Regression Analysis',
             tabPanel('Looking Data',
                      sidebarPanel(
                        tags$h3("Data's Preview"),
                        selectInput('selectid','Choose',
                                    choices = c("Summary Statistics" = "summary",
                                                "Pairs Plot" = "pairs",
                                                "Preview Data"="pre_data"),
                                    selected = "pre_data")
                      ),
                      mainPanel(
                        h3("Output"),
                        verbatimTextOutput('summs'),
                        tableOutput("head_data"),
                        plotOutput('pairs')
                      )
             ),
             tabPanel('Normality',
                      sidebarPanel(
                        tags$h3("Setups:"),
                        sliderInput('alpha','Set your alpha',0.01,0.5,0.07),
                        checkboxInput('checkNormalGraph','Show Normality Graph'),
                        actionButton("submitbutton", "Comment", class = "btn btn-primary"),
                        tags$hr(style="border-color: black;"),
                        actionButton('outlierButton','Drop the Outliers', icon = icon("fa-solid fa-bath"))
                      ),
                      mainPanel(
                        tags$h3("Results"),
                        verbatimTextOutput('shapiroResult'),
                        textOutput('commentResult'),
                        tags$hr(style = "border-color: black"),
                        plotOutput('normalityGraph')
                      )
             ),
             tabPanel("Residuals",
                      sidebarPanel(
                        tags$h3("Choices:"),
                        selectInput("residuals_selection", "Residuals Selections:",
                                    choices = list("Show Estimate Model"="model",
                                                   "Show Regression output"="reg_output",
                                                   "Correlations"="correlation",
                                                   "Standart Errors"="std.err")),
                        checkboxInput("cook_plot","Influential Obs. Plot",value = TRUE),
                        tags$hr(style="border-color: black"),
                        checkboxGroupInput("diags","Residuals Dataset",
                                           choices = c("Standartized Residuals"="Standartized Res.",
                                                       "Studentized Residuals"="Studentized Res.",
                                                       "Cooks Distance"="Cooks Dist."))
                      ),
                      mainPanel(
                        h3("Results"),
                        verbatimTextOutput('linear_model'),
                        tableOutput('correlations'),
                        verbatimTextOutput('std_err'),
                        plotOutput('inf_obs'),
                        dataTableOutput('diag_table')
                      )
             ),
             tabPanel("Varying Variance",
                      sidebarPanel(
                        tags$h3("Inputs:"),
                        numericInput("alpha_var","Select your alpha",min = 0.01, max = 0.5, value = 0.05, step=0.01),
                        checkboxInput("self_rs",label = "Examine Self Reliationship"),
                        checkboxInput("multi_coll",label = "Examine Multi Collinearity"),
                        checkboxInput("plot_var","Show Plot")
                      ),
                      mainPanel(tags$h3("Ouputs"),
                                verbatimTextOutput("varying_bp"),
                                tags$hr(style= "border-color: black"),
                                verbatimTextOutput("self_estem"),
                                verbatimTextOutput("collinearity"),
                                plotOutput("varying_plot")
                      )
             ),
             tabPanel("Prediction & Ridge",
                      sidebarPanel(
                        tags$h3("Settings:"),
                        sliderInput("predict_alpha","Tolerance", min=0.01, max=0.1, step=0.01, value=0.05),
                        numericInput("predict_x1","X1 value:", min=0, max=100, step = 0.001, value=5.95),
                        numericInput("predict_x2","X2 value:", min=-10, max=10, step = 0.001, value=1.97),
                        numericInput("predict_x3","X3 value:", min=0, max=10, step = 0.001, value=3.06),
                        selectInput("predict_select", "Choose your category",
                                    c("Sunny"="first","Rainy"="second","Snowy"="third"),
                                    selected = "first"),
                        actionButton("predict_submit","Predict"),
                        tags$hr(style="border-color: black"),
                        checkboxInput("ridge","Show Ridge Chart")
                      ),
                      mainPanel(
                        tags$h3("Status/Result"),
                        verbatimTextOutput("status"),
                        tags$hr(style = "border-color: black"),
                        tableOutput("prediction"),
                        textOutput("prediction_comment"),
                        plotOutput("ridgeReg")
                      )
             )
  )
)

server <- function(input, output, session) {
  
  # Preview PAGE 1
  output$summs <- renderPrint({
    if(input$selectid == 'summary'){
      summary(data)
    }
  })
  output$pairs <- renderPlot({
    if(input$selectid == 'pairs'){
      pairs(data[,1:4])
    }
  })
  output$head_data <- renderTable({
    if(input$selectid == "pre_data"){
      head(data,10)
    }
  })
  
  # Normality PAGE2
  output$normalityGraph <- renderPlot({
    if(input$checkNormalGraph == 1){
      if(input$outlierButton == 0){
        qqnorm(data$y); qqline(data$y)
      }
      else{
        qqnorm(data_no_outlier$y); qqline(data_no_outlier$y)
      }
    }
  })
  output$shapiroResult <- renderPrint({
    if(input$outlierButton == 0){
      shapNorm <- shapiro.test(data$y)
      print("Alpha:")
      print(input$alpha)
      print(shapNorm)
    }
    else{
      shapNorm <- shapiro.test(data_no_outlier$y)
      print("Alpha:")
      print(input$alpha)
      print(shapNorm)
    }
  })
  resultInput <- reactive({
    if(input$outlierButton == 0){
      shapNorm <- shapiro.test(data$y)
      if(shapNorm$p.value<input$alpha){
        print("Null hypothesis is rejected. Dependent variable is not approximately to normal distribution.You may try to drop the outliers.")
      }
      else{
        print("Null hypothesis can't rejected. Dependent variable approximately to normal distrubiton")
      }
    }
    else{
      shapNorm <- shapiro.test(data_no_outlier$y)
      if(shapNorm$p.value<input$alpha){
        print("Null hypothesis is rejected. Dependent variable is not approximately to normal distribution.You may try to other transformations.")
      }
      else{
        print("Null hypothesis can't rejected. Dependent variable approximately to normal distrubiton")
      }
    }
  })
  output$commentResult <- renderPrint({
    if (input$submitbutton>0) { 
      isolate(resultInput()) 
    } 
  })
  
  # Residuals PAGE3
  output$linear_model <- renderPrint({
    if(input$residuals_selection == "model"){
      estimate_model <- summary(lModel)$coefficients
      a <- estimate_model[1,1]
      b1 <- estimate_model[2,1]
      b2 <- estimate_model[3,1]
      b3 <- estimate_model[4,1]
      b4 <- estimate_model[5,1]
      b5 <- estimate_model[6,1]
      
      paste("y = ",a,"+",b1,"+",b2,b3,b4,b5)
    }
    else if(input$residuals_selection == "reg_output"){
      summary(lModel)
    }
  })
  output$correlations <- renderTable({
    if(input$residuals_selection == "correlation"){
      diag_cor <- as.data.frame(diag$correlation)
      x11 <- diag_cor[2:6,2]
      x21 <- diag_cor[2:6,3]
      x31 <- diag_cor[2:6,4]
      x421 <- diag_cor[2:6,5]
      x431 <- diag_cor[2:6,6]
      cors <- as.data.frame(rbind(x11,x21,x31,x421,x431))
      names(cors) <- c("x1","x2","x3","x42","x43")
      cors
    }
  })
  output$std_err <- renderPrint({
    if(input$residuals_selection == "std.err"){
      std_error <- as.data.frame(diag$std.err)
      x1 <- std_error[1,]
      x2 <- std_error[2,]
      x3 <- std_error[3,]
      x42 <- std_error[4,]
      x43 <- std_error[5,]
      std_error <- as.data.frame(rbind(x1,x2,x3,x42,x43))
      names(std_error) <- c("Standart Errors")
      std_error
    }
  })
  output$inf_obs <- renderPlot({
    if(input$cook_plot == 1){
      cooksd <- cooks.distance(lModel)
      plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance");
      abline(h = 4/length(data_no_outlier$y), col="red")
      text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 4/length(data_no_outlier$y),names(cooksd),""), col="red")
    }
  })
  output$diag_table <- renderDataTable({
    diag_data <- data.frame(diag$hat,diag$std.res,diag$stud.res,diag$cooks)
    names(diag_data) <- c("Hats","Standartized Res.", "Studentized Res.","Cooks Dist.")
    diag_data[, c("Hats",input$diags), drop = FALSE]
  })
  
  # Varying variance PAGE4
  output$varying_bp <- renderPrint({
    print(input$alpha_var)
    print(bp)
    if(bp$p.value < input$alpha_var){
      print("There is no varying variance problem")
    }
    else{
      print("There is varying variance problem")
    }
  })
  output$varying_plot <- renderPlot({
    if(input$plot_var == 1){
      plot(predict(lModel), diag$stud.res, ylab= "Studentized residuals", xlab="Expected Value");abline(h=0, col="red")
    }
  })
  output$self_estem <- renderPrint({
    if(input$self_rs == 1){
      print(dw)
      if(dw$p.value < input$alpha_var){
        print("There is a positive self reliationship")
      }
      else{
        print("There is no self reliationship")
      }
    }
  })
  output$collinearity <- renderPrint({
    if(input$multi_coll > 0){
      print(vif(lModel))
      cDiag <- colldiag(model.matrix(lModel),add.intercept = FALSE)
      print(cDiag$condindx)
      if(sum(cDiag$condindx > 30) > 0){
        sum_coll <- sum(cDiag$condindx > 30)
        paste("There are", sum_coll,"strong multi collinearity", sep = " ")
      }
      else if(sum(cDiag$condindx > 10 & cDiag$condindx < 30) > 0){
        sum_coll <- sum(cDiag$condindx > 10 & cDiag$condindx < 30)
        paste("There are",sum_coll,"multi collinearity that is not strong", sep = " ")
      }
      else{
        print("There is no multi collinearity")
      }
    }
  })
  
  # Best Model & Ridge PAGE5
  predict_func <- reactive({
    if(input$predict_select == "first"){
      result <- predict(lModel_2,data.frame(X1=input$predict_x1, X2=input$predict_x2, X3=input$predict_x3, new_x42=0, new_x43=0),interval="prediction", level=1-input$predict_alpha)
    }
    else if(input$predict_select == "second"){
      result <- predict(lModel_2,data.frame(X1=input$predict_x1, X2=input$predict_x2, X3=input$predict_x3, new_x42=1, new_x43=0),interval="prediction", level=1-input$predict_alpha)
    }
    else if(input$predict_select == "third"){
      result <- predict(lModel_2,data.frame(X1=input$predict_x1, X2=input$predict_x2, X3=input$predict_x3, new_x42=0, new_x43=1),interval="prediction", level=1-input$predict_alpha)
    }
    return(result)
  })
  output$status <- renderPrint({
    if(input$predict_submit){
      "Calculation succesful!"
    }
    else{
      "Server ready to calculate..."
    }
  })
  output$prediction <- renderTable({
    if(input$predict_submit>0){
      isolate(predict_func())
    }
  })
  output$prediction_comment <- renderPrint({
    if(input$predict_submit>0){
      result <- as.data.frame(isolate(predict_func()))
      lwr <- round(result$lwr , 2)
      upr <- round(result$upr , 2)
      paste(1-input$predict_alpha, "probability the result will be between", lwr, "and", upr)
    }
  })
  output$ridgeReg <- renderPlot({
    if(input$ridge > 0){
      matplot(ridgeReg$lambda,t(ridgeReg$coef),type="l",xlab=expression(lambda),ylab=expression(hat(beta)))
      abline(h=0,lwd=2, col="red")
    }
  })
}

shinyApp(ui, server)