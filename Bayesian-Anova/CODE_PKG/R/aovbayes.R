#' @importFrom stats TukeyHSD aggregate bartlett.test density kruskal.test lm na.omit pairwise.wilcox.test qnorm qqnorm quantile sd shapiro.test
#' @importFrom utils read.csv2 str
#' @importFrom graphics hist
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT formatSignif
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard dashboardBody
#' @importFrom car durbinWatsonTest
#' @importFrom reshape melt
#' @importFrom shiny column radioButtons textOutput checkboxInput fileInput fluidRow htmlOutput icon numericInput reactive renderPrint renderTable renderText renderUI runApp selectInput shinyApp sliderInput stopApp tableOutput tabPanel uiOutput withMathJax verbatimTextOutput
#' @import shinycssloaders shinydashboardPlus tibble BayesFactor broom dplyr highcharter moments nortest rstan rstantools stringr waiter
#' @importFrom car some
#' @import htmltools
#' @importFrom purrr map

globalVariables(c("aov","fluidRow","column","a","img","dashboardPage","tagList","spin_three_bounce","textOutput","h3","Trat","upr","Trat","upr","lwr","hist","Names","Mean","se_mean","n_eff","names_from_WB","Iteration","mu","sig2","value","HTML","h2","radioButtons","checkboxInput","fileInput","variable"))





#' Interactive panel ANOVA classic, non parametric and bayesian
#'
#' Interactive panel to visualize and develop one-way analysis of variance models, from the classical, non-parametric and Bayesian approach.
#' @param dataset Data set
#' @return A shiny panel with the classical, non-parametric and Bayesian analyzes of variance, based on the specification of the dependent and independent variable of the data set provided in \code{dataset}, also provides a decision diagram that suggests which method is appropriate, based on the assumptions of the models.
#' @examples
#' \dontrun{
#' data(PollutionData)
#' aovbayes(PollutionData)
#' }
#' @export
aovbayes <- function(dataset=FALSE) {

 # require(shiny)
 # require(highcharter)
 # require(shinydashboard)
 # require(shinydashboardPlus)
 # require(BayesFactor)
 # require(dplyr)
 # require(waiter)
 # require(broom)
 # require(nortest)
 # require(moments)
 # require(car)
 # require(DT)
 # require(shinycssloaders)
 # require(rstan)
 # require(rstantools)
 # require(reshape)
 # require(purrr)
 # require(stringr)

 # left_footer <- fluidRow(
 #   column(
 #     width = 6,
 #     align = "left",
 #     a(
 #       href = "http://www.fcnm.espol.edu.ec/",
 #       target = "_blank",
 #       img(src = "https://github.com/JavierRojasC/JavierRCam/blob/master/fcnm.png?raw=true", height = "30px"),
 #       class = "dropdown",
 #       title = "Facultad de Ciencias Naturales y Matematicas")
 #   )
 # )

  runApp(list(
    ui = dashboardPage(
      preloader = list(html = tagList(spin_three_bounce(), h3("Please wait a moment ...")), color = "#1E3A4E"),

      title =  'One-way analysis of variance' ,
      dashboardHeader(title = "One-way analysis of variance",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Database", tabName = "BD", startExpanded = TRUE,icon = icon("database")),
          menuItem("Assumptions", tabName = "Assumptions", startExpanded = TRUE,icon = icon("tasks")),
          menuItem("Classic ANOVA", tabName = "ANOVAcl", startExpanded = TRUE,icon = icon("adn")),
          menuItem("Kruskal Wallis", tabName = "KW", startExpanded = TRUE,icon = icon("kickstarter-k")),
          menuItem("Bayesian ANOVA", tabName = "ANOVAby", startExpanded = TRUE,icon = icon("bold"))



        )),

      dashboardBody( tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #DADADA;
                                color: #2B1F57
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #A1A1A1;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #6B94BF;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #546A90;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A8A8A8;

                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #8B8989;
                                color: #151515;
                                style:"font-family:verdana";
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6F6F6F;
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DDDDDD;
                                }

                             /* body */
                                 .skin-blue .main-body .content-wrapper, .right-side {
                                background-color: #F3F3F3;
                                 }

                                .box.box-solid.box-primary>.box-header{
  background: rgb(0, 129, 201);
  color: #57A184;
    font-size: 18px;
  font-weight; bold;
}

.box.box-solid.box-primary{
  font-family: OpenSans;
  font-size: 16px;
  text-align: left;
  color: #AA3B3B;
}

                                '))),
                     tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

                     tabItems(
                       tabItem(tabName= "BD",
                               box(width=12,title="Upload base in csv",
                                   fluidRow(
                                 column(12,fileInput("file1", " ",
                                                    accept = c(
                                                      "text/csv",
                                                      "comma-separated-values,text/plain",
                                                      ".csv")
                                 ),
                                 checkboxInput("header", "Press if the first row contains the column names", TRUE),
                                        radioButtons(inputId="separador",label="Separador",
                                                     choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                     selected = ','))
                               ),uiOutput('var')),
                               fluidRow(width=12,
                                        box(title="Viewer",
                                            width=12,
                                            DT::dataTableOutput("DTable")))

                       ),
                       tabItem(tabName = "Assumptions",
                               sliderInput(inputId = 'alpha',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               box(title = 'Normality of the residuals',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('normality',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2(textOutput('pruebaNorm')),
                                          tableOutput('normalityTest'),
                                          h3(textOutput('normalityConclu')),
                                          h2(htmlOutput('CumpleNorm')))),
                               box(title = 'Homoscedasticity of the residuals',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('homoscedasticity',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Homoscedasticity by Bartlett`s test'),
                                         tableOutput('homoscedasticityBart'),
                                          h3(textOutput('homoscedasticityConclu')),
                                          h2(htmlOutput('CumpleHomoc'))
                                          )),
                               box(title = 'Independence of residuals',collapsible = TRUE,
                                   width = 12,

                                   column(12,
                                          h2('Independence by Durbin Watson Test'),
                                          tableOutput('independenceDurbin'),
                                          h3(textOutput('independenceConclu')),
                                          h2(htmlOutput('Cumpleindependence')))),
                               box(title = 'Symmetry of the residuals',
                                   width = 12,collapsible = TRUE,
                                   column(6,
                                          withSpinner(highchartOutput('symmetry',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Symmetry - Asymmetry coefficient'),
                                          tableOutput('symmetryCoef'),
                                          h3(textOutput('symmetryConclu')),
                                          h2(htmlOutput('CumpleSimet')))),
                               box(width = 12,collapsible = TRUE,
                                   withSpinner(highchartOutput('diagram',  height = "650px"), type = 7, color='#C7D5EB'),
                                   h2('Technique available'),
                                   withSpinner(highchartOutput('technique'), type = 7, color='#C7D5EB'))),

                       tabItem(tabName = "ANOVAcl",
                               sliderInput(inputId = 'alpha2',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),

                               box(width=12,
                                     title = "Classic ANOVA Table",collapsible = TRUE,
                                   column(width=12,align="center",
                                   tableOutput('Aov')),
                                   h2("Conclution"),
                                   h3(textOutput('conclutionAov'))),
                               column(12,withSpinner(highchartOutput('Box',  height = "450px"), type = 7, color='#C7D5EB')),
                               box(title = "Post-Hoc",collapsible = TRUE,
                                   width=12,
                                   column(6,
                                          h3('TukeyHSD'),
                                          tableOutput('AovPostHoc')),
                                   column(6,
                                          withSpinner(highchartOutput('AovPostHocGraph',  height = "450px"), type = 7, color='#C7D5EB')))
                       ),
                       tabItem(tabName = "KW",
                               sliderInput(inputId = 'alphakw',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),

                               box(title = "Kruskal Wallis Table",collapsible = TRUE,
                                   tableOutput('kw'),
                                   h2("Conclution"),
                                   h3(textOutput('conclusionKW'))),

                               box(title = 'Post Hoc: Pairwise comparisons using Wilcoxon rank sum exact test ',collapsible = TRUE,
                                   selectInput('padjust', 'Adjustment methods',
                                               c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
                                                 "fdr", "none")),
                                   h3('p-values adjusted'),
                                   DT::dataTableOutput('KWpost')
                               )

                       ),
                       tabItem(tabName = "ANOVAby",

                               box(title = "Bayesian ANOVA Table",collapsible = TRUE,
                                   tableOutput('AovBY'),
                                   h2("Conclution"),
                                   h3(textOutput('conclutionaovby'))),
                               box(title = 'Control center',collapsible = TRUE,
                                   sliderInput(inputId = 'prior',
                                               label='Enter prior probability',
                                               value=0.5,
                                               min=0,
                                               max=1),
                                   numericInput(inputId = 'numberiterations',
                                                label='Enter the number of iterations',
                                                value=1000,
                                                min=500,
                                                max=3000),
                                   sliderInput(inputId = 'chainsnumber',
                                               label='Enter number of chains:',
                                               value=1,
                                               min=1,
                                               max=4)),
                               box(title = "Posterior", width=12,collapsible = TRUE,
                                   column(12, align="center",DT::dataTableOutput('AovBYpost'))),
                               box(title = "MCMC",collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          selectInput("mcmcCHAIN","Seleccione MCMC",
                                                      c("Mean and Variance",
                                                        "Treatments")),
                                          withSpinner(highchartOutput('AovBYposmcmc',  height = "450px"), type = 7, color='#C7D5EB')),
                                   column(6,withSpinner(highchartOutput('AovBYposcurves',  height = "450px"), type = 7, color='#C7D5EB'))

                               )

                       )
                     ))),
    dashboardFooter(
      left = NULL,
      right = NULL),

    server = function(input, output) {



      data <- reactive({


        if (dataset == FALSE){
          inFile <- input$file1

          if (is.null(inFile))
            return(NULL)

          data=read.csv2(inFile$datapath, sep=input$separador,header = input$header)
          data
        } else {
          data = dataset}






      })

      output$DTable <- DT::renderDataTable({
        Data <- data()

        datatable(Data, extensions = 'FixedColumns',
        options = list(
          dom = 't',
          scrollX = TRUE,
          fixedColumns = TRUE
        ))
      })

      output$var <- renderUI({

        if(is.null(data())){return()}

        else list (

          selectInput("y", "Dependent variable", choices =    names(data())),
          selectInput("x", "Independent variable", choices = names(data()))


        )
      })

      output$Aov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- summary(aov(Depend~Factor))
        S <- as.data.frame(SA[[1]])
        S <- signif(S,4)
        S[is.na(S)] <- ' '

        S <- data.frame(c(Ind,'Residuals'),S)

        colnames(S) <- c('','df','SS','MS','F','p-value')
        S
      })
      output$conclutionAov <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- summary(aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (SA[[1]][['Pr(>F)']][1] < input$alpha2){
          response <- paste0('There are significant differences between the groups of',Ind)
        } else if  (SA[[1]][['Pr(>F)']][1] > input$alpha2){
          response <- paste0('There are no significant differences between the groups of',Ind)}

        response
      })

      output$normality <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x



        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        Graph <- qqnorm(SA$residuals, pch = 1, frame = FALSE)
        DataLine <- data.frame(xd=Graph[[1]],yd=Graph['y'])
        colnames(DataLine) <- c('xd','yd')
        LIN <- augment(lm(yd~xd, data=DataLine))



        yRES=SA$residuals
        distribution = qnorm
        probs = c(0.25, 0.75)
        qtype = 7

        y1 <- quantile(yRES, probs, names = FALSE, type = qtype, na.rm = TRUE)
        x1 <- distribution(probs)

        slope <- diff(y1)/diff(x1)
        int <- y1[1L] - slope * x1[1L]

        Int=int
        Slp=slope


        x=Graph[[1]]
        Recta <- Int+Slp*x
        lineQQ <- data.frame(x2=Graph[[1]], y2=Recta)
        highchart() %>%
          hc_add_series(lineQQ, "line", hcaes(x = 'x2', y = 'y2'), name='QQ line', color='#A9DEDE',
                        marker= list(symbol='url(graphic.png)'))%>%
          hc_add_series(LIN, "scatter", hcaes(x='xd', y='yd'), name='Points', color='#2B275A') %>%
          hc_yAxis(
            title = list(text = "Standardized Residuals"),
            max=max(lineQQ$y2),
            min=min(lineQQ$y2))%>%
          hc_xAxis(
            title = list(text = "Theoretical Quantiles"))%>%
          hc_title(text='QQ plot')
      })

      output$AovPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- (aov(Depend~Factor))
        intervals = TukeyHSD(SA)

        S <- as.data.frame(intervals[[1]])
        S <- signif(S,4)
        S <- cbind(rownames(S),S)

        names(S)[1] <- ' '

        S
      })

      output$AovPostHocGraph <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- (aov(Depend~Factor))
        intervals = TukeyHSD(SA)

        S <- as.data.frame(intervals[[1]])
        S <- signif(S,4)
        S <- cbind(rownames(S),S)


        names(S)[1] <- 'Trat'

        hchart(pointWidth=0,type = 'columnrange',S,name='Interval',
               hcaes(x=Trat,high=upr, low=lwr), color='#224361')%>%
          hc_add_series(S, type='scatter', hcaes(x=Trat, y=diff), name='Diferences', color='#289B9C',
                        tooltip = list(pointFormat = "<br> Diference = {point.y}"))%>%
          hc_xAxis(title=list(text=('Treatment combinations')))%>%
          hc_yAxis(title=list(text=('Diferences')),
                   plotLines = list(list(
                     value = 0,
                     color = '#DAE0EA',
                     width = 3,
                     zIndex = 4,
                     label = list(text = "",
                                  style = list( color = '#DAE0EA', fontWeight = 'bold' )))))
      })

      output$normalityTest <- renderTable({
        Data <- data()

        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))

        if (length(SA$residuals)>30){

          Test <- lillie.test(SA$residuals)
          Tabla <- data.frame(Statistic=signif(Test$statistic,4),
                              ValP=signif(Test$p.value,4))
          colnames(Tabla) <- c('KS Statistic','p-value')
          Tabla
        } else {
          Test <- shapiro.test(SA$residuals)
          Tabla <- data.frame(Statistic=Test$statistic,
                              ValP=Test$p.value)
          colnames(Tabla) <- c('Shapiro-Wilk statistic','p-value')
          Tabla
        }
      })


      output$normalityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('According to the Kolmogorov-Smirnov test, the residuals are normal')
          } else {
            response=paste0('According to the Kolmogorov-Smirnov test, the residuals are not normal')
          }
          response
        } else {
          Test <- shapiro.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('According to the Shapiro-Wilk test, the residuals are normal')
          } else {
            response=paste0('According to the Shapiro-Wilk test, the residuals are not normal')
          }
          response
        }
      })

      output$pruebaNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (length(SA$residuals)>30){

          response=paste0('Normality by Kolmogorov-Smirnov test')

          response
        } else {

          response=paste0('Normality by Shapiro-Wilk test')

          response
        }
      })

      output$CumpleNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))

        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)

          if(Test$p.value >=  input$alpha ){
            return(paste("Assumption of Normality: ","<span style=\"color:green;\"> Is met. </span>"))

          }else{
            return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Is not met.</span>"))
          }} else {

            Test <- shapiro.test(SA$residuals)

            if(Test$p.value >=  input$alpha ){
              return(paste("Assumption of Normality: ","<span style=\"color:green;\"> Is met.</span>"))

            }else{
              return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Is not met.</span>"))
            }
          }
      })



      #_________________________________________________________________

      output$homoscedasticity <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        xs=SA$fitted.values
        ys=SA$residuals
        lineAR <- data.frame(x2=xs, y2=ys)
        highchart() %>%

          hc_yAxis(
            title = list(text = "Residuals"),
            plotLines = list(list(
              value = 0,
              color = '#A9DEDE',
              width = 3,
              zIndex = 4,
              label = list(text = "",
                           style = list( color = '#1D4B5E', fontWeight = 'bold' )))),
            max=max(lineAR$y2),
            min=min(lineAR$y2))%>%
          hc_add_series(lineAR, "scatter", hcaes(x = 'x2', y = 'y2'), name='Residual vs Adjusted', color='#2B275A'
          )%>%
          hc_xAxis(
            title = list(text = "Adjusted values"))%>%
          hc_title(text='Residual vs Adjusted')
      })


      output$homoscedasticityBart <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)
        Tabla <- data.frame(Statistic=signif(Bart$statistic,4),
                            ValP=signif(Bart$p.value,4))
        colnames(Tabla) <- c('Bartlett`s K-square statistic','p-value')
        Tabla
      })


      output$homoscedasticityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)

        if (Bart$p.value >= input$alpha){
          response=paste0('According to the Bartlett test, the samples show equal variances')
        } else {
          response=paste0('According to the Bartlett test, the samples show unequal variances')
        }
        response
      })


      output$CumpleHomoc <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)
        if(Bart$p.value >=  input$alpha ){
          return(paste("Homoscedasticity assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Homoscedasticity assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })
      #________________________________________________________________




      output$independenceDurbin <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)
        Tabla <- data.frame(Autocor=DW[1],
                            Dw=signif(as.numeric(DW[2]),4),
                            ValP=signif(as.numeric(DW[3]),4))
        colnames(Tabla) <- c('Autocorrelation','D-W Statistic',
                             'p-value')
        Tabla
      })


      output$independenceConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          response=paste0('According to the Durbin Watson test, there is no presence of autocorrelation in the residuals.')
        } else {
          response=paste0('According to the Durbin Watson test, there is the presence of autocorrelation in the residuals.')
        }
        response
      })


      output$Cumpleindependence <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          return(paste("Independence assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Independence assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })

      #_________________________________________________________

      output$symmetry <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        # Dep <- names(Data)[2]
        #  Ind <- names(Data)[1]
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        skewness(SA$residuals)

        histRes <- hist(SA$residuals, plot=FALSE)
        hchart(histRes, name='', color='#84DED4')%>%
          hc_title(text='Histogram of the residuals')
      })


      output$symmetryCoef <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))


        Tabla <- data.frame(Statistic=skewness(SA$residuals))
        colnames(Tabla) <- c('Asymmetry coefficient')
        Tabla
      })


      output$symmetryConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        if (round(skewness(SA$residuals),2) > 0){
          response=paste0('According to the skewness coefficient, the distribution of the residuals has a positive skewness (Right bias)')
        } else if (round(skewness(SA$residuals),2) < 0){
          response=paste0('According to the skewness coefficient, the distribution of the residuals has a negative skewness (Left bias)')
        } else {
          response=paste0('According to the coefficient of skewness, the distribution of the residuals is symmetric.')

        }
        response
      })


      output$CumpleSimet <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        if(skewness(SA$residuals) ==  0 ){
          return(paste("Symmetry Assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Symmetry Assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })

      #__________________________________________________




      output$Box <- renderHighchart({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Means <- aggregate(as.matrix(Data[,Dep]) ~ as.factor(as.matrix(Data[,Ind])), data = Data, mean)
        colnames(Means) <- c('Names', 'Mean')

        hcboxplot(x=as.numeric(as.matrix(Data[,Dep])), var=as.factor(as.matrix(Data[,Ind])), name = "Boxplot", color = "#0E1142", outliers = FALSE,
                  showInLegend=TRUE)%>%
          hc_yAxis(title = list(text = Dep))%>%
          hc_xAxis(title = list(text = "Levels"))%>%
          hc_chart(type = "column")%>%
          hc_plotOptions(showInLegend=TRUE,dataLabels=TRUE)%>%
          hc_add_series(Means, type='bubble', hcaes(x =Names,y=Mean),maxSize = "7%",
                        tooltip=list(pointFormat='<br> {point.y} ',headerFormat='<b> Mean'), name='Means',
                        showInLegend=TRUE)
      })
      output$AovBY <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior





        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Treatment','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Treatment, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))



        S <- data.frame(Priori=(prio), BF=Anovabyy[1][1])
        TabBY <- S[,1:3]
        TabBY$BF.bf <- round(TabBY$BF.bf,3)
        TabBY$BF.error <- signif(TabBY$BF.error,3 )

        colnames(TabBY) <- c('Priori','BF10','Error')
        TabBY <- rbind(TabBY, c(1-prio,1,''))

        rownames(TabBY) <- c('Alternative Model', 'Null Model')
        TabBY <- cbind(rownames(TabBY),TabBY)

        names(TabBY)[1] <- ''
        TabBY
      })

      stan_summary = function(
        from_stan
        , par
        , probs = c(.5,.025,.975)
        , X = NULL
        , W = NULL
        , B = NULL
        , is_cor = F
      ){
        m = monitor(from_stan,probs=probs,print=F)
        all_pars = dimnames(m)[[1]]
        all_pars_no_squares = str_replace(dimnames(m)[[1]],'\\[.*\\]','')
        select_pars = all_pars[all_pars_no_squares%in%par]
        requested_pars = par
        m %>%
          tibble::as_tibble(m) %>%
          dplyr::mutate(
            par = str_replace(dimnames(m)[[1]],'\\[.*\\]','')
          ) %>%
          dplyr::filter(
            par%in%requested_pars
          ) %>%
          dplyr::select(
            par
            , mean
            , se_mean
            , sd
            , contains('%')
            , n_eff
            , Rhat

          ) ->
          m

        if(!is_cor){
          if(!is.null(X)){
            m$par = dimnames(X)[[2]]
          }
          if(!is.null(W)){
            m$par = names_from_WB(W,B)
          }
        }else{
          temp = select_pars
          temp = gsub(']','',temp)
          temp = unlist(strsplit(temp,'[',fixed=T))
          temp = temp[(1:length(temp))%%2==0]
          temp = unlist(strsplit(temp,',',fixed=T))
          v1 = temp[(1:length(temp))%%2==1]
          v2 = temp[(1:length(temp))%%2==0]
          keep = v2>v1
          v1 = v1[keep]
          v2 = v2[keep]
          if(!is.null(X)){
            v1 = dimnames(X)[[2]][as.numeric(v1)]
            v2 = dimnames(X)[[2]][as.numeric(v2)]
          }
          if(!is.null(W)){
            temp = names_from_WB(W,B)
            v1 = temp[as.numeric(v1)]
            v2 = temp[as.numeric(v2)]
          }
          m = m[keep,]
          m$par = paste(v1,v2,sep='~')
        }
        return(m)
      }
      output$AovBYpost <- DT::renderDataTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior



        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        lambda <- -log(0.01)/(3*sd(dataBY$Dep2))



        data2 <- list(N=length(dataBY$Dep2),
                      J=length(unique(dataBY$Tratamiento)),
                      response=dataBY$Dep2,
                      predictor=as.numeric(dataBY$Tratamiento),
                      lambda=lambda)

        sm <- rstan::sampling(stanmodels$onewaymodel,
                              data=data2, chains=input$chainsnumber,
                              seed = 12345,iter=input$numberiterations,
                              open_progress =FALSE)


        tab <- stan_summary(sm, par=c("mu","sigmaalpha","sigmaepsilon","a"),
                     probs  =c(.5,.025,.975))

        tab$par <- c('Mu','Sigma Alpha','Sigma Epsilon',unique(as.character(dataBY$Tratamiento)))
        DT::datatable(tab, extensions = 'FixedColumns',
                      options = list(
                        dom = 't',
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = length(tab$par)
                      ))%>% formatSignif(c("mean", "se_mean", "sd","50%","2.5%","97.5%", "Rhat"), 3)
    })


      output$AovBYposmcmc <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Treatment','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Treatment, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))

        post <- (posterior(Anovabyy,iterations = input$numberiterations))
        MCMC <- data.frame(Iteration=1:input$numberiterations,post[,])

        if (input$mcmcCHAIN=="Mean and Variance"){

          highchart()%>%
            hc_yAxis_multiples( list(top = "0%", height = "50%", title = list(text = "Mean"),opposite=FALSE),
                                list(top = "50%", height = "50%", title = list(text = "Sigma2") ,opposite=TRUE))%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteration,y=mu),yAxis=0, name='Mean',color='#24509C')%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteration,y=sig2),yAxis=1, name='Sigma2',color='#31999C')
        } else {

          MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
          rownames(MCMCCom) <- MCMC[,1]
          MCMCCom2 <- as.matrix(MCMCCom)
          MCMCMer <- melt(MCMCCom, id.vars="Iteration")
          highchart()%>%
            hc_add_series(MCMCMer, type='line', hcaes(x=Iteration, y=value, group=variable))%>%
            hc_title(text='MCMC chains')%>%
            hc_exporting(enabled = TRUE,
                         filename = paste0('Markov chains'))

        }
      })

      output$AovBYposcurves <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Treatment','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Treatment, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))

        post <- (posterior(Anovabyy,iterations = input$numberiterations))

        MCMC <- data.frame(Iteration=1:input$numberiterations,post[,])
        MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
        MCMCMer <- melt(MCMCCom, id.vars="Iteration")


        ds <- map(levels(MCMCMer$variable), function(x){
          MCMCMer <- density(MCMCMer$value[MCMCMer$variable == x])[1:2]
          MCMCMer <- list_parse2(as.data.frame(MCMCMer))
          list(data = MCMCMer, name = x)
        })

        highchart() %>%
          hc_add_series_list(ds)%>%
          hc_yAxis(title=list(text='Density'))%>%
          hc_exporting(enabled = TRUE,
                       filename = paste0('Density curves - Posterior marginal distributions.'))
      })



      output$conclutionaovby <- renderText({

        Data <- data()

        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Ind2, data=dataBY, whichRandom = "all",
                             rscaleFixed = prio))


        S <- data.frame(Priori=prio, BF=Anovabyy[1])
        FB <- S[,2]
        if (FB <= 3 & FB > 1 ){
          response <- paste0('Weak evidence in favor of rejection of the null hypothesis')
        } else if  (FB <= 10 & FB > 3 ) {
          response <- paste0('Moderate evidence in favor of rejection of the null hypothesis')
        } else if  (FB <= 30 & FB > 10 ){
          response <- paste0('Strong evidence in favor of the rejection of the null hypothesis')
        }else if  (FB > 30 ){
          response <- paste0('Decisive evidence in favor of the rejection of the null hypothesis')
        }else if  (FB < 1 & FB > 1/3 ){
          response <- paste0('Weak evidence in favor of the null hypothesis')
        }else if  (FB < 1/3 & FB > 1/10 ){
          response <- paste0('Moderate evidence in favor of the null hypothesis')
        }else if  (FB <= 1/10 & FB > 1/30 ){
          response <- paste0('Strong evidence in favor of the null hypothesis')
        }else if  (FB < 1/30){
          response <- paste0('Decisive evidence in favor of the null hypothesis')
        }else if  (FB == 1){
          response <- paste0('There is no evidence')}

        response
      })


      output$diagram <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        alph <- input$alpha
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        Test <- lillie.test(SA$residuals)

        if(Test$p.value >=  alph ){
          col_normality= "#77DA85"
          col_normality_yes= "#77DA85"
          col_normality_no= "#D5D5D5"

        }else{
          col_normality= "#D5D5D5"
          col_normality_yes= "#D5D5D5"
          col_normality_no= "#77DA85"
        }

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)

        if(Bart$p.value >=  alph ){
          col_homoscedasticity= "#77DA85"
          col_homoscedasticity_yes= "#77DA85"
          col_homoscedasticity_no= "#D5D5D5"

        }else{
          col_homoscedasticity= "#D5D5D5"
          col_homoscedasticity_yes= "#D5D5D5"
          col_homoscedasticity_no= "#77DA85"
        }

        if(skewness(SA$residuals) ==  0 ){
          col_symmetry= "#77DA85"
          col_symmetry_yes= "#77DA85"
          col_symmetry_no= "#D5D5D5"

        }else{
          col_symmetry= "#D5D5D5"
          col_symmetry_yes= "#D5D5D5"
          col_symmetry_no= "#77DA85"
        }

        if(durbinWatsonTest(SA)[3] >=  alph){
          col_independence= "#77DA85"
          col_independence_yes= "#77DA85"
          col_independence_no= "#D5D5D5"

        }else{
          col_independence= "#D5D5D5"
          col_independence_yes= "#D5D5D5"
          col_independence_no= "#77DA85"
        }



        if (col_symmetry_yes == "#77DA85"){
          col_kw="#77DA85"
        } else {col_kw="#D5D5D5" }

        if (col_homoscedasticity_yes == "#77DA85"){
          col_independence="#77DA85"
        } else {col_independence="#D5D5D5" }

        #  if (col_independence_no == "#77DA85" | col_independence_yes == "#77DA85"){
        #    col_independence="#77DA85"
        #  } else {col_independence="#D5D5D5" }

        if (col_normality_yes== "#77DA85" & col_homoscedasticity_yes== "#77DA85" ){
          col_anova="#77DA85"
        }else {col_anova="#D5D5D5" }


        if (col_symmetry_yes=="#77DA85" | col_symmetry_no=="#77DA85"){
          col_symmetry= "#77DA85"
        }
        if (col_homoscedasticity_yes=="#77DA85" | col_homoscedasticity_no=="#77DA85"){
          col_homoscedasticity= "#77DA85"
        }


        highchart() %>%
          hc_chart(type = 'organization', inverted = TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          list(from = 'Comparison of means by group', to = 'Does it comply with the normality assumption?'),
                          list(from = 'Does it comply with the normality assumption?', to = 'Yes, it fulfills normality'),
                          list(from = 'Yes, it fulfills normality', to = 'Does it meet the homoscedasticity assumption?'),
                          list(from = 'Does it comply with the normality assumption?', to = 'It does not meet normality'),
                          list(from = 'Does it meet the homoscedasticity assumption?', to = 'Yes, it fulfills homoscedasticity'),
                          list(from = 'Yes, it fulfills homoscedasticity', to = 'Does it comply with the independence assumption?'),
                          list(from = 'Does it comply with the independence assumption?', to = 'Yes, it fulfills independence'),
                          list(from = 'Does it comply with the independence assumption?', to = 'It does not meet independence'),

                          list(from = 'Does it meet the homoscedasticity assumption?', to = 'It does not meet homoscedasticity'),
                          list(from = 'Does it meet the simmetry assumption?', to = 'Yes, it fulfills simmetry'),
                          list(from = 'Does it meet the simmetry assumption?', to = 'It does not meet simmetry'),

                          list(from = 'It does not meet homoscedasticity', to = 'Does it meet the simmetry assumption?')







                        ),
                        nodes=  list(
                          list(id = 'Comparison of means by group', color="#77D0DA"),
                          list(id = 'Does it comply with the normality assumption?', color=col_normality),
                          list(id = 'Yes, it fulfills normality', color=col_normality_yes),
                          list(id = 'It does not meet normality', color=col_normality_no),
                          list(id = 'Does it meet the homoscedasticity assumption?', color=col_homoscedasticity),
                          list(id = 'Yes, it fulfills homoscedasticity', color=col_homoscedasticity_yes),
                          list(id = 'It does not meet homoscedasticity', color=col_homoscedasticity_no),
                          list(id = 'Does it meet the simmetry assumption?', color=col_symmetry),
                          list(id = 'Yes, it fulfills simmetry', color=col_symmetry_yes),
                          list(id = 'It does not meet simmetry', color=col_symmetry_no),
                          list(id = 'Does it comply with the independence assumption?', color=col_independence),
                          list(id = 'Yes, it fulfills independence', color=col_independence_yes),
                          list(id = 'It does not meet independence', color=col_independence_no)

                        ))

      })



      output$technique <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        dataBY <- data.frame(Ind2=Factor, Dep2=Data[,Dep])
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        Test <- lillie.test(SA$residuals)

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)



        if(Test$p.value >=  input$alpha & Bart$p.value >=  input$alpha){
          col_anova="#77DA85"
        } else {col_anova="#DC7676"}

        if(skewness(SA$residuals) ==  0){
          col_kw="#77DA85"
        } else {col_kw="#DC7676"}

        highchart() %>%
          hc_chart(type = 'organization', inverted=TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          list(from = 'Kruskal Wallis', to = 'Kruskal Wallis'),
                          list(from = 'Classic ANOVA', to = 'Classic ANOVA'),
                          list(from = 'Bayesian ANOVA', to = 'Bayesian ANOVA')
                        ),
                        nodes=  list(
                          list(id = 'Classic ANOVA', color=col_anova),
                          list(id = 'Kruskal Wallis', color=col_kw),
                          list(id = 'Bayesian ANOVA', color='#77DA85')
                        ))

      })


      output$kw <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <-kruskal.test(Depend~Factor, data = Data)

        S <- data.frame(SA$statistic,SA$parameter,signif(SA$p.value,4))

        colnames(S) <- c('Kruskal-Wallis chi-squared','Gl','Val-p')
        S
      })
      output$conclusionKW <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        SA <-kruskal.test(Depend~Factor, data = Data)
        if (SA$p.value < input$alphakw){
          response <- paste0('There are significant differences between the groups of ',Ind)
        } else if  (SA$p.value > input$alphakw){
          response <- paste0('There are no significant differences between the groups of ',Ind)}

        response
      })

      output$KWpost <- DT::renderDataTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        Pares <- pairwise.wilcox.test(x = Depend, g = Factor, p.adjust.method = input$padjust )
        Pv <- Pares$p.value
        Pv[is.na(Pv)] <- ' - '
        #Pv <- cbind(rownames(Pv),Pv)

        DT::datatable(Pv, extensions = 'FixedColumns',
                      options = list(
                        dom = 't',
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = nrow(Pv)
                      ))
      })

    }))
}
