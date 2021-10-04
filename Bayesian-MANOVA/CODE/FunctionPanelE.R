manoby <- function(dataset=FALSE) {
  
  require(shiny)
  require(highcharter)
  require(shinydashboard)
  require(shinydashboardPlus)
  require(BayesFactor)
  require(waiter)
  require(broom)
  require(nortest)
  require(moments)
  require(car)
  require(shinycssloaders)
  require(rstan)
  require(reshape)
  require(purrr)
  require(brms)
  require(MVN)
  require(heplots)
  require(vegan)
  require(mvnormtest)
  require(posterior)
  
  
  
  left_footer <- fluidRow(
    column(
      width = 6,
      align = "left",
      a(
        href = "http://www.fcnm.espol.edu.ec/",
        target = "_blank",
        img(src = "https://github.com/JavierRojasC/JavierRCam/blob/master/fcnm.png?raw=true", height = "30px"),
        class = "dropdown",
        title = "Facultad de Ciencias Naturales y Matematicas")
    )
  )
  
  app <- list(
    ui = dashboardPage(
      preloader = list(html = tagList(spin_three_bounce(), h3("Loading ...")), color = "#003F63"),
      
      title =  '' ,
      dashboardHeader(title = "Multivariate Variance Analysis",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Database", tabName = "BD", startExpanded = TRUE,icon = icon("database")),
          menuItem("Assumptions", tabName = "Assumptions", startExpanded = TRUE,icon = icon("tasks")),
          menuItem("Classic MANOVA", tabName = "MANOVAcl", startExpanded = TRUE,icon = icon("medium-m")),
          menuItem("PERMANOVA", tabName = "PERMANOVA", startExpanded = TRUE,icon = icon("product-hunt")),
          menuItem("Bayesian MANOVA", tabName = "MANOVAby", startExpanded = TRUE,icon = icon("bold"))
          
          
          
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
                               box(title="Data entry",
                                   width=12,
                                   fluidRow(
                                     column(6,fileInput("file1", "Upload base in csv",
                                                        accept = c(
                                                          "text/csv",
                                                          "comma-separated-values,text/plain",
                                                          ".csv")
                                     )),
                                     column(6,checkboxInput("header", "press if the first row contains the names of the columns", TRUE),
                                            radioButtons(inputId="separator",label="Separator",
                                                         choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                         selected = ','))
                                   ),uiOutput('var')),
                               fluidRow(width=12,
                                        box(width=12,
                                            title="Database view",
                                            
                                            DT::dataTableOutput("DTable")))
                               
                       ),
                       tabItem(tabName = "Assumptions",
                               sliderInput(inputId = 'alpha',
                                           label='Enter alpha (Error type 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               box(title = 'Multivariate normality',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('Normality',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2(textOutput('testNorm')),
                                          tableOutput('normalitymardia'),
                                          h3(textOutput('normalityConclu')),
                                          h2(htmlOutput('MeetsNorm')))),
                               box(title = 'Homoscedasticity of variance between groups',collapsible = TRUE,
                                   width = 12,
                                   column(12,
                                          h2('Homoscedasticity by M-Box test'),
                                          tableOutput('homoscedasticityBox'),
                                          h3(textOutput('homoscedasticityConclu')),
                                          h2(htmlOutput('MeetsHomoc')))),
                               box(title = 'Homogeneity of dispersion between groups',collapsible = TRUE,
                                   width = 12,
                                   #column(12,
                                   #      withSpinner(highchartOutput('homogeneity',  height = "390px"), type = 7, color='#C7D5EB')
                                   #),
                                   
                                   column(12,
                                          h2('Dispersion homogeneity permutest'),
                                          tableOutput('homogeneityPermutest'),
                                          h3(textOutput('homogeneityConclu')),
                                          h2(htmlOutput('Meetshomog')))),
                               
                               box(width = 12,collapsible = TRUE,
                                   withSpinner(highchartOutput('diagramAssumptions',  height = "650px"), type = 7, color='#C7D5EB'),
                                   h2('Technique available'),
                                   withSpinner(highchartOutput('TechnicalChoice'), type = 7, color='#C7D5EB'))),
                       
                       tabItem(tabName = "MANOVAcl",
                               sliderInput(inputId = 'alpha2',
                                           label='Enter alpha (Error type 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               
                               box(title = "MANOVA Classic Table",collapsible = TRUE,
                                   width=12,
                                   tableOutput('MAov'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionMAov')),
                                   column(12,withSpinner(highchartOutput('Box',  height = "400px"), type = 7, color='#C7D5EB'))),
                               
                               box(title = "Post-Hoc",collapsible = TRUE,
                                   width=12,
                                   selectInput('test', 'Statistics Test',
                                               c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")),
                                   
                                   column(6,
                                          h3('post'),
                                          tableOutput('MAovPostHoc')),
                                   column(6,
                                          withSpinner(highchartOutput('MAovPostHocGraph',  height = "450px"), type = 7, color='#C7D5EB')))
                       ),
                       tabItem(tabName = "PERMANOVA",
                               sliderInput(inputId = 'alpha3',
                                           label='Enter alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               numericInput("text", "Number of permutations:",999),
                               
                               box(title = "PERMANOVA Table",status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   tableOutput('PERMaov'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionPERMaov'))),
                               
                               box(title = 'Permutest ',status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   tableOutput('PerPostHoc'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionPerPostHoc'))
                                   
                               ),
                               box(title = 'Graphic',status = 'primary',solidHeader = TRUE,
                                   collapsible = TRUE,
                                   width=12,
                                   column(12,withSpinner(highchartOutput('Dispersions',  height = "450px"), type = 7, color='#C7D5EB'))
                               )
                               
                       ),
                       tabItem(tabName = "MANOVAby",
                               
                               
                               box(title = 'Control Center',collapsible = TRUE,
                                   sliderInput(inputId = 'prior',
                                               label='Enter probability a priori',
                                               value=0.5,
                                               min=0,
                                               max=1),
                                   numericInput(inputId = 'numberiterations',
                                                label='Enter the number of iterations',
                                                value=1000,
                                                min=500,
                                                max=3000),
                                   sliderInput(inputId = 'chainsnumber',
                                               label='Enter string number:',
                                               value=1,
                                               min=1,
                                               max=4)),
                               box(title = "POSTERIOR",collapsible = TRUE,
                                   width = 12,
                                   DT::dataTableOutput('MAovBY')
                                   # h2("Conclusion"),
                                   # h3(textOutput('conclusionmaovby'))
                               ),
                               # box(title = "Posterior", width=12,collapsible = TRUE,
                               #     column(12, align="center",tableOutput('AovBYpost'))),
                               box(title = "MCMC",collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          selectInput("mcmcCHAIN","Seleccione MCMC",
                                                      c("Mean and Variance",
                                                        "Treatments")),
                                          withSpinner(highchartOutput('MAovBYposmcmc',  height = "450px"), type = 7, color='#C7D5EB')),
                                   column(6,withSpinner(highchartOutput('MAovBYposcurves',  height = "450px"), type = 7, color='#C7D5EB'))
                                   
                               )
                               
                       )
                     ))),
    dashboardFooter(
      left = left_footer,
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
        
        DT::datatable(Data, extensions = "FixedColumns", 
                      options = list(dom = "t", scrollX = TRUE, 
                                     fixedColumns = TRUE))
      })
      
      output$var <- renderUI({
        
        if(is.null(data())){return()}
        
        else list (
          
          selectInput("y", "Dependent variable", choices =    names(data()),multiple = TRUE),
          selectInput("x", "Independent variable", choices = names(data()),multiple = FALSE)
          
          
        )
      })
      
      output$MAov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- summary(manova(Depend~Factor))
        tabma<-as.data.frame(SA$stats)
        tabmaT<-data.frame(grupo=row.names(tabma),
                           Pillai=c(signif(tabma$Pillai[1],4)," "),
                           AppoxF=c(signif(tabma$`approx F`[1],4)," "),
                           PrF=c(signif(tabma$`Pr(>F)`[1],4)," "))
        tabmaT
        
      })
      
      output$conclusionMAov <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- summary(manova(Depend~Factor))
        if (SA[[4]][11] < input$alpha2){
          response <- paste0('There are significant differences between the groups of ',Ind)
        } else if  (SA[[4]][11] > input$alpha2){
          response <- paste0('There are no significant differences between the groups of ',Ind)}
        
        response
      })
      
      output$Normality <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        
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
          hc_add_series(LIN, "scatter", hcaes(x='xd', y='yd'), name='Puntos', color='#2B275A') %>% 
          hc_yAxis(
            title = list(text = "Standardized Residuals"),
            max=max(lineQQ$y2),
            min=min(lineQQ$y2))%>% 
          hc_xAxis(
            title = list(text = "Theoretical Quantiles"))%>%
          hc_title(text='QQ plot')
      })
      
      output$MAovPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- summary(manova(Depend~Factor,test=input$test),input$test)
        
        tabma<-as.data.frame(SA$stats)
        
        tabmaT<-data.frame(grupo=c(row.names(tabma)[1]," "),
                           c(signif(tabma[2][1],4)),
                           AppoxF=c(signif(tabma$`approx F`[1],4)," "),
                           PrF=c(signif(tabma$`Pr(>F)`[1],4)," "))
        tabmaT
        
        
        
      })
      
      output$MAovPostHocGraph <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(Data[,Ind])
        Depend <- as.matrix(Data[,Dep])
        
        manobi<- MultBiplotR::CanonicalBiplot(Depend, group=Factor)
        manobix <- manobi$RowCoordinates[,1]
        manobiy <- manobi$RowCoordinates[,2]
        dtma<-data.frame(manobix,manobiy,Factor)
        
        highchart() %>%
          
          hc_add_series(dtma, "scatter", hcaes(x = manobix, y = manobiy, group = Factor)) %>%
          hc_title(text = "") %>% 
          hc_chart(type = 'line') %>%
          hc_add_series(c(0,manobi$Structure_Correlations[1,2]), 
                        name = row.names(manobi$Structure_Correlations)[1], dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,manobi$Structure_Correlations[2,2]), 
                        name =row.names(manobi$Structure_Correlations)[2], dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,manobi$Structure_Correlations[3,2]), 
                        name = row.names(manobi$Structure_Correlations)[3], dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,manobi$Structure_Correlations[4,2]), 
                        name = row.names(manobi$Structure_Correlations)[4], dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,manobi$Structure_Correlations[5,2]), 
                        name = row.names(manobi$Structure_Correlations)[5], dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,manobi$Structure_Correlations[6,2]), 
                        name = row.names(manobi$Structure_Correlations)[6], dataLabels = list(enabled = TRUE))
        
      })
      
      
      
      output$normalityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        SA <- (manova(Depend~Factor))
        if (length(SA$residuals)>30){
          Test <- mvn(Data[,Dep],mvnTest='mardia')
          if (Test$multivariateNormality$Result == 'NO'){
            response=paste0('According to Mardias test, the set of dependent variables are not normal')
          } else {
            response=paste0('According to mardias test, the set of dependent variables are normal')
          }
          response
        } else {
          Test <- mshapiro.test(t(Data[,Dep]))
          if (Test$p.value >= input$alpha){
            response=paste0('According to the Shapiro-Wilk test, the set of dependent variables are normal')
          } else {
            response=paste0('According to the Shapiro-Wilk test, the set of dependent variables are not normal')
          }
          response
        }
      })
      
      output$testNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        SA
        
        if (length(SA$residuals)>30){
          
          response=paste0('Normality by Mardia Test')
          
          response
        } else {
          
          response=paste0('Normality by Shapiro-Wilk test')
          
          response
        }
      })
      
      output$normalitymardia <-renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        
        if (nrow(Data[,Dep])>30){
          
          
          Test <- mvn(Data[,Dep],mvnTest='mardia')
          
          TabNorm <- as.data.frame(Test$multivariateNormality)
          TabNorm$Statistic <- as.numeric(as.character(TabNorm$Statistic))
          TabNorm$`p value` <- as.numeric(as.character(TabNorm$`p value`))
          TabNorm$Result <- as.character(TabNorm$Result)
          
          TabNormT <- data.frame(Test=c(TabNorm$Test),Statistic=c(round(TabNorm$Statistic[1:2],3)," "),pval=c(signif(TabNorm$`p value`[1:2],3)," "),Results=TabNorm$Result)}else{
            TabNorm <-  mshapiro.test(t(Data[,Dep]))
            TabNormT <- data.frame(W=TabNorm[[1]], pval=signif(TabNorm[[2]],3))
          }
        TabNormT
      })
      
      output$MeetsNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        
        if (length(SA$residuals)>30){
          Test <- mvn(Data[,Dep],mvnTest='mardia')
          
          if(Test$multivariateNormality$Result == 'NO'){
            return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Fails.</span>"))
            
          }else{
            return(paste("Assumption of Normality: ","<span style=\"color:green;\"> If it complies.</span>"))
          }} else {
            
            Test <- mshapiro.test(t(Data[,Dep]))
            
            if(Test$p.value >=  input$alpha ){
              return(paste("Assumption of Normality: ","<span style=\"color:green;\"> If it complies.</span>"))
              
            }else{
              return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Fails.</span>"))
            }
          }
      })
      
      
      
      #_________________________________________________________________
      
      output$homocedasticidad <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- boxM(Depend ~ Factor, data=Data)
        SA
        #xs=SA$fitted.values
        #ys=SA$residuals
        #lineAR <- data.frame(x2=xs, y2=ys)
        #highchart() %>%
        #  
        #  hc_yAxis(
        #    title = list(text = "Residuos"),
        #    plotLines = list(list(
        #      value = 0,
        #      color = '#A9DEDE',
        #      width = 3,
        #      zIndex = 4,
        #      label = list(text = "",
        #                   style = list( color = '#1D4B5E', fontWeight = 'bold' )))),
        #    max=max(lineAR$y2),
        #    min=min(lineAR$y2))%>% 
        #  hc_add_series(lineAR, "scatter", hcaes(x = 'x2', y = 'y2'), name='Residuos vs Ajustados', color='#2B275A'
        #  )%>% 
        #  hc_xAxis(
        #    title = list(text = "Valores Ajustados"))%>%
        #  hc_title(text='Residuos vs Ajustados')
      })
      
      
      output$homoscedasticityBox <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        Tabla <- data.frame(Estadastico=round(MBox$statistic,3),
                            ValP=signif(MBox$p.value,3))
        colnames(Tabla) <- c('Statistic Chi-Sq','p-value')
        Tabla
      })
      
      output$homoscedasticityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        if (MBox$p.value >= input$alpha){
          response=paste0('According to Boxs M-test for Homogeneity, the samples show equal variances')
        } else {
          response=paste0('According to Boxs M-test for Homogeneity, the samples show unequal variances')
        }
        response
      })
      
      output$MeetsHomoc <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        if(MBox$p.value >=  input$alpha ){
          return(paste("Homoscedasticity assumption: ","<span style=\"color:green;\"> if it complies.</span>"))
          
        }else{
          return(paste("Homoscedasticity assumption: ","<span style=\"color:red;\"> fails.</span>"))
        }
      })
      
      #________________________________________________________________
      
      output$PERMaov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        
        Factor <- as.factor(Data[,Ind])
        Depend <- as.matrix(Data[,Dep])
        
        
        DistanciasY<-dist(Depend)
        Data<-data.frame(Factor)
        
        Permanova1 <-adonis2(DistanciasY ~ Factor ,Data,
                             permutations = input$text)
        
        as.data.frame(Permanova1)
        
        
      })
      
      
      output$conclusionPERMaov <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        
        Factor <- as.factor(Data[,Ind])
        Depend <- as.matrix(Data[,Dep])
        
        
        DistanciasY<-dist(Depend)
        Data<-data.frame(Factor)
        
        Permanova1 <-adonis2(DistanciasY ~ Factor, Data,
                             permutations = as.numeric(input$text))
        
        if (Permanova1$`Pr(>F)`[1]>= input$alpha3){
          response=paste0('According to Permutation test, The set of Dependent 
                          variables does not have a significant effect on ',Ind)
        } else {
          response=paste0('According to Permutation test, the set of Dependent 
                          variables if it has a significant effect on ',Ind)
        }
        response
      })
      
      
      output$PerPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- Data[,Ind]
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor)
        P<-permutest(dispersionS)
        
        Tabhomg <- as.data.frame(P$tab)
        
        Tabhomg$F <- as.numeric(as.character(Tabhomg$F))
        Tabhomg$N.Perm <- as.numeric(as.character(Tabhomg$N.Perm))
        Tabhomg$`Pr(>F)` <- as.numeric(as.character(Tabhomg$`Pr(>F)`))
        TabhomgT <- data.frame(G=c(row.names(Tabhomg)[1]," "),                      
                               F=c(round(Tabhomg$F[1],3)," "),
                               NPerm=c(signif(Tabhomg$N.Perm[1],3)," "),
                               PrF=c(signif(Tabhomg$`Pr(>F)`[1])," "))
        
        TabhomgT
        
        
      })
      
      output$conclusionPerPostHoc <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- Data[,Ind]
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
        
        
        if (P$tab$`Pr(>F)`[1] < input$alpha){
          response=paste0('According to Permutation test for homogeneity of
                          multivariate dispersions, the homogeneity of the set 
                          of dependent variables is significant')
        } else {
          response=paste0('According to Permutation test for homogeneity of
                          multivariate dispersions, the homogeneity of the set 
                          of dependent variables is not significant')
          
        }
        response
      })
      
      output$Dispersions <- renderHighchart({
        
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(Data[,Ind])
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY <-dist(Depend)
        
        dispersionS <- betadisper(DistanciasY,group = Factor )
        
        
        PCoA1 <-dispersionS$vectors[,1]
        
        PCoA2 <- dispersionS$vectors[,2]
        
        pcdf<- data.frame(PCoA1,PCoA2,Factor)
        
        highchart() %>%
          
          hc_add_series(pcdf, "scatter", hcaes(x = PCoA1, y = PCoA2, group = Factor)) %>%
          hc_title(text = "") %>% 
          hc_chart(type = 'line') %>%
          hc_add_series(c(0,dispersionS$centroids[1]), name = 'centroide1', dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,dispersionS$centroids[2]), name = 'centroide2', dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,dispersionS$centroids[3]), name = 'centroide3', dataLabels = list(enabled = TRUE))%>%
          hc_add_series(c(0,dispersionS$centroids[4]), name = 'centroide4', dataLabels = list(enabled = TRUE))
        
        
      })
      
      
      #_________________________________________________________
      
      output$homogeneity <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        gp <- ggerrorplot(data =  Data, x = str(Ind),
                          y = names(Data[1:length(Dep)]),
                          merge = TRUE,
                          ylab = "Expression", 
                          add = "mean_sd", palette = "jco",
                          position = position_dodge(0.3)
        )
        
        pg <- as.data.frame(gp$data,Factor)
        
        highchart() %>%
          hc_add_series(pg, 
                        type = "scatter", 
                        hcaes(x = pg$.y., 
                              y = pg$.value., 
                              group = Factor)) %>% 
          hc_yAxis(opposite = TRUE) %>% 
          hc_tooltip(pointFormat = '{point.x}
                           {point.y: .4f}')
      })
      
      
      
      output$homogeneityPermutest <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
        
        Tabhomg <- as.data.frame(P$tab)
        
        Tabhomg$F <- as.numeric(as.character(Tabhomg$F))
        Tabhomg$N.Perm <- as.numeric(as.character(Tabhomg$N.Perm))
        Tabhomg$`Pr(>F)` <- as.numeric(as.character(Tabhomg$`Pr(>F)`))
        TabhomgT <- data.frame(G=c(row.names(Tabhomg)[1]," "),                      
                               F=c(round(Tabhomg$F[1],3)," "),
                               NPerm=c(signif(Tabhomg$N.Perm[1],3)," "),
                               PrF=c(signif(Tabhomg$`Pr(>F)`[1])," "))
        
        TabhomgT
        
      })
      
      output$homogeneityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
        
        
        if (P$tab$`Pr(>F)`[1] < input$alpha){
          response=paste0('According to Permutation test for homogeneity of
                          multivariate dispersions, the homogeneity of the set 
                          of dependent variables is significant')
        } else {
          response=paste0('According to Permutation test for homogeneity of
                          multivariate dispersions, the homogeneity of the set 
                          of dependent variables is not significant')
          
        }
        response
      })
      
      
      output$Meetshomog <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
        
        if(P$tab$`Pr(>F)`[1] < input$alpha){
          return(paste("Homogeneity assumption: ","<span style=\"color:green;\"> if it complies.</span>"))
          
        }else{
          return(paste("Homogeneity assumption: ","<span style=\"color:red;\"> Fails.</span>"))
        }
      })
      
      #__________________________________________________
      
      
      output$Box <- renderHighchart({
        
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        dfboxplot1 <-
          data_to_boxplot(Data,
                          as.matrix(Data[1]),
                          add_outliers = TRUE,
                          name = names(Data[1]),
                          color = 'teal')
        dfboxplot2 <-
          data_to_boxplot(Data,
                          as.matrix(Data[2]),
                          add_outliers = TRUE,
                          name = names(Data[2]),
                          color = 'red')
        dfboxplot3 <-
          data_to_boxplot(Data,
                          as.matrix(Data[3]),
                          add_outliers = TRUE,
                          name = names(Data[3]),
                          color = 'orange')
        dfboxplot4 <-
          data_to_boxplot(Data,
                          as.matrix(Data[4]),
                          add_outliers = TRUE,
                          name = names(Data[4]),
                          color = 'teal')
        dfboxplot5 <-
          data_to_boxplot(Data,
                          as.matrix(Data[6]),
                          add_outliers = TRUE,
                          name = names(Data[5]),
                          color = 'red')
        dfboxplot6 <-
          data_to_boxplot(Data,
                          as.matrix(Data[6]),
                          add_outliers = TRUE,
                          name = names(Data[6]),
                          color = 'orange')
        
        highchart() %>%
          hc_chart(type = 'boxplot') %>%
          hc_add_series_list(dfboxplot1) %>%
          hc_add_series_list(dfboxplot2) %>%
          hc_add_series_list(dfboxplot3) %>%
          hc_add_series_list(dfboxplot4) %>%
          hc_add_series_list(dfboxplot5) %>%
          hc_add_series_list(dfboxplot6) %>%
          hc_exporting(enabled = TRUE) %>%
          hc_plotOptions(series = list(animation = FALSE))
        
      })
      
      output$MAovBY <- DT::renderDataTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        Data<-data.frame(Depend,Factor)
        fit1 <- brms::brm(mvbind(brainrate_temporal, brainrate_frontal,
                                 brainrate_central, complexity_temporal,
                                 complexity_frontal,complexity_central)~Factor,save_all_pars = TRUE,
                          data = Data, chains = 1)
        
        
        SF <- summary(fit1)
        
        DT::datatable(SF$fixed, options = list(
          dom = 't',
          scrollX = TRUE,
          fixedColumns = TRUE,
          pageLength = nrow(SF$fixed)))%>% 
          DT::formatSignif(c("Estimate", "Est.Error", "l-95% CI","u-95% CI","Rhat","Bulk_ESS", "Tail_ESS"), 4)
        
        
        
        #fitNulo <- brm(mvbind(brainrate_temporal, brainrate_frontal,brainrate_central, complexity_temporal,complexity_frontal,complexity_central) ~ 1,
        #               save_all_pars = TRUE,
        #              data = EEGwide, chains = 3)
        
        
        
      })
      
      output$AovBYpost <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        Data=EEGwide
        Ind <- 'diagnosis'
        Dep <- c("brainrate_temporal" , "brainrate_frontal"  , "brainrate_central"  , "complexity_temporal", "complexity_frontal" ,
                 "complexity_central")
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        fit1 <- brms::brm(Dep~Ind,save_all_pars = TRUE,
                          data = Data, chains = 2)
        fit1$prior
        
      })
      
      output$MAovBYposmcmc <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        Data<-data.frame(Depend,Factor)
        
        fit1 <- brms::brm(mvbind(brainrate_temporal, brainrate_frontal,
                                 brainrate_central, complexity_temporal,
                                 complexity_frontal,complexity_central)~Factor,save_all_pars = TRUE,
                          data = Data, chains = 1)
        #SF <- posterior_samples(fit1) #DENSIDADES
        SF <- posterior_samples(fit1)
        
        SF <- SF[,c(1:12)]
        
        MCMC <- data.frame(SF,id=1:nrow(SF))
        
        
        
        MCMCMer <- melt(MCMC, id.vars="id")
        highchart()%>% 
          hc_add_series(MCMCMer, type='line', hcaes(x=id, y=value, group=variable))%>%
          hc_title(text='MCMC chains')%>%
          hc_exporting(enabled = TRUE,
                       filename = paste0('Markov chains'))
        
        
      })
      
      output$MAovBYposcurves <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        Data<-data.frame(Depend,Factor)
        fit1 <- brms::brm(mvbind(brainrate_temporal, brainrate_frontal,
                                 brainrate_central, complexity_temporal,
                                 complexity_frontal,complexity_central)~Factor,save_all_pars = TRUE,
                          data = Data, chains = 1)
        #SF <- posterior_samples(fit1) #DENSIDADES
        SF <- posterior_samples(fit1)
        
        SF <- SF[,c(1:12)]
        
        MCMC <- data.frame(SF,id=1:nrow(SF))
        
        MCMCMer <- melt(MCMC, id.vars="id")
        
        
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
      
      output$conclusionmaovby <- renderText({
        
        Data <- data()
        
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        ft1 <- (anovaBF(Depend ~ Factor, data=dataBY, whichRandom = "all", 
                        rscaleFixed = prio))
        
        
        S <- data.frame(Priori=prio, BF=Anovabyy[1])
        FB <- S[,2]
        if (FB <= 3 & FB > 1 ){
          response <- paste0('Weak evidence in favor of rejection of the null hypothesis ')
        } else if  (FB <= 10 & FB > 3 ) {
          response <- paste0('Evidencia moderada a favor del rechazo de la hipotesis nula ')
        } else if  (FB <= 30 & FB > 10 ){
          response <- paste0('Moderate evidence in favor of rejection of the null hypothesis ')
        }else if  (FB > 30 ){
          response <- paste0('Decisive evidence in favor of the rejection of the null hypothesis')
        }else if  (FB < 1 & FB > 1/3 ){
          response <- paste0('Moderate evidence in favor of the null hypothesis')
        }else if  (FB <= 1/3 & FB > 1/10 ){
          response <- paste0('Strong evidence in favor of the null hypothesis ')
        }else if  (FB <= 1/30 & FB > 1/100){
          response <- paste0('Decisive evidence in favor of the null hypothesis')
        }else if  (FB == 1){
          response <- paste0('There is no evidence')}
        
        response
      })
      
      
      output$diagramAssumptions <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- manova(Depend ~ Factor)
        
        
        Test <- mvn(Depend,desc = T,mvnTest = 'mardia')
        
        if(Test$multivariateNormality$Result[1] == 'NO' ){
          col_Normality= "#77DA85" 
          col_Normality_yes= "#77DA85"     
          col_Normality_no= "#D5D5D5"       
          
        }else{
          col_Normality= "#D5D5D5" 
          col_Normality_yes= "#D5D5D5"     
          col_Normality_no= "#77DA85"    
        }
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        
        if(MBox$p.value >=  input$alpha ){
          col_homoscedasticity= "#77DA85" 
          col_homoscedasticity_yes= "#77DA85"     
          col_homoscedasticity_no= "#D5D5D5"       
          
        }else{
          col_homoscedasticity= "#D5D5D5" 
          col_homoscedasticity_yes= "#D5D5D5"     
          col_homoscedasticity_no= "#77DA85"    
        }
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor)
        P<-permutest(dispersionS)
        
        if(P$tab$`Pr(>F)`[1] < input$alpha ){
          col_homogeneity= "#77DA85" 
          col_homogeneity_yes= "#77DA85"     
          col_homogeneity_no= "#D5D5D5"       
          
        }else{
          col_homogeneity= "#D5D5D5" 
          col_homogeneity_yes= "#D5D5D5"     
          col_homogeneity_no= "#77DA85"    
        }
        
        
        
        if (col_homogeneity_yes == "#77DA85"){
          col_pm="#77DA85"
        } else {col_pm="#D5D5D5" }
        
        
        if (col_Normality_yes== "#77DA85" & col_homoscedasticity_yes== "#77DA85" ){
          col_manova="#77DA85"
        }else {col_manova="#D5D5D5" }
        
        
        if (col_homogeneity_yes=="#77DA85" | col_homogeneity_no=="#77DA85"){
          col_homogeneity= "#77DA85"
        } else {col_pm="#D5D5D5"}
        
        if (col_homoscedasticity_yes=="#77DA85" | col_homoscedasticity_no=="#77DA85"){
          col_homoscedasticity= "#77DA85"
        } else {col_manova="#D5D5D5"}
        
        
        highchart() %>%
          hc_chart(type = 'organization', inverted = TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          list(from = 'Comparison by group', to = '?Does it comply with the normality assumption?'),
                          list(from = '?Does it comply with the normality assumption?', to = 'Yes, it is normal'),
                          list(from = 'Yes, it is normal', to = '?Does it meet the homoscedasticity assumption?'),
                          list(from = '?Does it comply with the normality assumption?', to = 'It does not meet normality'),
                          list(from = 'It does not meet normality', to = '?Does it meet homogeneity assumption?'),
                          list(from = '?Does it meet the homoscedasticity assumption?', to = 'Yes, it meets homoscedasticity'),
                          list(from = 'Yes, it meets homoscedasticity', to = '?Does it meet the homoscedasticity assumption?'),
                          list(from = '?Does it meet the homoscedasticity assumption?', to = 'Does not meet homoscedasticity'),
                          list(from = 'Does not meet homoscedasticity', to = '?Does it meet homogeneity assumption?'),
                          list(from = '?Does it meet homogeneity assumption?', to = 'Yes, it meets homogeneity'),
                          list(from = 'Yes, it meets homogeneity', to = '?Does it meet homogeneity assumption?'),
                          
                          list(from = '?Does it meet homogeneity assumption?', to = 'does not meet homogeneity')
                          
                        ),
                        nodes=  list(
                          list(id = 'Comparison by group', color="#77D0DA"),
                          list(id = '?Does it comply with the normality assumption?', color=col_Normality),
                          list(id = 'Yes, it is normal', color=col_Normality_yes),
                          list(id = 'It does not meet normality', color=col_Normality_no),
                          list(id = '?Does it meet the homoscedasticity assumption?', color=col_homoscedasticity),
                          list(id = 'Yes, it meets homoscedasticity', color=col_homoscedasticity_yes),
                          list(id = 'Does not meet homoscedasticity', color=col_homoscedasticity_no),
                          list(id = '?Does it meet homogeneity assumption?', color=col_homogeneity),
                          list(id = 'Yes, it meets homogeneity', color=col_homogeneity_yes),
                          list(id = 'does not meet homogeneity', color=col_homogeneity_no)
                          
                        ))
        
      })
      
      output$TechnicalChoice <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <-as.matrix(Data[,Dep])
        
        SA <- manova(Depend ~ Factor)
        
        Test <- mvn(Depend,desc = T,mvnTest = 'mardia')
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor)
        P<-permutest(dispersionS)
        
        if(Test$multivariateNormality$Result[1] == 'NO' & MBox$p.value >= input$alpha){
          col_manova="#77DA85"
        } else {col_manova="#DC7676"}
        
        if(P$tab$`Pr(>F)`[1] < input$alpha){
          col_pm="#77DA85"
        } else {col_pm="#DC7676"}
        
        highchart() %>%
          hc_chart(type = 'organization', inverted=TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          
                          list(from = 'PERMANOVA', to = 'PERMANOVA'),
                          list(from = 'Classic MANOVA', to = 'Classic MANOVA'),
                          list(from = 'Bayesian MANOVA', to = 'Bayesian MANOVA')
                          
                        ),
                        nodes=  list(
                          
                          list(id = 'Classic MANOVA', color=col_manova),
                          list(id = 'PERMANOVA', color=col_pm),
                          list(id = 'Bayesian MANOVA', color='#77DA85')
                        ))
        
      })
      
      
    })
  runApp(app)
}

library(MANOVA.RM)

data(EEGwide)
EEGwide
library(MVN)
library(heplots)
library(vegan)
library(MultBiplotR)
library(ggpubr)
manoby(EEGwide)
Data=EEGwide

