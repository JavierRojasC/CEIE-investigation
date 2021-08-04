
manovabay <- function(dataset=FALSE) {
  
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
  
  #Model <- "data {
  #        int<lower=0> N;
  #        int<lower=0> J;
  #        int<lower=1,upper=J> predictor[N];
  #        vector[N] response;
  #      }
  #      parameters {
  #        vector[J] eta;
  #        real mu;
  #        real<lower=0> sigmaalpha;
  #        real<lower=0> sigmaepsilon;
  #      }
  #      transformed parameters {
  #        vector[J] a;
  #        vector[N] yhat;
  #        
  #        a = mu + sigmaalpha * eta;
  #        
  #        for (i in 1:N)
  #          yhat[i] = a[predictor[i]];
  #      }
  #      model {
  #        eta ~ normal(0, 1);
  #        
  #        response ~ normal(yhat, sigmaepsilon);
  #      }"
  #
  ##rt <- stanc(file='https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/oneway.stan')
  #rt <- (stanc(model_code = Model))
  #sm <- stan_model(stanc_ret = rt, verbose=FALSE)
  
  left_footer <- fluidRow(
    column(
      width = 6,
      align = "left",
      a(
        href = "http://www.fcnm.espol.edu.ec/",
        target = "_blank",
        img(src = "https://github.com/JavierRojasC/JavierRCam/blob/master/fcnm.png?raw=true", height = "30px"),
        class = "dropdown",
        title = "Facultad de Ciencias Naturales y Matemáticas")
    )
  )
  
  app <- list(
    ui = dashboardPage(
      preloader = list(html = tagList(spin_three_bounce(), h3("Cargando ...")), color = "#003F63"),
      
      title =  '' ,
      dashboardHeader(title = "Analisis de Varianza Multivariante",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Base de Datos", tabName = "BD", startExpanded = TRUE,icon = icon("database")),
          menuItem("Supuestos", tabName = "Supuestos", startExpanded = TRUE,icon = icon("tasks")),
          menuItem("MANOVA Clasico", tabName = "MANOVAcl", startExpanded = TRUE,icon = icon("adn")),
          menuItem("PERMANOVA", tabName = "PERMANOVA", startExpanded = TRUE,icon = icon("kickstarter-k")),
          menuItem("MANOVA Bayesiano", tabName = "MANOVAby", startExpanded = TRUE,icon = icon("bold"))
          
          
          
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
                               box(fluidRow(
                                 column(6,fileInput("file1", "Subir base en csv",
                                                    accept = c(
                                                      "text/csv",
                                                      "comma-separated-values,text/plain",
                                                      ".csv")
                                 )),
                                 column(6,checkboxInput("header", "Presione si la primera fila contiene los nombres de las columnas", TRUE),
                                        radioButtons(inputId="separador",label="Separador",
                                                     choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                     selected = ','))
                               ),uiOutput('var')),
                               fluidRow(width=12,
                                        box(title="Vista de la base de datos",
                                            
                                            dataTableOutput("DTable")))
                               
                       ),
                       tabItem(tabName = "Supuestos",
                               sliderInput(inputId = 'alpha',
                                           label='Ingrese alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               box(title = 'Normalidad multivariante',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('normalidad',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2(textOutput('pruebaNorm')),
                                          tableOutput('normalidadmardia'),
                                          h3(textOutput('normalidadConclu')),
                                          h2(htmlOutput('CumpleNorm')))),
                               box(title = 'Homocedasticidad de varianza entre grupos',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('homocedasticidad',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Homocedasticidad por prueba MdeBox'),
                                          tableOutput('homocedasticidadBox'),
                                          h3(textOutput('homocedasticidadConclu')),
                                          h2(htmlOutput('CumpleHomoc')))),
                               box(title = 'Homogeneidad de dispersion entre grupos',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('homogeneidad',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                          
                                   column(12,
                                          h2('Homogeneidad de dispersion permutest'),
                                          tableOutput('homogeneidadPermutest'),
                                          h3(textOutput('homogeneidadConclu')),
                                          h2(htmlOutput('Cumplehomog')))),
                              
                               box(width = 12,collapsible = TRUE,
                                   withSpinner(highchartOutput('diagramaSupuestos',  height = "650px"), type = 7, color='#C7D5EB'),
                                   h2('Tecnica disponible'),
                                   withSpinner(highchartOutput('eleccionTecnica'), type = 7, color='#C7D5EB'))),
                       
                       tabItem(tabName = "MANOVAcl",
                               sliderInput(inputId = 'alpha2',
                                           label='Ingrese alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               
                               box(title = "Tabla MANOVA Clasico",collapsible = TRUE,
                                   tableOutput('MAov'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionMAov')),
                                   column(12,withSpinner(highchartOutput('Box',  height = "450px"), type = 7, color='#C7D5EB'))),
                            
                               box(title = "Post-Hoc",collapsible = TRUE,
                                   width=12,
                                   selectInput('padjust', 'Adjustment methods',
                                               c("Wilks", "Pillai","none")),
                        
                                   column(6,
                                          h3('post'),
                                          tableOutput('MAovPostHoc')),
                                   column(6,
                                          withSpinner(highchartOutput('MAovPostHocGraph',  height = "450px"), type = 7, color='#C7D5EB')))
                       ),
                       tabItem(tabName = "PERMANOVA",
                               sliderInput(inputId = 'Permutaciones',
                                           label='Ingrese numero de permutaciones',
                                           value=0.05,
                                           min=1,
                                           max=1000),
                               
                               box(title = "Tabla PERMANOVA",collapsible = TRUE,
                                   tableOutput('PERMaov'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionPERMaov'))),
                               
                               box(title = 'Permutest ',collapsible = TRUE,
                                   tableOutput('PerPostHoc'),
                                   h3('Conclusion')
                                   
                               )
                               
                       ),
                       tabItem(tabName = "MANOVAby",
                               
                               box(title = "Tabla MANOVA Bayesiano",collapsible = TRUE,
                                   tableOutput('MAovBY'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclusionmaovby'))),
                               box(title = 'Centro de control',collapsible = TRUE,
                                   sliderInput(inputId = 'prior',
                                               label='Ingrese probabilidad a priori',
                                               value=0.5,
                                               min=0,
                                               max=1),
                                   numericInput(inputId = 'numberiterations',
                                                label='Ingrese el numero de iteraciones',
                                                value=1000,
                                                min=500,
                                                max=3000),
                                   sliderInput(inputId = 'chainsnumber',
                                               label='Ingrese numero de cadenas:',
                                               value=1,
                                               min=1,
                                               max=4)),
                               box(title = "Posterior", width=12,collapsible = TRUE,
                                   column(12, align="center",tableOutput('AovBYpost'))),
                               box(title = "MCMC",collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          selectInput("mcmcCHAIN","Seleccione MCMC",
                                                      c("Media y Varianza",
                                                        "Tratamientos")),
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
      
      output$DTable <- renderDataTable({
        Data <- data()
        
      })
      
      output$var <- renderUI({
        
        if(is.null(data())){return()}
        
        else list (
          
          selectInput("y", "Variable dependiente", choices =    names(data()),multiple = TRUE),
          selectInput("x", "Variable independiente", choices = names(data()),multiple = TRUE)
          
          
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
        SA$stats
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
          response <- paste0('Existen diferencias significativas entre los grupos de ',Ind)
        } else if  (SA[[4]][11] > input$alpha2){
          response <- paste0('No existen diferencias significativas entre los grupos de ',Ind)}
        
        response
      })
      
      output$normalidad <- renderHighchart({
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
            title = list(text = "Residuos Estandarizados"),
            max=max(lineQQ$y2),
            min=min(lineQQ$y2))%>% 
          hc_xAxis(
            title = list(text = "Cuantiles Teoricos"))%>%
          hc_title(text='QQ plot')
      })
      
      output$MAovPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- summary(manova(Depend~Factor))
        SA$stats
        
        
      
      })
      
      output$MAovPostHocGraph <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- (manova(Depend~Factor))
        
        hcboxplot(x = Depend, var = Factor,
                  name = "Length", color = "#2980b9") %>%
          hc_add_theme(hc_theme_economist())
        
        #S <- as.data.frame(SA[[4]])
        #S <- signif(S,4)
        #S <- cbind(rownames(S),S)
        
        
        #names(S)[1] <- 'Trat'
        
        #hchart(pointWidth=0,type = 'columnrange',S,name='Intervalo',
        #       hcaes(x=Trat,high=upr, low=lwr), color='#224361')%>%
        #  hc_add_series(S, type='scatter', hcaes(x=Trat, y=diff), name='Diferencias', color='#289B9C',
        #                tooltip = list(pointFormat = "<br> Diferencia = {point.y}"))%>%
        #  hc_xAxis(title=list(text=('Combinaciones de tratamientos')))%>%
        #  hc_yAxis(title=list(text=('Diferencias')),
        #           plotLines = list(list(
        #             value = 0,
        #             color = '#DAE0EA',
        #             width = 3,
        #             zIndex = 4,
        #             label = list(text = "",
        #                          style = list( color = '#DAE0EA', fontWeight = 'bold' )))))
      })
      
      
  
      output$normalidadConclu <- renderText({
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
            response=paste0('Segun el test de Mardia, el conjunto de variables dependientes no son normales')
          } else {
            response=paste0('Segun el test de Mardia, el conjunto de variables dependientes son normales')
          }
          response
        } else {
          Test <- mshapiro.test(t(Data[,Dep]))
          if (Test$p.value >= input$alpha){
            response=paste0('Segun el test de Shapiro-Wilk, el conjunto de variables dependientes son normales')
          } else {
            response=paste0('Segun el test de Shapiro-Wilk, el conjunto de variables dependientes no son normales')
          }
          response
        }
      })
      
      output$pruebaNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        if (length(SA$residuals)>30){
          
          response=paste0('Normalidad por Test de Mardia')
          
          response
        } else {
          
          response=paste0('Normalidad por prueba de Shapiro-Wilk')
          
          response
        }
      })
      
      output$CumpleNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        SA <- (manova(as.matrix(Data[,Dep])~as.factor(as.matrix(Data[,Ind]))))
        
        if (length(SA$residuals)>30){
          Test <- mvn(Data[,Dep],mvnTest='mardia')
          
          if(Test$multivariateNormality$Result == 'NO'){
            return(paste("Supuesto de Normalidad: ","<span style=\"color:red;\"> No cumple.</span>"))
            
          }else{
            return(paste("Supuesto de Normalidad: ","<span style=\"color:green;\"> si cumple.</span>"))
          }} else {
            
            Test <- mshapiro.test(t(Data[,Dep]))
            
            if(Test$p.value >=  input$alpha ){
              return(paste("Supuesto de Normalidad: ","<span style=\"color:green;\"> SI cumple.</span>"))
              
            }else{
              return(paste("Supuesto de Normalidad: ","<span style=\"color:red;\"> No cumple.</span>"))
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
        
        SA <- boxM(Depend ~ Factor,data = Data)
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
      
      
      output$homocedasticidadBox <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data = Data)
        Tabla <- data.frame(Estadastico=signif(MBox$statistic,4),
                            ValP=signif(MBox$p.value,4))
        colnames(Tabla) <- c('Estadistico Chi-Sq','Valor-P')
        Tabla
      })
      
      
      output$homocedasticidadConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data = Data)
        
        if (MBox$p.value >= input$alpha){
          response=paste0('Segun Boxs M-test for Homogeneity, las muestras presentan varianzas iguales')
        } else {
          response=paste0('Segun Boxs M-test for Homogeneity, las muestras presentan varianzas desiguales')
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
        Depend <- as.matrix(Data[,Dep])
        
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        if(MBox$p.value >=  input$alpha ){
          return(paste("Supuesto de Homocedasticidad: ","<span style=\"color:green;\"> Si cumple.</span>"))
          
        }else{
          return(paste("Supuesto de Homocedasticidad: ","<span style=\"color:red;\"> No cumple.</span>"))
        }
      })
      #________________________________________________________________
      
      output$PERMaov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        Permanova1 <-adonis2(DistanciasY ~ Factor, 
                             data=Data, permutations = 999)
        Permanova1
        
        #  dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        #  colnames(dataBY) <- c('Ind2','Dep2')
        
        
        #  Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        #  DW <- durbinWatsonTest(Aov)
        #  Tabla <- data.frame(Autocor=DW[1],
        #                      Dw=signif(as.numeric(DW[2]),4),
        #                      ValP=signif(as.numeric(DW[3]),4))
        #  colnames(Tabla) <- c('Autocorrelación','D-W Statistic',
        #                       'p-value')
        #  Tabla
      })
      
      
      output$conclusionPERMaov <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
      
        
        DistanciasY<-dist(Depend)
        Permanova1 <-adonis2(DistanciasY ~ Factor, 
                             data=Data, permutations = 999)
        
        if (Permanova1$`Pr(>F)`[1]>= input$alpha){
          response=paste0('Segun el Permutation test, el conjunto de Variables 
                          dependientes no tiene efecto significativo en',Ind)
        } else {
          response=paste0('Segun el Permutation test, el conjunto de Variables 
          dependientes si tiene efecto significativo en',Ind)
        }
        response
      })
      
      
     #output$independenciaDurbin <- renderTable({
     #  Data <- data()
     #  Data <- na.omit(Data)
     #  Dep <- input$y
     #  Ind <- input$x
     #  
     #  
     #  Factor <- as.factor(as.matrix(Data[,Ind]))
     #  Depend <- as.matrix(Data[,Dep])
     #  
     #  dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
     #  colnames(dataBY) <- c('Ind2','Dep2')
     #  
     #  Aov <- aov(Dep2 ~ Ind2, data=dataBY)
     #  DW <- durbinWatsonTest(Aov)
     #  Tabla <- data.frame(Autocor=DW[1],
     #                      Dw=signif(as.numeric(DW[2]),4),
     #                      ValP=signif(as.numeric(DW[3]),4))
     #  colnames(Tabla) <- c('Autocorrelación','D-W Statistic',
     #                       'p-value')
     #  Tabla
     #})
     #
     #
     #output$independenciaConclu <- renderText({
     #  Data <- data()
     #  Data <- na.omit(Data)
     #  Dep <- input$y
     #  Ind <- input$x
     #  prio <- input$prior
     #  
     #  Factor <- as.factor(as.matrix(Data[,Ind]))
     #  Depend <- as.numeric(as.matrix(Data[,Dep]))
     #  
     #  dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
     #  colnames(dataBY) <- c('Ind2','Dep2')
     #  
     #  Aov <- aov(Dep2 ~ Ind2, data=dataBY)
     #  DW <- durbinWatsonTest(Aov)
     #  
     #  if (DW[3] >= input$alpha){
     #    response=paste0('Según el test de Durbin Watson, no existe presencia de autocorrelación en los residuos.')
     #  } else {
     #    response=paste0('Según el test de Durbin Watson, existe presencia de autocorrelación en los residuos.')
     #  }
     #  response
     #})
     #
     #
     #output$CumpleIndependencia <- renderText({
     #  Data <- data()
     #  Data <- na.omit(Data)
     #  Dep <- input$y
     #  Ind <- input$x
     #  prio <- input$prior
     #  
     #  
     #  Factor <- as.factor(as.matrix(Data[,Ind]))
     #  Depend <- as.numeric(as.matrix(Data[,Dep]))
     #  
     #  dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
     #  colnames(dataBY) <- c('Ind2','Dep2')
     #  Aov <- aov(Dep2 ~ Ind2, data=dataBY)
     #  DW <- durbinWatsonTest(Aov)
     #  
     #  if (DW[3] >= input$alpha){
     #    return(paste("Supuesto de Independencia: ","<span style=\"color:green;\"> Sí cumple.</span>"))
     #    
     #  }else{
     #    return(paste("Supuesto de Independencia: ","<span style=\"color:red;\"> No cumple.</span>"))
     #  }
     #})
      
      #_________________________________________________________
      
      output$homogeneidad <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        # Dep <- names(Data)[2]
        #  Ind <- names(Data)[1]
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
      
        hcboxplot(x = dispersionS$distances, var = dispersionS$group,
                  name = "Length", color = "#2980b9") %>%
          hc_add_theme(hc_theme_economist())
      })
      
      
      output$homogeneidadPermutest <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor )
        P<-permutest(dispersionS)
        
        
        Tabla <- P$tab
        Tabla
      })
      
      
      output$homogeneidadConclu <- renderText({
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
          response=paste0('Segun Permutation test for homogeneity of
                          multivariate dispersions, la Homogenidad del conjunto 
                          de variables dependientes es significativa')
        } else {
          response=paste0('Segun Permutation test for homogeneity of
                          multivariate dispersions, la Homogenidad del conjunto 
                          de variables dependientes no es significativa')
          
        }
        response
      })
      
      
      output$Cumplehomog <- renderText({
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
          return(paste("Supuesto de Homogeneidad: ","<span style=\"color:green;\"> Si cumple.</span>"))
          
        }else{
          return(paste("Supuesto de Homogeneidad: ","<span style=\"color:red;\"> No cumple.</span>"))
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
        
        
        hcboxplot(x = Depend, var = Factor,
                  name = "Length", color = "#2980b9") %>%
          hc_add_theme(hc_theme_economist())
        
        #hcboxplot(x=(as.matrix(Data[,Dep])), var=as.factor(as.matrix(Data[,Ind])), name = "Diagrama de cajas", color = "#0E1142", outliers = FALSE,
        #          showInLegend=TRUE)%>%
        #  hc_yAxis(title = list(text = Dep))%>% 
        #  hc_xAxis(title = list(text = "Niveles"))%>%
        #  hc_chart(type = "column")%>%
        #  hc_plotOptions(showInLegend=TRUE,dataLabels=TRUE)%>% 
        #  hc_add_series(Means, type='bubble', hcaes(x =Nombres,y=Media),maxSize = "7%",
        #                tooltip=list(pointFormat='<br> {point.y} ',headerFormat='<b> Media'), name='Medias',
        #                showInLegend=TRUE)
      })
      output$MAovBY <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- (as.matrix(Data[,Dep]))
        
        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
                             rscaleFixed = prio,iterations = input$numberiterations))
        
        
        
        S <- data.frame(Priori=(prio), BF=Anovabyy[1][1])
        TabBY <- S[,1:3]
        TabBY$BF.bf <- round(TabBY$BF.bf,3)
        TabBY$BF.error <- signif(TabBY$BF.error,3 )
        
        colnames(TabBY) <- c('Priori','BF10','Error')
        TabBY <- rbind(TabBY, c(1-prio,1,''))
        
        rownames(TabBY) <- c('Modelo Alternativo', 'Modelo Nulo')
        TabBY <- cbind(rownames(TabBY),TabBY)
        
        names(TabBY)[1] <- ''
        TabBY
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
        BD2 <- data.frame(Data[,Dep],Factor)
        #dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        #colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        #Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
        #                    rscaleFixed = prio,iterations = input$numberiterations))
        names(BD2[1:4])
        dtF <- list(y=(BD2[1:4]),x=as.factor(as.matrix(Data[,Ind])))
        dtF$y
        
        MV <- brms::mvbind(brainrate_temporal,brainrate_frontal)
        fit1 <- brms::brm(mvbind(BD2[1:4])~diagnosis,save_all_pars = TRUE,
                    data = Data, chains = 2)
        fit1
        plot(fit1, ask = FALSE)
        pulpdat <- list(N=length(dataBY$Dep2),J=length(unique(dataBY$Tratamiento)),response=dataBY$Dep2,predictor=as.numeric(dataBY$Tratamiento),lambda=lambda)
        
        fit <- sampling(sm, data=pulpdat, chains=input$chainsnumber,  seed = 12345,iter=input$numberiterations)
        fit.sum <- summary(fit, pars=c("mu","sigmaalpha","sigmaepsilon","a") )
        
        TablaPos <- data.frame(fit.sum$summary)
        rownames(TablaPos) <- c('Mu','Sigma Alpha','Sigma Epsilon',unique(as.character(dataBY$Tratamiento)))
        TablaPos2 <- data.frame(rownames(TablaPos),TablaPos)
        colnames(TablaPos2) <- c('','Mean','SE Mean', 'SD', '2.5%','25%','50%','75%','97.5%','n eff','R hat')
        TablaPos2
        #str(dataBY)
        #Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
        #                     rscaleFixed = prio,iterations = input$numberiterations))
        #
        #post <- summary(posterior(Anovabyy,iterations = input$numberiterations))
        #
        #S <- data.frame(post[1][1])
        #colnames(S) <- c('Media Posterior','Desv. Posterior','Naive SE','Time Series SE')
        #rownames(S)[1] <- 'Media General'
        #S <- cbind(rownames(S),S)
        #names(S)[1] <- ''
        #S
      })
      
      
      
      
      output$AovBYposmcmc <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        #dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        #colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        #Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
         #                    rscaleFixed = prio,iterations = input$numberiterations))
        
        fit1 <- brm(mvbind(Depend) ~ Factor,
                    save_all_pars = TRUE,
                    data = EEGwide, chains = 3)
        fit1
        
        post <- (posterior(Anovabyy,iterations = input$numberiterations))
        MCMC <- data.frame(Iteraci�n=1:input$numberiterations,post[,])
        
        if (input$mcmcCHAIN=="Media y Varianza"){
          
          highchart()%>% 
            hc_yAxis_multiples( list(top = "0%", height = "50%", title = list(text = "Media"),opposite=FALSE),
                                list(top = "50%", height = "50%", title = list(text = "Sigma2") ,opposite=TRUE))%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteraci�n,y=mu),yAxis=0, name='Media',color='#24509C')%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteraci�n,y=sig2),yAxis=1, name='Sigma2',color='#31999C')
        } else {
          
          MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
          rownames(MCMCCom) <- MCMC[,1]
          MCMCCom2 <- as.matrix(MCMCCom)
          MCMCMer <- melt(MCMCCom, id.vars="Iteración")
          highchart()%>% 
            hc_add_series(MCMCMer, type='line', hcaes(x=Iteraci�n, y=value, group=variable))%>%
            hc_title(text='MCMC chains')%>%
            hc_exporting(enabled = TRUE,
                         filename = paste0('Cadenas de Marcov'))
          
        }
      })
      
      
      #   output$AovBYposchains <- renderHighchart({
      #     Data <- data()
      #     Data <- na.omit(Data)
      #     Dep <- input$y
      #     Ind <- input$x
      #     prio <- input$prior
      #     
      #     
      #     Factor <- as.factor(as.matrix(Data[,Ind]))
      #     Depend <- as.numeric(as.matrix(Data[,Dep]))
      #     
      #     dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
      #     colnames(dataBY) <- c('Tratamiento','Dep2')
      #     #str(dataBY)
      #     Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
      #                          rscaleFixed = prio,iterations = input$numberiterations))
      #     
      #     post <- (posterior(Anovabyy,iterations = input$numberiterations))
      #     
      #     MCMC <- data.frame(Iteración=1:input$numberiterations,post[,])
      #     MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
      #     rownames(MCMCCom) <- MCMC[,1]
      #     MCMCCom2 <- as.matrix(MCMCCom)
      #     MCMCMer <- melt(MCMCCom, id.vars="Iteración")
      #     highchart()%>% 
      #      hc_add_series(MCMCMer, type='line', hcaes(x=Iteración, y=value, group=variable))%>%
      #       hc_title(text='MCMC chains')
      #   })
      #   
      output$AovBYposcurves <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior
        
        
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        
        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID", 
                             rscaleFixed = prio,iterations = input$numberiterations))
        
        post <- (posterior(Anovabyy,iterations = input$numberiterations))
        
        MCMC <- data.frame(Iteraci�n=1:input$numberiterations,post[,])
        MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
        MCMCMer <- melt(MCMCCom, id.vars="Iteración")
        
        
        ds <- map(levels(MCMCMer$variable), function(x){
          MCMCMer <- density(MCMCMer$value[MCMCMer$variable == x])[1:2]
          MCMCMer <- list_parse2(as.data.frame(MCMCMer))
          list(data = MCMCMer, name = x)
        })
        
        highchart() %>% 
          hc_add_series_list(ds)%>%
          hc_yAxis(title=list(text='Density'))%>%
          hc_exporting(enabled = TRUE,
                       filename = paste0('Curvas de densidad - Distribuciones marginales posteriores.'))
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
        
        
        # posterior(Anovabyy,iterations = 1000)
        # plot(Anovabyy)
        
        
        S <- data.frame(Priori=prio, BF=Anovabyy[1])
        FB <- S[,2]
        if (FB <= 3 & FB > 1 ){
          response <- paste0('Evidencia débil a favor del rechazo de la hipótesis nula ')
        } else if  (FB <= 10 & FB > 3 ) {
          response <- paste0('Evidencia moderada a favor del rechazo de la hipótesis nula ')
        } else if  (FB <= 30 & FB > 10 ){
          response <- paste0('Evidencia fuerte a favor del rechazo de la hipótesis nula ')
        }else if  (FB > 30 ){
          response <- paste0('Evidencia decisiva  a favor del rechazo de la hipótesis nula')
        }else if  (FB < 1 & FB > 1/3 ){
          response <- paste0('Evidencia moderada a favor de la hipótesis nula ')
        }else if  (FB <= 1/3 & FB > 1/10 ){
          response <- paste0('Evidencia fuerte a favor de la hipótesis nula ')
        }else if  (FB <= 1/30 & FB > 1/100){
          response <- paste0('Evidencia decisiva  a favor de de la hipótesis nula')
        }else if  (FB == 1){
          response <- paste0('No existe evidencia')}
        
        response
      })
      
      
      output$diagramaSupuestos <- renderHighchart({
        Data <- data()
        #Data = Datas
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        #Ind <- "boneDev"
        #Dep <- "growth"
        #alph <- input$alpha
        #alph <- 0.05
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        SA <- manova(Depend ~ Factor)
        
        
        
        Test <- mvn(Depend,desc = T,mvnTest = 'mardia')
        
        if(Test$multivariateNormality$Result[1] == 'NO' ){
          col_normalidad= "#77DA85" 
          col_normalidad_si= "#77DA85"     
          col_normalidad_no= "#D5D5D5"       
          
        }else{
          col_normalidad= "#D5D5D5" 
          col_normalidad_si= "#D5D5D5"     
          col_normalidad_no= "#77DA85"    
        }
        
        MBox <- boxM(Depend ~ Factor, data=Data)
        
        if(MBox$p.value >=  0.05 ){
          col_homocedasticidad= "#77DA85" 
          col_homocedasticidad_si= "#77DA85"     
          col_homocedasticidad_no= "#D5D5D5"       
          
        }else{
          col_homocedasticidad= "#D5D5D5" 
          col_homocedasticidad_si= "#D5D5D5"     
          col_homocedasticidad_no= "#77DA85"    
        }
        
        DistanciasY<-dist(Depend)
        dispersionS<- betadisper(DistanciasY,group = Factor)
        P<-permutest(dispersionS)
        
        if(P$tab$`Pr(>F)`[1] < 0.05 ){
          col_homogeneidad= "#77DA85" 
          col_homogeneidad_si= "#77DA85"     
          col_homogeneidad_no= "#D5D5D5"       
          
        }else{
          col_homogeneidad= "#D5D5D5" 
          col_homogeneidad_si= "#D5D5D5"     
          col_homogeneidad_no= "#77DA85"    
        }
        
    
        
        if (col_homogeneidad_si == "#77DA85"){
          col_pm="#77DA85"
        } else {col_pm="#D5D5D5" }
        
        #if (col_homocedasticidad_si == "#77DA85"){
        #  col_independencia="#77DA85"
        #} else {col_independencia="#D5D5D5" }
        
        #  if (col_independencia_no == "#77DA85" | col_independencia_si == "#77DA85"){
        #    col_independencia="#77DA85"
        #  } else {col_independencia="#D5D5D5" }
        
        if (col_normalidad_si== "#77DA85" & col_homocedasticidad_si== "#77DA85" ){
          col_manova="#77DA85"
        }else {col_manova="#D5D5D5" }
        
        
        if (col_homogeneidad_si=="#77DA85" | col_homogeneidad_no=="#77DA85"){
          col_homogeneidad= "#77DA85"
        } else {col_pm="#D5D5D5"}
        
        if (col_homocedasticidad_si=="#77DA85" | col_homocedasticidad_no=="#77DA85"){
          col_homocedasticidad= "#77DA85"
        } else {col_manova="#D5D5D5"}
        
        
        highchart() %>%
          hc_chart(type = 'organization', inverted = TRUE) %>%
          hc_add_series(name='Diagrama de tecnicas segun cumplimiento de supuestos',
                        data = list(
                          list(from = 'Comparacion por grupo', to = '�Cumple supuesto de normalidad?'),
                          list(from = '�Cumple supuesto de normalidad?', to = 'Si, cumple normalidad'),
                          list(from = 'Si, cumple normalidad', to = '�Cumple supuesto de homocedasticidad?'),
                          list(from = '�Cumple supuesto de normalidad?', to = 'No cumple normalidad'),
                          list(from = 'No cumple normalidad', to = '�Cumple supuesto de homogeneidad?'),
                          list(from = '�Cumple supuesto de homocedasticidad?', to = 'Si, cumple homocedasticidad'),
                          list(from = 'Si, cumple homocedasticidad', to = '�Cumple supuesto de homogeneidad?'),
                          list(from = '�Cumple supuesto de homogeneidad?', to = 'Si, cumple homogeneidad'),
                          list(from = '�Cumple supuesto de homogeneidad?', to = 'No cumple homogeneidad'),
                          
                          list(from = '�Cumple supuesto de homocedasticidad?', to = 'No cumple homocedasticidad'),
                          #list(from = '¿Cumple supuesto de simetría?', to = 'Sí, cumple simetría'),
                          #list(from = '¿Cumple supuesto de simetría?', to = 'No cumple simetría'),
                          #  list(from = 'Sí, cumple homocedasticidad', to = 'ANOVA Clásico'),
                          #list(from = 'No cumple normalidad', to = '¿Cumple supuesto de simetría?'),
                          list(from = 'No cumple homocedasticidad', to = '�Cumple supuesto de homogeneidad?')
                          #list(from = 'Sí, cumple simetría', to = 'Kruskal Wallis'),
                          #list(from = 'No cumple simetría', to = 'ANOVA Bayesiano'),
                          # list(from = 'Comparación de medias por grupo', to = 'ANOVA Bayesiano')
                          
                          
                          
                          
                          
                          
                        ),
                        nodes=  list(
                          list(id = 'Comparacion por grupo', color="#77D0DA"),
                          list(id = '�Cumple supuesto de normalidad?', color=col_normalidad),
                          list(id = 'Si, cumple normalidad', color=col_normalidad_si),
                          list(id = 'No cumple normalidad', color=col_normalidad_no),
                          list(id = '�Cumple supuesto de homocedasticidad?', color=col_homocedasticidad),
                          list(id = 'Si, cumple homocedasticidad', color=col_homocedasticidad_si),
                          list(id = 'No cumple homocedasticidad', color=col_homocedasticidad_no),
                          list(id = '�Cumple supuesto de homogeneidad?', color=col_homogeneidad),
                          list(id = 'Si, cumple homogeneidad', color=col_homogeneidad_si),
                          list(id = 'No cumple homogeneidad', color=col_homogeneidad_no)
                          #list(id = '¿Cumple supuesto de independencia?', color=col_independencia),
                          #list(id = 'Sí, cumple independencia', color=col_independencia_si),
                          #list(id = 'No cumple independencia', color=col_independencia_no)
                          #list(id = 'MANOVA Clasico', color=col_manova),
                          #list(id = 'PERMANOVA', color=col_pm),
                          #list(id = 'MANOVA Bayesiano', color='#77DA85'))
                        ))
        
      })
      
      
      
      output$eleccionTecnica <- renderHighchart({
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
          hc_add_series(name='Diagrama de tecnicas segun cumplimiento de supuestos',
                        data = list(
                          
                          list(from = 'PERMANOVA', to = 'PERMANOVA'),
                          list(from = 'MANOVA Clasico', to = 'MANOVA Clasico'),
                          list(from = 'MANOVA Bayesiano', to = 'MANOVA Bayesiano')
                          
                        ),
                        nodes=  list(
                          
                          list(id = 'MANOVA Clasico', color=col_manova),
                          list(id = 'PERMANOVA', color=col_pm),
                          list(id = 'MANOVA Bayesiano', color='#77DA85')
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
          response <- paste0('Existen diferencias significativas entre los grupos de ',Ind)
        } else if  (SA$p.value > input$alphakw){
          response <- paste0('No existen diferencias significativas entre los grupos de ',Ind)}
        
        response
      })
      
      output$KWpost <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.matrix(Data[,Dep])
        
        Pares <- pairwise.wilcox.test(x = Depend, g = Factor, p.adjust.method = input$padjust )
        Pv <- Pares$p.value
        Pv[is.na(Pv)] <- ' - '
        Pv <- cbind(rownames(Pv),Pv)
        Pv
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
manovabay(EEGwide)

