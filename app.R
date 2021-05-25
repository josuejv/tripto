## Aplicacion desarrollada por Josué Jiménez Vázquez
# Load packages ----
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")

# library(shiny)
# library(shinythemes)
# library(shinydashboard)
# library(tidyverse)
# library(ggthemes)
# library(shinyWidgets)
# library(DT)
# #library(heatmaply)
# library(plotly)
# library(lubridate)
# library(scales)
# library(xlsx)


# Source helper functions ----

source("functions.R")
source("values.R")
source("helpers.R")

# -----Create the data if don't exists----

createDF0()

#------Load data ----

 if(file.exists("rdas/Finanzas.rda")){
     load(file = "rdas/Finanzas.rda")
 }

 if(file.exists("rdas/nCapturas.rda")){
    load(file = "rdas/nCapturas.rda")
 }



# -------------_______UI______-------

ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, "Tripto", id = "nav",
                 
                 
                 ###### Here : insert shinydashboard dependencies ######
    header = tagList(
    useShinydashboard()
    ),

    #---- Panel Principal---------
    
    tabPanel("Principal",

             fluidRow(
                 box(width = 4,
                     fluidRow(
                            column(1),
                            column(6, uiOutput("dateUI")
                                   ),
                     
                            column(2,
                                   br(),
                                   actionButton("MesAnterior", "Anterior", width = 90)
                                   ),
                            column(2, 
                                   br(),
                                   actionButton("MesSiguiente", "Siguiente", width = 90)),
                            column(1)
                        ),
                     br(),
                     valueBoxOutput("boxIngresosMes", width = 12),
                     valueBoxOutput("boxEgresosMes", width = 12),
                     valueBoxOutput("boxDeudas", width = 12),
                     valueBoxOutput("boxActivos", width = 12)
                        ),
                 box(width = 8,
                     uiOutput("dateRangeUI"),
                     h4("BALANCE GENERAL", align = "center"),
                     plotlyOutput("BalanceGeneralPlot", height = '500px')
                    )
             ),

            #---------INGRESOS-------- 
            box(width = 6,
                title = h3("INGRESOS", align = "center"),
                collapsible = TRUE,
                collapsed = TRUE,
                box(width = 12,
                    title = "INGRESOS FIJOS",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("N_IF_Ingresos"),
                    uiOutput("IF_Ingresos"),
                    uiOutput("Btn_IF_Ingresos")
                ),
                box(width = 12,
                    title = "INGRESOS VARIABLES",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    uiOutput("N_IV_Ingresos"),
                    uiOutput("IV_Ingresos"),
                    uiOutput("Btn_IV_Ingresos")
                )
            ),
            #---------EGRESOS-------- 
            box(width = 6,
                title = h3("EGRESOS", align = "center"),
                collapsible = TRUE,
                collapsed = TRUE,
                box(width = 12,
                    #title = "MANTENCIÓN",
                    title = htmlOutput("PresMantencion"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Mantencion"),
                        uiOutput("GF_Mantencion"),
                        uiOutput("Btn_GF_Mantencion")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Mantencion"),
                        uiOutput("GV_Mantencion"),
                        uiOutput("Btn_GV_Mantencion")
                        )

                    ),

                box(width = 12,
                    #title = "DIVERSIÓN",
                    title = htmlOutput("PresDiversion"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Diversion"),
                        uiOutput("GF_Diversion"),
                        uiOutput("Btn_GF_Diversion")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Diversion"),
                        uiOutput("GV_Diversion"),
                        uiOutput("Btn_GV_Diversion")
                        )
                    ),
                                
                box(width = 12,
                    #title = "APRENDER",
                    title = htmlOutput("PresAprender"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Aprender"),
                        uiOutput("GF_Aprender"),
                        uiOutput("Btn_GF_Aprender")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Aprender"),
                        uiOutput("GV_Aprender"),
                        uiOutput("Btn_GV_Aprender")
                        )
                    ),
                box(width = 12,
                    #title = "VIAJAR",
                    title = htmlOutput("PresViajar"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Viajar"),
                        uiOutput("GF_Viajar"),
                        uiOutput("Btn_GF_Viajar")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Viajar"),
                        uiOutput("GV_Viajar"),
                        uiOutput("Btn_GV_Viajar")
                        )
                    ),

                box(width = 12,
                    #title = "INVERTIR",
                    title = htmlOutput("PresInvertir"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Invertir"),
                        uiOutput("GF_Invertir"),
                        uiOutput("Btn_GF_Invertir")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Invertir"),
                        uiOutput("GV_Invertir"),
                        uiOutput("Btn_GV_Invertir"))
                    ),
                box(width = 12,
                    #title = "OTROS",
                    title = htmlOutput("PresOtros"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    box(width = 12,
                        title = "GASTOS FIJOS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GF_Otros"),
                        uiOutput("GF_Otros"),
                        uiOutput("Btn_GF_Otros")
                        ),
                    box(width = 12,
                        title = "GASTOS VARIABLES",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GV_Otros"),
                        uiOutput("GV_Otros"),
                        uiOutput("Btn_GV_Otros")
                        ),
                    box(width = 12,
                        title = "GASTOS DEUDAS",
                        collapsible = TRUE,
                        collapsed = TRUE,
                        uiOutput("N_GD_Otros"),
                        uiOutput("GD_Otros")
                    )
                    
                    ),

        ),
    #---------ACTIVOS-------- 
    box(width = 6,
        title = h3("ACTIVOS", align = "center"),
        collapsible = TRUE,
        collapsed = TRUE,
        box(width = 12,
            title = "EFECTIVO",
            collapsible = TRUE,
            collapsed = TRUE,
            box(width = 12,
                title = "INICIAL",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput("Act_EF_Inicial"),
                uiOutput("Btn_Act_EF_Inicial")
                ),
            box(width = 12,
                title = "ACUMULADO",
                collapsible = TRUE,
                collapsed = TRUE,
                h3(textOutput("EfecAcumulado"))
                )
            
            ),
        box(width = 12,
            title = "INVERSIÓN",
            collapsible = TRUE,
            collapsed = TRUE,
            box(width = 12,
                title = "AGREGAR",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput("N_Inversion"),
                uiOutput("Inversion"),
                uiOutput("Btn_Inversion")
                )
            ),
        box(width = 12,
            title = "BIENES RAÍCES",
            collapsible = TRUE,
            collapsed = TRUE,
            box(width = 12,
                title = "AGREGAR",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput("N_Bienes_raices"),
                uiOutput("Bienes_raices"),
                uiOutput("Btn_Bienes_raices")
                )
            ),
        box(width = 12,
            title = "PROPIEDAD PERSONAL",
            collapsible = TRUE,
            collapsed = TRUE,
            box(width = 12,
                title = "AGREGAR",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput("N_Propiedad_P"),
                uiOutput("Propiedad_P"),
                uiOutput("Btn_Propiedad_P")
                )
            ),
        
        ),
    
    #---------DEUDAS-------- 
    box(width = 6,
        title = h3("DEUDAS", align = "center"),
        collapsible = TRUE,
        collapsed = TRUE,
        box(width = 12,
            title = "AGREGAR",
            collapsible = TRUE,
            collapsed = TRUE,
            uiOutput("N_Deudas"),
            uiOutput("Deudas"),
            uiOutput("Btn_Deudas")
            )
        )
    
    ),
    #---- Panel Ingresos---------
    tabPanel("Ingresos",
             fluidRow(
                   column(6,
                          tabsetPanel(
                              tabPanel("Concepto",
                                       DT::dataTableOutput("IngresosTable")
                                        ),
                               tabPanel("Categoria", DT::dataTableOutput("IngresosTable2"))
                          )
                          
                        
                    ),
                   column(6,
                        box(width = 12,
                            uiOutput("dateRangeIngresosUI1"),
                            h4("INGRESOS", align = "center"),
                            plotlyOutput("IngresosPlot", height = '350px')
                        )#,
                        # box(width = 12,
                        #     h4("", align = "center"),
                        #     plotlyOutput("IngresosPlot2", height = '350px')
                        # )
                    ),
                   box(width = 12,
                       h4("", align = "center"),
                       plotlyOutput("IngresosPlot2", height = '350px')
                   ),
                   
                   box(width = 12,
                       h4("", align = "center"),
                       plotlyOutput("IngresosPlot3", height = '350px')
                   )
                   
                   
                )
             ),
    #---- Panel Egresos---------
    tabPanel("Egresos",
             fluidRow(
                 column(6,
                        tabsetPanel(
                          tabPanel("Concepto",
                                   DT::dataTableOutput("EgresosTable")
                          ),
                          tabPanel("Categoria", DT::dataTableOutput("EgresosTable2"))
                        )
      
                 ),
                 column(6,
                        box(width = 12,
                            uiOutput("dateRangeEgresosUI1"),
                            h4("EGRESOS", align = "center"),
                            plotlyOutput("EgresosPlot", height = '350px')
                        ),
                        box(width = 12,
                            h4("EGRESOS", align = "center"),
                            plotlyOutput("EgresosPlot2", height = '350px')
                        ) #,
                        # box(width = 12,
                        #     h4("", align = "center"),
                        #     plotlyOutput("EgresosPlot3", height = '350px')
                        # )
                    ),
                 box(width = 12,
                     h4("", align = "center"),
                     plotlyOutput("EgresosPlot3", height = '350px')
                 )
                 
                 
                )
             ),
    #---- Panel Activos---------
    tabPanel("Activos",
             fluidRow(
                 column(6,
                        DT::dataTableOutput("ActivosTable")
                 ),
                 column(6,
                        box(width = 12,
                            uiOutput("dateRangeActivosUI1"),
                            h4("ACTIVOS", align = "center"),
                            plotlyOutput("ActivosPlot", height = '350px')
                        ),
                    ) 
                )
             ),
    #---- Panel Deudas---------
    tabPanel("Deudas",
             fluidRow(
                 column(6,
                        DT::dataTableOutput("DeudasTable")
                 ),
                 column(6,
                        box(width = 12,
                            uiOutput("dateRangeDeudasUI1"),
                            h4("DEUDAS", align = "center"),
                            plotlyOutput("DeudasPlot", height = '350px')
                        ),
                    ) 
                )
             ),
    #---- Panel Balance---------
    tabPanel("Balance",
             column(2),
             column(8,
                    DT::dataTableOutput("BalanceTable")
             ),
             column(2)
    )
    
)

### ----_____SERVER_____----

server <- function(input, output, session) {
    
    #----UI para hacer reactivo al mes----
    
    mes <- reactiveValues(mes = Sys.Date(),
                          anterior = NULL,
                          seleccion = NULL)
    
    observeEvent(input$MesSiguiente,
                 {
                     if(!is.null(mes$seleccion)){
                         d <- day(mes$seleccion)
                         y <- year(mes$seleccion)
                         m <- month(mes$seleccion)
                         mes$seleccion <- NULL
                     } else {
                         d <- day(mes$mes)
                         y <- year(mes$mes)
                         m <- month(mes$mes)
                     }
                     if(m == 12){
                         y <- y+1
                         m <- 1
                     } else {
                         m <- m+1
                     }
                     mes$anterior <- mes$mes
                     mes$mes <- as.Date(paste0(y, "-", m, "-", d))
                 })
    
    observeEvent(input$MesAnterior,
                 {
                     if(!is.null(mes$seleccion)){
                         d <- day(mes$seleccion)
                         y <- year(mes$seleccion)
                         m <- month(mes$seleccion)
                         mes$seleccion <- NULL
                     } else {
                         d <- day(mes$mes)
                         y <- year(mes$mes)
                         m <- month(mes$mes)
                     }
                     if(m == 1){
                         y <- y-1
                         m <- 12
                     } else {
                         m <- m-1
                     }
                     mes$anterior <- mes$mes
                     mes$mes <- as.Date(paste0(y, "-", m, "-", d))
                 })
    
    
    output$dateUI <- renderUI({
        mes <- mes$mes
        dateInput("date", "MES", value = mes, format = "MM-yyyy", language = "es")
    })
    
    
    output$dateRangeUI <- renderUI({
        dat <- datBalance()
        dat <- dat$Balance
        
        if(!is.null(dat)){
            minDate <- min(dat$Fecha)
            maxDate <- max(dat$Fecha)
        } else {
            minDate <- mes$mes
            maxDate <- mes$mes
        }
        
        
        dateRangeInput("dateRange", "PERIODO", start = minDate, end = maxDate, format = "MM-yyyy", language = "es")
    })
    
    observe({
        if(!is.null(input$date)){
            mesS <- input$date
            mes$seleccion <- mesS
        }else{
            mesS <- mes$mes
        }
        datF <- dataFiltered()
        datnCF <- datanCapturasFiltered()
        lapply(1:length(var_names$nombre), UIcreatorCaptures, output, input, mesS, datF, datnCF)
        lapply(1:length(var_names$nombre), UIcreatorNo, output, mesS, datnCF)
    })

    observeEvent(input$MesSiguiente,{
        mes <- mes$mes
        datF <- dataFiltered()
        datnCF <- datanCapturasFiltered()
        lapply(1:length(var_names$nombre), UIcreatorCaptures, output, input, mes, datF, datnCF)
        lapply(1:length(var_names$nombre), UIcreatorNo, output, mes, datnCF)
    })

    observeEvent(input$MesAnterior,{
        mes <- mes$mes
        datF <- dataFiltered()
        datnCF <- datanCapturasFiltered()
        lapply(1:length(var_names$nombre), UIcreatorCaptures, output, input, mes, datF, datnCF)
        lapply(1:length(var_names$nombre), UIcreatorNo, output, mes, datnCF)
    })

    lapply(1:length(var_names$nombre), UIcreatorBtns, output, input)
    
    ##----Values Boxes del resumen-------
    
    output$boxIngresosMes <- renderValueBox({
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        y <- year(mes)
        m <- month(mes)
        dat <- datBalance()
        dat <- dat$Ingresos
        
        if(!is.null(dat)){
            
          Ingreso <- dat %>% filter(year == y, month == m) #%>% pull(Egresos)
            if(length(Ingreso$Ingresos) == 0) {
                Ingreso <- 0
            } else {
                Ingreso <- Ingreso %>% pull(Ingresos)
            }  
            
        } else {
            
            Ingreso <-0
        }
        
        
        valueBox(
            prettyNum(round(Ingreso, digits = 2), big.mark = ","), "Ingreso", 
            icon = icon("money-bill", lib = "font-awesome"),
            color = "green", width = 12
        )
    })
    
    output$boxEgresosMes <- renderValueBox({
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        y <- year(mes)
        m <- month(mes)
        dat <- datBalance()
        dat <- dat$Egresos
        
        if(!is.null(dat)){
            Egreso <- dat %>% filter(year == y, month == m) #%>% pull(Egresos)
            if(length(Egreso$Egresos) == 0) {
                Egreso <- 0
            } else {
                Egreso <- Egreso %>% pull(Egresos)
            }
        } else {
            Egreso <- 0
        }
        
        
        valueBox(
            prettyNum(round(Egreso, digits = 2), big.mark = ","), "Egreso", 
            icon = icon("balance-scale-left", lib = "font-awesome"),
            color = "orange", width = 12
        )
    })
    
    output$boxDeudas <- renderValueBox({
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        y <- year(mes)
        m <- month(mes)
        dat <- datBalance()
        dat <- dat$Deudas
        
        if(!is.null(dat)){
            Deudas <- dat %>% filter(year == y, month == m) #%>% pull(Egresos)
            if(length(Deudas$Deudas) == 0) {
                Deudas <- 0
            } else {
                Deudas <- Deudas %>% pull(Deudas)
            }
        } else {
            Deudas <- 0
        }
        
        
        
        valueBox(
            prettyNum(round(Deudas, digits = 2), big.mark = ","), "Deudas", 
            icon = icon("credit-card", lib = "font-awesome"),
            color = "red", width = 12
        )
    })
    
    output$boxActivos <- renderValueBox({
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        y <- year(mes)
        m <- month(mes)
        dat <- datBalance()
        dat <- dat$Activos
        
        if(!is.null(dat)){
            Activos <- dat %>% filter(year == y, month == m) #%>% pull(Egresos)
            if(length(Activos$Activos) == 0) {
                Activos <- 0
            } else {
                Activos <- Activos %>% pull(Activos)
            }
        } else {
           Activos <- 0 
        }
        
        
        valueBox(
            prettyNum(round(Activos, digits = 2), big.mark = ","), "Activos", 
            icon = icon("landmark", lib = "font-awesome"),
            color = "blue", width = 12
        )
    })
    
    ##----Para cargar los datos guardados y que se actualicen---------
    
    #----__finanzas_totales__
    
    finanzas_totales <- reactiveValues(datosAgregados = NULL,
                                       nCapturas = NULL,
                                       datosCapturados = NULL,
                                       datosCapturadosN = NULL,
                                       indicesAgregados = NULL)
    
    lapply(1:length(var_names$nombre), BtnsEvents, input, output, finanzas_totales, input$date, dataFiltered(), datanCapturasFiltered(), 
           Finanzas, nCapturas)
    
    ##----Para ver los datos de cada mes----
    ##----___dataFiltered___----
    dataFiltered <- reactive({
        if(!is.null(input$date)){
            mes <- input$date
        }else{
            mes <- mes$mes
        }
        if (is.null(finanzas_totales$datosAgregados)){
            dat <- Finanzas
        } else {
            dat <- finanzas_totales$datosAgregados
        }
        dataF <- lapply(1:length(var_names$nombre), dataFilter, mes, dat)
        dataF
    })
    
    ##------Para las capturas guardadas y filtradas-----
    
    datanCapturasFiltered <- reactive({
        
        if(!is.null(input$date)){
            mes <- input$date
        }else{
            mes <- mes$mes
        }

        if (is.null(finanzas_totales$nCapturas)){
            dat <- nCapturas
        } else {
            dat <- finanzas_totales$nCapturas
        }
        dataF <- lapply(1:length(var_names$nombre), dataFilter, mes, dat)
        dataF
    })
    
    ##-------Para los datos de las tablas---------
    
    datTables <- reactive({
        
        if (is.null(finanzas_totales$datosAgregados)){
            dat <- Finanzas
        } else {
            dat <- finanzas_totales$datosAgregados
        }
        
        Ingresos <- dat %>% filter(Seccion == "Ingresos") %>% select(date, Seccion, Type, categoria, concepto, captura)
        Egresos <- dat %>% filter(str_detect(Type, "Gasto")) %>% select(date, Seccion, Type, categoria, concepto, captura)
        Activos <- dat %>% filter(Type %in% c("Activo Efectivo Inicial",
                                                 "Inversion",
                                                 "Bienes Raices",
                                                 "Propiedad Personal")) %>% 
            select(date, Seccion, Type, categoria, concepto, captura, rendimiento)
        Deudas <- dat %>% filter(Seccion == "Deudas") %>% select(date, Seccion, Type, categoria, concepto, captura, interes)
        
        if(!is.null(Egresos)){
            EgresosConcepto <- Egresos %>% group_by(concepto) %>% summarise(cantidad = sum(captura), .groups = 'drop')
        }
        if(!is.null(Ingresos)){
            IngresosConcepto <- Ingresos %>% group_by(concepto) %>% summarise(cantidad = sum(captura), .groups = 'drop')
        }
        if(!is.null(Ingresos)){
          IngresosCategoria <- Ingresos %>% group_by(categoria) %>% summarise(cantidad = sum(captura), .groups = 'drop')
        }
        if(!is.null(Deudas)){
            DeudasConcepto <- Deudas %>% group_by(concepto) %>% summarise(cantidad = sum(captura), .groups = 'drop')
        }
        if(!is.null(Activos)){
            ActivosConcepto <- Activos %>% group_by(concepto) %>% summarise(cantidad = sum(captura), .groups = 'drop')
        }

        dat <- list(
            Ingresos = Ingresos,
            Egresos = Egresos,
            Activos = Activos,
            Deudas = Deudas,
            EgresosConcepto = EgresosConcepto,
            IngresosConcepto = IngresosConcepto,
            IngresosCategoria = IngresosCategoria,
            DeudasConcepto = DeudasConcepto,
            ActivosConcepto = ActivosConcepto
            
        )
        
        dat
        
    })
    
    
  ##----Tablas------  
    output$IngresosTable <- DT::renderDataTable({
        dat <- datTables()
        dat <- dat$Ingresos
        dat <- dat %>% 
          rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura) %>%
          arrange(desc(Fecha))
        
        if(!is.null(dat)){
            DT::datatable(dat, options = list(
                scrollY = '400px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
        }
        
    })
    
    output$IngresosTable2 <- DT::renderDataTable({
        dat <- datTables()
        dat <- dat$Ingresos
        dat <- dat %>% rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura) %>%
          mutate(Año = year(Fecha), Mes = month(Fecha))
        
        dat <- dat %>% group_by(Año, Mes, `Sección`, Tipo, Categoria) %>% 
          summarise(Monto = sum(Monto), .groups = 'drop') %>%
          arrange(desc(Año), desc(Mes))%>%
          mutate(Mes = str_to_title(month(Mes, label = TRUE, abbr = FALSE)))
        
        if(!is.null(dat)){
            DT::datatable(dat, options = list(
                scrollY = '400px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
        }

    })
    
    
    output$EgresosTable <- DT::renderDataTable({
        dat <- datTables()
        dat <- dat$Egresos
        dat <- dat %>% rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura) %>%
          arrange(desc(Fecha))
        if(!is.null(dat)){
            DT::datatable(dat, options = list(
                scrollY = '700px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
        }
        
    })
    
    output$EgresosTable2 <- DT::renderDataTable({
      dat <- datTables()
      dat <- dat$Egresos
      dat <- dat %>% rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura) %>%
        mutate(Año = year(Fecha), Mes = month(Fecha))
      
      dat <- dat %>% group_by(Año, Mes, `Sección`, Tipo, Categoria) %>% 
        summarise(Monto = sum(Monto), .groups = 'drop') %>%
        arrange(desc(Año), desc(Mes))%>%
        mutate(Mes = str_to_title(month(Mes, label = TRUE, abbr = FALSE)))
      
      if(!is.null(dat)){
        DT::datatable(dat, options = list(
          scrollY = '700px', paging = FALSE),
          rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
      }
      
    })
    
    
    
    output$ActivosTable <- DT::renderDataTable({
        dat <- datTables()
        dat <- dat$Activos
        dat <- dat %>% rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura, Rendimiento = rendimiento) %>%
          arrange(desc(Fecha))
        if(!is.null(dat)){
            DT::datatable(dat, options = list(
                scrollY = '700px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
        }
    })
    
    output$DeudasTable <- DT::renderDataTable({
        dat <- datTables()
        dat <- dat$Deudas
        dat <- dat %>% rename(Fecha = date, "Sección" = Seccion, Tipo = Type, Categoria = categoria, Concepto = concepto, Monto = captura, "Interés" = interes) %>%
          arrange(desc(Fecha))
        if(!is.null(dat)){
            DT::datatable(dat, options = list(
                scrollY = '700px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = "Monto", digits = 2)
        }
        
    })
    
    
    output$BalanceTable <- DT::renderDataTable({
        dat <- datBalance()
        dat <- dat$BalanceTable
        
        if(!is.null(dat)){
            
            dat <- dat %>% select(Fecha, Ingresos, Egresos, "Ingresos-Egresos", Deudas, Efectivo, Activos, "Patrimonio Neto") %>%
              arrange(desc(Fecha))
            
            DT::datatable(dat, options = list(
                scrollY = '700px', paging = FALSE),
                rownames = FALSE) %>% formatRound(columns = c(2:length(dat)), digits = 2)
        }
        
    })
    
    ##-------datBalance---------
    
    datBalance <- reactive({
        
        if (is.null(finanzas_totales$datosAgregados)){
            dat <- Finanzas
        } else {
            dat <- finanzas_totales$datosAgregados
        }
        
        
        if(!is.na(dat[1,1]) | is.null(dat)){

            Ingresos <- dat %>% filter(Seccion == "Ingresos") %>%
            select(year, month, captura) %>%
            group_by(year, month) %>%
            summarise(Ingresos = sum(captura), .groups = 'drop')

            Egresos <- dat %>% filter(str_detect(Type, "Gasto")) %>%
                group_by(year, month) %>%
                summarise(Egresos = sum(captura), .groups = 'drop')

                EfectivoInicial <- dat %>% filter(Seccion == "Efectivo Inicial") 
                
            if(length(EfectivoInicial$Seccion) == 0) {
                EfectivoInicial <- 0
            } else {
                EfectivoInicial <- EfectivoInicial %>% pull(captura)
            }

            Activos <- dat %>% filter(Type %in% c("Inversion",
                                                  "Bienes Raices",
                                                  "Propiedad Personal")) %>%
                group_by(year, month) %>%
                summarise(Activos = sum(captura), .groups = 'drop')
            
        Deudas <- dat %>% filter(Seccion == "Deudas") %>% 
            group_by(year, month) %>%
            summarise(Deudas = sum(captura), .groups = 'drop')

            Balance <- merge(Ingresos, Egresos, all = TRUE)
            Balance <- merge(Balance, Activos, all = TRUE)
            Balance <- merge(Balance, Deudas, all = TRUE)
            
            Balance <- Balance %>% gather(Feature, Value, -year, -month)
            Balance <- Balance %>% mutate(Value = ifelse(is.na(Value), 0, Value))
            
            Balance <- Balance %>% spread(Feature, Value)
            
            if(!is.na(Balance$year[1])){
                Balance <- Balance %>% mutate(ImenosE = Ingresos - Egresos,
                                      Efectivo = cumsum(ImenosE) + EfectivoInicial, 
                                      Activos = Efectivo + Activos,
                                      "Patrimonio Neto" = Activos - Deudas,
                                      Fecha = as.Date(paste0(ifelse(month+1 <= 12, year, year+1), "-", ifelse(month+1 <= 12, month+1, 1) , "-", 1)))

            
                Balance <- Balance %>% rename("Ingresos-Egresos" = ImenosE)
            
                Efectivo <- Balance %>% select(year, month, Efectivo)
                Efectivo <- Efectivo %>% rename(Activos = Efectivo)
            
                Activos <- rbind(Activos, Efectivo)
            
                Activos <- Activos %>% 
                    group_by(year, month) %>%
                    summarise(Activos = sum(Activos), .groups = 'drop')
            
            
                Balance <- Balance %>% gather(Feature, Value, -year, -month, -Fecha)
                Balance <- Balance %>% mutate(Value = ifelse(is.na(Value), 0, Value))
            
                BalanceTable <- Balance %>% spread(Feature, Value) #%>%
                #select()

                Balance <- list(
                    Balance = Balance,
                    BalanceTable = BalanceTable,
                    Ingresos = Ingresos,
                    Egresos = Egresos,
                    Activos = Activos,
                    Deudas = Deudas
                )
                
                
            } else {
                Balance <- NULL
            }

            
 
            
        } else {
            
            Balance <- NULL
            
        }
        
        Balance
    })
    
    
    output$BalanceGeneralPlot <- renderPlotly({
        
        inicio <- input$dateRange[1]
        fin <- input$dateRange[2]
        dat <- datBalance()
        dat <- dat$Balance
        
        if(!is.null(dat)){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(Fecha <= fin & Fecha >= inicio)
                }
            }
            
            BalancePlot(dat)
        }
    })
    
    #------Graficas de los paneles-------
    
    output$dateRangeIngresosUI1 <- renderUI({
        dat <- datTables()
        dat <- dat$Ingresos
        
        
        if(is.Date(dat$date[1]) & !is.na(dat$date[1])){
            minDate <- min(dat$date)
            maxDate <- max(dat$date)
        } else {
            minDate <- mes$mes
            maxDate <- mes$mes
        }
        
        dateRangeInput("dateRangeIngresos", "PERIODO", start = minDate, end = maxDate, format = "MM-yyyy", language = "es")
    })
    
    output$IngresosPlot <- renderPlotly({
        
        inicio <- input$dateRangeIngresos[1]
        fin <- input$dateRangeIngresos[2]
        
        dat <- datTables()
        dat <- dat$Ingresos
        
        
        
        if(!is.null(dat) & !is.na(dat$captura[1])){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(date <= fin & date >= inicio)
                }
            }
            ConceptPlot(dat, "Ingresos")
        }
    })
    
    
    output$IngresosPlot2 <- renderPlotly({
        
        dat <- datTables()
        dat <- dat$IngresosConcepto
        
        if(!is.null(dat)){
            BarPlot(dat)
        }
    })
    
    output$IngresosPlot3 <- renderPlotly({
      
      dat <- datTables()
      dat <- dat$IngresosCategoria
      
      if(!is.null(dat)){
        BarPlot2(dat)
      }
    })
    
    
    output$dateRangeEgresosUI1 <- renderUI({
        dat <- datTables()
        dat <- dat$Egresos
        
        if(is.Date(dat$date[1]) & !is.na(dat$date[1])){
            minDate <- min(dat$date)
            maxDate <- max(dat$date)
        } else {
            minDate <- mes$mes
            maxDate <- mes$mes
        }
        
        dateRangeInput("dateRangeEgresos", "PERIODO", start = minDate, end = maxDate, format = "MM-yyyy", language = "es")
    })

    
    output$EgresosPlot <- renderPlotly({
        
        inicio <- input$dateRangeEgresos[1]
        fin <- input$dateRangeEgresos[2]
        
        dat <- datTables()
        dat <- dat$Egresos
        
        
        
        if(!is.null(dat) & !is.na(dat$captura[1])){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(date <= fin & date >= inicio)
                }
            }
            
            TypePlot(dat)
        }
    })
    
    
    output$EgresosPlot2 <- renderPlotly({
        
        inicio <- input$dateRangeEgresos[1]
        fin <- input$dateRangeEgresos[2]
        
        dat <- datTables()
        dat <- dat$Egresos

        if(!is.null(dat) & !is.na(dat$captura[1])){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(date <= fin & date >= inicio)
                }
            }
            ConceptPlot(dat, "Egresos")
        }
    })
    
    
    output$EgresosPlot3 <- renderPlotly({
        
        dat <- datTables()
        dat <- dat$EgresosConcepto
        
        if(!is.null(dat)){
            BarPlot(dat)
        }
    })
    
    output$dateRangeDeudasUI1 <- renderUI({
        dat <- datTables()
        dat <- dat$Deudas
        if(is.Date(dat$date[1]) & !is.na(dat$date[1])){
            minDate <- min(dat$date)
            maxDate <- max(dat$date)
        } else {
            minDate <- mes$mes
            maxDate <- mes$mes
        }
        dateRangeInput("dateRangeDeudas", "PERIODO", start = minDate, end = maxDate, format = "MM-yyyy", language = "es")
    })
    
    
    output$DeudasPlot <- renderPlotly({
        
        inicio <- input$dateRangeDeudas[1]
        fin <- input$dateRangeDeudas[2]
        
        dat <- datTables()
        dat <- dat$Deudas
        
        
        
        if(!is.null(dat) & !is.na(dat$captura[1]) ){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(date <= fin & date >= inicio)
                }
            }
            ConceptPlot(dat, "Deudas")
        }
    })
    
    output$dateRangeActivosUI1 <- renderUI({
        dat <- datTables()
        dat <- dat$Activos
        if(is.Date(dat$date[1]) & !is.na(dat$date[1])){
            minDate <- min(dat$date)
            maxDate <- max(dat$date)
        } else {
            minDate <- mes$mes
            maxDate <- mes$mes
        }
        dateRangeInput("dateRangeActivos", "PERIODO", start = minDate, end = maxDate, format = "MM-yyyy", language = "es")
    })
    

    output$ActivosPlot <- renderPlotly({
        
        inicio <- input$dateRangeActivos[1]
        fin <- input$dateRangeActivos[2]
        
        dat <- datTables()
        dat <- dat$Activos
        
        

        if(!is.null(dat) & !is.na(dat$captura[1])){
            if(!is.null(inicio) & !is.null(fin)){
                if(fin > inicio){
                    dat <- dat %>% filter(date <= fin & date >= inicio)
                }
            }
            AreaPlot(dat)
        }
    })
    
    ##---- Presupuesto----
    
    datPresupuesto <- reactive({
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        y <- year(mes)
        m <- month(mes)
        
        dat <- datBalance()
        dat <- dat$Ingresos
        
        dat2 <- datTables()
        dat2 <- dat2$Egresos
        
        
        if(!is.null(dat)){
            
            Ingreso <- dat %>% filter(year == y, month == m) #%>% pull(Egresos)
            if(length(Ingreso$Ingresos) == 0) {
                Ingreso <- 0
            } else {
                Ingreso <- Ingreso %>% pull(Ingresos)
            }  
            
        } else {
            
            Ingreso <-0
        }
        
        if(!is.null(dat2)){
           
        
            
            Egreso <- dat2 %>% mutate(year = year(date), month = month(date)) 
            
          
            Egreso <- Egreso %>% filter(year == y, month == m) #%>% pull(Egresos)
           
            
            if(length(Egreso$captura) == 0) {
                Egreso <- 0
            }  
            
        } else {
            
            Egreso <-0
        }
        
        if(is.data.frame(Egreso)){
            Egreso <- Egreso %>% group_by(Seccion) %>% summarise(Egreso = sum(captura), .groups = 'drop' )
            
        }
        
        
        Presupuesto <- list(
            Mantencion = Ingreso*0.3,
            Diversion = Ingreso*0.2,
            Aprender = Ingreso*0.15,
            Viajar = Ingreso*0.1,
            Invertir = Ingreso*0.25,
            Otros = 0,
            Egreso = Egreso
        )
        
        Presupuesto
        
    })
    
    
    ##----Texto para el presupuesto------
    
    observe({
        datPres <- datPresupuesto()
        lapply(1:6, presupuesto, datPres, output)
        })
    
    
    output$EfecAcumulado <- renderText({
        
        if(!is.null(mes$seleccion)){
            mes <- mes$seleccion
        } else {
            mes <- mes$mes
        }
        
        y <- year(mes)
        m <- month(mes)
        
        dat <- datBalance()
        dat <- dat$Balance
        
        if(!is.null(dat)){
            Efectivo <- dat %>% filter(year == y, month == m) 
        
            if(length(Efectivo$Efectivo) == 0) {
                Efectivo <- 0
            } else {
                Efectivo <- Efectivo %>% pull(Efectivo)
            }
        } else {
            Efectivo <- 0
        }
        
        prettyNum(round(Efectivo, digits = 2), big.mark = ",")

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
