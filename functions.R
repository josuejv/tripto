## Por Josué Jiménez Vázquez
##----Funcion que hace la UI reactiva para capturar las cantidades de cada apartado y donde van a aparecer los datos guardados----

capturaCreator1 <- function(Total_capturas, type, mes, datF, cptsaved) {

  if(is.null(datF)){
    
    type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
    
    id_captura <- paste0(type0, Total_capturas)
    n_captura <- paste(type, Total_capturas, "MXP")
    id_concepto <- paste0("Concepto", type0, Total_capturas)
    n_concepto <- paste("Concepto", Total_capturas)
    id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
    n_dateCaptura <- paste("Fecha", type, Total_capturas)
    id_categoria <- paste0("Categoria", type0, Total_capturas)
    n_categoria <- paste("Categoria", Total_capturas) 
    
    categoria <- ""
    concepto <- ""
    captura <- 0
    
  } else {
    
    if(length(cptsaved$nCapturasMes) == 0 | cptsaved$nCapturasMes < Total_capturas){
    
        type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
    
        id_captura <- paste0(type0, Total_capturas)
        n_captura <- paste(type, Total_capturas, "MXP")
        id_concepto <- paste0("Concepto", type0, Total_capturas)
        n_concepto <- paste("Concepto", Total_capturas)
        id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
        n_dateCaptura <- paste("Fecha", type, Total_capturas)
        id_categoria <- paste0("Categoria", type0, Total_capturas)
        n_categoria <- paste("Categoria", Total_capturas) 
        
        categoria <- ""
        concepto <- ""
        captura <- 0
        
    
    } else {
    
        datFRow <- datF[Total_capturas,]
    
        type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
    
        id_captura <- datFRow$id_captura
        n_captura <- paste(type, Total_capturas, "MXP")
        id_concepto <- datFRow$id_concepto
        n_concepto <- paste("Concepto", Total_capturas)
        id_dateCaptura <- datFRow$id_dateCaptura
        n_dateCaptura <- paste("Fecha", type, Total_capturas)
        id_categoria <- datFRow$id_categoria
        n_categoria <- paste("Categoria", Total_capturas) 
        
        categoria <- datFRow$categoria
        concepto <- datFRow$concepto
        captura <- datFRow$captura
 
        y <- datFRow$year
        m <- datFRow$month
        d <- datFRow$day
    
        mes <- as.Date(paste0(y, "-", m, "-", d))
    }
 
  }
  
  fluidRow(
    column(4,dateInput(id_dateCaptura, n_dateCaptura, value = mes, format = "yyyy-mm-dd", language = "es")),
    column(4,textInput(id_categoria, n_categoria, value = categoria)),
    column(4,textInput(id_concepto, n_concepto, value = concepto)),
    column(4,numericInput(id_captura, n_captura, value = captura, min = 0))
  )
}


capturaCreator2 <- function(Total_capturas, type, mes, datF, cptsaved) {
  
  if(is.null(datF)){
    
    type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
    
    id_captura <- paste0(type0, Total_capturas)
    n_captura <- paste(type, Total_capturas, "MXP")
    id_concepto <- paste0("Concepto", type0, Total_capturas)
    n_concepto <- paste("Concepto", Total_capturas)
    id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
    n_dateCaptura <- paste("Fecha", type, Total_capturas)
    id_rendimiento <- paste0("Rendimiento", type0, Total_capturas)
    n_rendimiento <- paste("Rendimiento %", type, Total_capturas)
    id_rend_periodo <- paste0("Periodo", type0, Total_capturas)
    n_rend_periodo <- paste("Periodo", type, Total_capturas, "Meses")
    id_categoria <- paste0("Categoria", type0, Total_capturas)
    n_categoria <- paste("Categoria", Total_capturas) 
    
    categoria <- ""
    concepto <- ""
    captura <- 0
    rendimiento <- 0
    rend_periodo <- 12
  } else {
    
    if(length(cptsaved$nCapturasMes) == 0 | cptsaved$nCapturasMes < Total_capturas){
      
      type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
      
      id_captura <- paste0(type0, Total_capturas)
      n_captura <- paste(type, Total_capturas, "MXP")
      id_concepto <- paste0("Concepto", type0, Total_capturas)
      n_concepto <- paste("Concepto", Total_capturas)
      id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
      n_dateCaptura <- paste("Fecha", type, Total_capturas)
      id_rendimiento <- paste0("Rendimiento", type0, Total_capturas)
      n_rendimiento <- paste("Rendimiento %", type, Total_capturas)
      id_rend_periodo <- paste0("Periodo", type0, Total_capturas)
      n_rend_periodo <- paste("Periodo", type, Total_capturas, "Meses")
      id_categoria <- paste0("Categoria", type0, Total_capturas)
      n_categoria <- paste("Categoria", Total_capturas) 
      
      categoria <- ""
      concepto <- ""
      captura <- 0
      rendimiento <- 0
      rend_periodo <- 12
      
    } else {
      
      datFRow <- datF[Total_capturas,]

        id_captura <- datFRow$id_captura
      n_captura <- paste(type, Total_capturas, "MXP")
      id_concepto <- datFRow$id_concepto
      n_concepto <- paste("Concepto", Total_capturas)
      id_dateCaptura <- datFRow$id_dateCaptura 
      n_dateCaptura <- paste("Fecha", type, Total_capturas)
      
      id_rendimiento <- datFRow$id_rendimiento
      n_rendimiento <- paste("Rendimiento %", type, Total_capturas)
      id_rend_periodo <- datFRow$id_rend_periodo
      n_rend_periodo <- paste("Periodo", type, Total_capturas, "Meses")
      id_categoria <- datFRow$id_categoria
      n_categoria <- paste("Categoria", Total_capturas) 
      
      categoria <- datFRow$categoria
      
      concepto <- datFRow$concepto  
      captura <- datFRow$captura  
      rendimiento <- datFRow$rendimiento  
      rend_periodo <- datFRow$rend_periodo  
      
      y <- datFRow$year
      m <- datFRow$month
      d <- datFRow$day
      
      mes <- as.Date(paste0(y, "-", m, "-", d))
  
    }

  }
  

  fluidRow(
    
    fluidRow(

      column(4,dateInput(id_dateCaptura, n_dateCaptura, value = mes, format = "yyyy-mm-dd", language = "es")),
      column(4,textInput(id_categoria, n_categoria, value = categoria)),
      column(4,textInput(id_concepto, n_concepto, value = concepto)),
      column(4,numericInput(id_captura, n_captura, value = captura, min = 0))
      
      
      
    ),
    fluidRow(
      column(6,numericInput(id_rendimiento, n_rendimiento, value = rendimiento, min = 0)),
      column(4,numericInput(id_rend_periodo, n_rend_periodo, value = rend_periodo, min = 0))

    )
    
    
  )
}
  
capturaCreator3 <- function(Total_capturas, type, mes, datF, cptsaved) {
  
  if(is.null(datF)){
    
    type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
    
    id_captura <- paste0(type0, Total_capturas)
    n_captura <- paste(type, Total_capturas, "MXP")
    id_concepto <- paste0("Concepto", type0, Total_capturas)
    n_concepto <- paste("Concepto", Total_capturas)
    id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
    n_dateCaptura <- paste("Fecha", type, Total_capturas)
    id_interes <- paste0("Interes", type0, Total_capturas)
    n_interes <- paste("Inter\u00E9s", "%", type, Total_capturas)
    id_deuda_periodo <- paste0("Periodo", type0, Total_capturas, "Meses")
    n_deuda_periodo <- paste("Periodo", type, Total_capturas, "Meses")
    id_categoria <- paste0("Categoria", type0, Total_capturas)
    n_categoria <- paste("Categoria", Total_capturas) 
    
    categoria <- ""
    concepto <- ""
    captura <- 0
    interes <- 0
    deuda_periodo <- 0
  } else {
    
    if(length(cptsaved$nCapturasMes) == 0 | cptsaved$nCapturasMes < Total_capturas){
      
      type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
      
      id_captura <- paste0(type0, Total_capturas)
      n_captura <- paste(type, Total_capturas, "MXP")
      id_concepto <- paste0("Concepto", type0, Total_capturas)
      n_concepto <- paste("Concepto", Total_capturas)
      id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
      n_dateCaptura <- paste("Fecha", type, Total_capturas)
      id_interes <- paste0("Interes", type0, Total_capturas)
      n_interes <- paste("Inter\u00E9s", "%", type, Total_capturas)
      id_deuda_periodo <- paste0("Periodo", type0, Total_capturas, "Meses")
      n_deuda_periodo <- paste("Periodo", type, Total_capturas, "Meses")
      id_categoria <- paste0("Categoria", type0, Total_capturas)
      n_categoria <- paste("Categoria", Total_capturas) 
      
      categoria <- ""
      concepto <- ""
      captura <- 0
      interes <- 0
      deuda_periodo <- 0
      
    } else {
      
      datFRow <- datF[Total_capturas,]
   
      type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
      
      id_captura <- datFRow$id_captura
      n_captura <- paste(type, Total_capturas, "MXP")
      id_concepto <- datFRow$id_concepto
      n_concepto <- paste("Concepto", Total_capturas)
      id_dateCaptura <- datFRow$id_dateCaptura 
      n_dateCaptura <- paste("Fecha", type, Total_capturas)
      
      id_interes <- datFRow$id_interes 
      n_interes <- paste("Inter\u00E9s", "%", type, Total_capturas)
      id_deuda_periodo <- datFRow$id_deuda_periodo
      n_deuda_periodo <- paste("Periodo", type, Total_capturas, "Meses")
      
      id_categoria <- datFRow$id_categoria
      n_categoria <- paste("Categoria", Total_capturas) 
      
      categoria <- datFRow$categoria
      
      concepto <- datFRow$concepto
      captura <- datFRow$captura
      interes <- datFRow$interes
      deuda_periodo <- datFRow$deuda_periodo

      y <- datFRow$year
      m <- datFRow$month
      d <- datFRow$day
      
      mes <- as.Date(paste0(y, "-", m, "-", d))
 
    }

  }
  
  
  fluidRow(
    
    fluidRow(
      column(4,dateInput(id_dateCaptura, n_dateCaptura, value = mes, format = "yyyy-mm-dd", language = "es")),
      column(4,textInput(id_categoria, n_categoria, value = categoria)),
      column(4,textInput(id_concepto, n_concepto, value = concepto)),
      column(4,numericInput(id_captura, n_captura, value = captura, min = 0))
      
    ),
    fluidRow(
      column(6,numericInput(id_interes, n_interes, value = interes, min = 0)),
      column(4,numericInput(id_deuda_periodo, n_deuda_periodo, value = deuda_periodo, min = 0))
      
    )
  
  )
}
  
capturaUI <- function(type, capturas, mes, datF, cptsaved){
 
  if (length(cptsaved$nCapturasMes) == 0){
    lapply(1:capturas, capturaCreator1, type, mes, 
           NULL, cptsaved)
  } else {
    lapply(1:capturas, capturaCreator1, type, mes, 
           datF, cptsaved)
  }
}

##----Funcion que hace la UI reactiva para capturar las cantidades de las inversiones----

capturaUI2 <- function(type, capturas, mes, datF, cptsaved){

  if (length(cptsaved$nCapturasMes) == 0){
    lapply(1:capturas, capturaCreator2, type, mes, NULL, cptsaved)
  } else {
    lapply(1:capturas, capturaCreator2, type, mes, datF, cptsaved)
  }
  
}

##-------Funcion que hace la UI reactiva para capturar las cantidades para las deudas-----

capturaUI3 <- function(type, capturas, mes, datF, cptsaved){

  if (length(cptsaved$nCapturasMes) == 0){
    
    lapply(1:capturas, capturaCreator3, type, mes, NULL, cptsaved)
    
  } else {

    lapply(1:capturas, capturaCreator3, type, mes, datF, cptsaved)
  }

}


## prueba para hacer una funcion que haga todas las Reactive UI

UIcreatorNo <- function(indice, output, mes, datF){
  
  nombre <- var_names$nombre[indice]
  seccion <- var_names$seccion[indice]
  numero <- paste0("N_", nombre)
  NoNombre <- paste0("No", nombre)
  type <- var_names$Tipo[indice]

  if (str_detect(nombre, "GF")) {
    objs <- "Gastos Fijos"
  } else if (str_detect(nombre, "GV")) {
    objs <- "Gastos Variables"
  } else if (str_detect(nombre, "IF")) {
    objs <- "Ingresos Fijos"
  } else if (str_detect(nombre, "IV")) {
    objs <- "Ingresos Variables"
  } else if (str_detect(nombre, "GD")) {
    objs <- "Gastos Deudas"
  } else {
    objs <- seccion
  }

  output[[numero]] <- renderUI({

    y <- year(mes)
    m <- month(mes)

    datF <- datF[[indice]]

    if(is.null(datF$nCapturasMes[1])){
      
      n_capturas <- 0
      
    } else {
  
      if(length(datF$nCapturasMes) > 0){
          n_capturas <- datF$nCapturasMes
      } else{
          n_capturas <- 0
      }
      
    }
    
    numericInput(NoNombre, paste("Numero de", objs),
                 value = n_capturas, min = 0, step = 1)
  })
  
}



UIcreatorCaptures <- function(indice, output, input, mes, datF, cptsaved){

  nombre <- var_names$nombre[indice]
  seccion <- var_names$seccion[indice]
  NoNombre <- paste0("No", nombre)
  type <- var_names$Tipo[indice]
 
  output[[nombre]] <- renderUI({
    
    datF <- datF[[indice]]
    cptsaved <- cptsaved[[indice]]

    if(nombre == "Act_EF_Inicial"){
      n_captura <- 1
    } else {
      n_captura <- input[[NoNombre]]
    }
    
    if(!is.null(n_captura)){
      if(is.numeric(n_captura)){
        if(n_captura > 0){
          
          if(nombre %in% c("Inversion", "Bienes_raices", "Propiedad_P")){
            capturaUI2(type, n_captura, mes, datF, cptsaved)
          } else if (nombre == "Deudas") {
            capturaUI3(type, n_captura, mes, datF, cptsaved)
          } else {
            capturaUI(type, n_captura, mes, datF, cptsaved)
          }
     
        }
      }
    }
  })
  
}


UIcreatorBtns <- function(indice, output, input){
  
  nombre <- var_names$nombre[indice]
  btn <- paste0("Btn_", nombre)
  NoNombre <- paste0("No", nombre)
  
  output[[btn]] <- renderUI({
    if(nombre == "Act_EF_Inicial"){
      n_captura <- 1
    } else {
      n_captura <- input[[NoNombre]]
    }
    if(nombre != "GD_Otros") {
      if(!is.null(n_captura)){
        if(is.numeric(n_captura)){
          if(n_captura > 0){
            actionButton(paste0("Agregar", nombre), "Agregar")
        }
      }
    }
      
    }

  })
  
}

## -----Para crear las tablas de los datos capturados-----


dfCreator <- function(Total_capturas, seccion, type, input, nCapturasMes, y, m, dia, nombre) {
  

  type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
  
  
  id_captura <- paste0(type0, Total_capturas)
  n_captura <- paste(type, Total_capturas, "MXP")
  id_concepto <- paste0("Concepto", type0, Total_capturas)
  n_concepto <- paste("Concepto", Total_capturas)
  id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
  n_dateCaptura <- paste("Fecha", type, Total_capturas)
  id_categoria <- paste0("Categoria", type0, Total_capturas)
  n_categoria <- paste("Categoria", Total_capturas) 
  
  categoria <- input[[id_categoria]]
  captura <- input[[id_captura]]
  concepto <- input[[id_concepto]]
  d0 <- input[[id_dateCaptura]]
  d <- input[[id_dateCaptura]]

  if(!is.null(d0)){
    d <- day(d0)
    yn <- year(d0)
    mn <- month(d0)
    date <- as.Date(paste0(yn, "-", mn, "-", d))
    
  } 

  if(nombre %in% c("Inversion", "Bienes_raices", "Propiedad_P")){
    id_interes <- NA
    interes <- NA
    id_deuda_periodo <- NA
    deuda_periodo <- NA
    id_rendimiento <- paste0("Rendimiento", type0, Total_capturas)
    rendimiento <- input[[id_rendimiento]]
    id_rend_periodo <- paste0("Periodo", type0, Total_capturas)
    rend_periodo <- input[[id_rend_periodo]]
    
  } else if (nombre == "Deudas") {
    id_interes <- paste0("Interes", type0, Total_capturas)
    interes <- input[[id_interes]]
    id_deuda_periodo <- paste0("Periodo", type0, Total_capturas, "Meses")
    deuda_periodo <- input[[id_deuda_periodo]]
    id_rendimiento <- NA
    rendimiento <- NA
    id_rend_periodo <- NA
    rend_periodo <- NA
    
  } else {
    id_interes <- NA
    interes <- NA
    id_deuda_periodo <- NA
    deuda_periodo <- NA
    id_rendimiento <- NA
    rendimiento <- NA
    id_rend_periodo <- NA
    rend_periodo <- NA
  }
  
  if(!is.null(captura) & !is.null(concepto) & !is.null(d)){
    finanzas <- data.frame(
      Seccion = seccion,
      Type = type,
      nCaptura = Total_capturas,
      nCapturasMes = nCapturasMes,
      id_dateCaptura = id_dateCaptura,
      # year = y,
      # month = m,
      year = yn,
      month = mn,
      day = d,
      date = date,
      #IMF = IMF,
      id_categoria = id_categoria,
      categoria = categoria,
      id_concepto = id_concepto,
      concepto = concepto,
      id_captura = id_captura,
      captura = captura,
      id_interes = id_interes,
      interes = interes,
      id_deuda_periodo = id_deuda_periodo,
      deuda_periodo = deuda_periodo,
      id_rendimiento = id_rendimiento,
      rendimiento = rendimiento,
      id_rend_periodo = id_rend_periodo,
      rend_periodo = rend_periodo
    )
     finanzas
    
  }

}

## -----Para crear la tabla del numero de capturas por mes y año capturados-----

dfnCaptureCreator <- function(seccion, type, input, nCapturasMes, y, m) {
    NCapturas <- data.frame(
      Seccion = seccion,
      Type = type,
      nCapturasMes = nCapturasMes,
      year = y,
      month = m
    )
  
}


#--------Funcion que crea las tablas vacias------

createDF0 <- function(){
  
  if(!file.exists("rdas/Finanzas.rda")){
    
    Finanzas <- data.frame(
      Seccion = NA,
      Type = NA,
      nCaptura = NA,
      nCapturasMes = NA,
      id_dateCaptura = NA,
      year = NA,
      month = NA,
      day = NA,
      date = NA,
      #IMF = NA,
      id_categoria = NA,
      categoria = NA,
      id_concepto = NA,
      concepto = NA,
      id_captura = NA,
      captura = NA,
      id_interes = NA,
      interes = NA,
      id_deuda_periodo = NA,
      deuda_periodo = NA,
      id_rendimiento = NA,
      rendimiento = NA,
      id_rend_periodo = NA,
      rend_periodo = NA
      
    )
    
    save(Finanzas, file = "rdas/Finanzas.rda")

  }
  
  if(!file.exists("rdas/nCapturas.rda")){
    
    nCapturas <- data.frame(
      Seccion = NA,
      Type = NA,
      nCapturasMes = NA,
      year = NA,
      month = NA
      
    )
    
    save(nCapturas, file = "rdas/nCapturas.rda")

  }
  
  
  
}



#--------Funcion que guarda los datos de una tabla en el sistema------

saveData <- function(dat1, dat2){
  
  Finanzas <- dat1
  nCapturas <- dat2 
  
  save(Finanzas, file = "rdas/Finanzas.rda")
  save(nCapturas, file = "rdas/nCapturas.rda")
  
  
  write.xlsx(Finanzas, 
             file = "data/Finanzas.xlsx", 
             sheetName = "Finanzas", append = FALSE)
  
  write.xlsx(nCapturas, 
             file = "data/nCapturas.xlsx", 
             sheetName = "nCapturas", append = FALSE)
  

}


#--------Funcion que determina si ya hay datos guardados en el mes y el año-----

dataFilter <- function(index, mes, dat){
  
  y <- year(mes)
  m <- month(mes)
  
  seccion <- var_names$seccion[index]
  type <- var_names$Tipo[index]
 
  if(type == "Activo Efectivo Inicial"){
    datfiltered <- dat %>% filter(Seccion == seccion & Type == type)
  } else {
    datfiltered <- dat %>% filter(Seccion == seccion & Type == type & year == y & month == m)
  }
  
  datfiltered <- datfiltered %>% filter(!is.na(Seccion))
  if(length(datfiltered) == 0){
    datfiltered <- NULL
  }
  
  datfiltered
  
}

#--------Funcion que determina los datos no filtrados-----

dataNotFilter <- function(dat, datF, type) {
 
  if(length(datF$nCapturasMes) > 0) {
    
    if(type == "Activo Efectivo Inicial"){
      dataNF <- anti_join(dat, datF, by = c("Seccion", "Type"))
    } else {
      dataNF <- anti_join(dat, datF, by = c("Seccion", "Type", "year", "month"))
    }
  } else {
    dataNF <- NULL
  }
  dataNF
  
}




##-------Funcion que hace el codigo para los botones------

BtnsEvents <- function(indice, input, output, finanzas_totales, mes, datF, datnCF, Finanzas0, nCapturas0){

  nombre <- var_names$nombre[indice]
  nombre <- paste0("Agregar", nombre)
  type <- var_names$Tipo[indice]

  observeEvent(input[[nombre]],{

    datFinanzas <- lapply(1:length(var_names$nombre), datFinanzasFunction,  mes, datF, input, finanzas_totales, Finanzas0)
    datnCapturas <- lapply(1:length(var_names$nombre), datnCapturasFunction,  mes, datnCF, input, finanzas_totales, nCapturas0)
    
    finanzas_totales$datosCapturados <- datFinanzas
    finanzas_totales$datosCapturadosN <- datnCapturas
    
    finanzas <- datFinanzas[[indice]]

    capturas <- datnCapturas[[indice]]
    
    if (str_detect(type, "Fijo")){
      
      finanzas <- lapply(0:11, datFijos,  finanzas)
      finanzas <- Reduce(rbind, finanzas)
      
      capturas <- lapply(0:11, datFijos,  capturas)
      capturas <- Reduce(rbind, capturas)
      
    } else if (type == "Deudas") {
      
      deudas <- datDeudas(finanzas, capturas)
      finanzas <- deudas$Deudas
      capturas <- deudas$capturas
      
    } else if(type %in% c("Inversion", "Bienes Raices", "Propiedad Personal")){
      
      activos <- datActivos(finanzas, capturas)
      finanzas <- activos$Activos
      capturas <- activos$capturas
      
    }
  
    datF <- datF[[indice]]
    datnCF <- datnCF[[indice]]

    dat1 <- dataBinder(finanzas, Finanzas0, finanzas, #datNF, 
                       finanzas_totales$datosAgregados, type)

    dat2 <- dataBinder(capturas, nCapturas0, capturas, #datnCNF, 
                       finanzas_totales$nCapturas, type)
    
    saveData(dat1, dat2)

    finanzas_totales$datosAgregados <- dat1
    finanzas_totales$nCapturas <- dat2

    lapply(1:length(var_names$nombre), UIcreatorCaptures, output, input, mes, finanzas_totales$datosCapturados, finanzas_totales$datosCapturadosN)
    lapply(1:length(var_names$nombre), UIcreatorNo, output, mes, finanzas_totales$datosCapturadosN)
  
    
  })
  
}


##-------Funcion que crea la tabla de todos los datos-----------

allData <- function(index, input, y, m, dia, datF, datNF, finanzas_totales, Finanzas){
  
  datF <- datF[[index]]
  type <- var_names$Tipo[index]
  nombre <- var_names$nombre[index]
  seccion <- var_names$seccion[index]
  NoNombre <- paste0("No", nombre)
  if(nombre == "Act_EF_Inicial"){
    nCapturasMes <- 1
  } else {
    nCapturasMes <- input[[NoNombre]]
  }

  if(!is.null(nCapturasMes)){
    if(is.numeric(nCapturasMes)){
      if(nCapturasMes > 0){
        
        finanzas <- lapply(1:nCapturasMes, dfCreator,seccion, type, input, nCapturasMes, y, m, dia, nombre)
        finanzas <- Reduce(rbind, finanzas)

        finanzas
        
      }
    }
  }
  
}
 

#----Funcion que crea la tabla de todas las capturas------


allNCapturas <- function(index, input, y, m, datF, #datNF, 
                         finanzas_totales, nCapturas){
  
  datF <- datF[[index]]

  type <- var_names$Tipo[index]
  
  nombre <- var_names$nombre[index]
  seccion <- var_names$seccion[index]
  NoNombre <- paste0("No", nombre)

  if(nombre == "Act_EF_Inicial"){
    nCapturasMes <- 1
  } else {
    nCapturasMes <- input[[NoNombre]]
  }
  
  if(!is.null(nCapturasMes)){
    if(is.numeric(nCapturasMes)){
      if(nCapturasMes > 0){
        
        numero_capturas <- dfnCaptureCreator(seccion, type, input, nCapturasMes, y, m)
        numero_capturas
      }
    }
  }
  
  
}


#------Funcion que une los datos finales----------

dataBinder <- function(dat, dat0, datF, #datNF, 
                       datosAgregados, type){
  
  if (is.null(datosAgregados)){
    
    if(length(datF$nCapturasMes) > 0){
      datNF <- dataNotFilter(dat0, datF, type)
      dat0 <- datNF
    } 
    
  } else {
    if(length(datF$nCapturasMes) == 0){
      dat0 <- datosAgregados
    } else {
      datNF <- dataNotFilter(datosAgregados, datF, type)
      dat0 <- datNF
    }
    
  }
  
  if(!is.null(dat0)){
    dat <- rbind(dat, dat0)
    dat <- dat %>% filter(!is.na(Seccion))
  }
  
  dat
  
}


datFinanzasFunction <- function(indice, mes, datF, input, finanzas_totales, Finanzas){
  y <- year(mes)
  m <- month(mes)
  dia <- day(mes)
  finanzas <- allData (indice, input, y, m, dia, datF, finanzas_totales, Finanzas)
  finanzas
}




datnCapturasFunction <- function(indice, mes, datF, input, finanzas_totales, nCapturas){
  
  mesC <-input$date
  y <- year(mesC)
  m <- month(mesC)

  numero_capturas <- allNCapturas (indice, input, y, m, datF, finanzas_totales, nCapturas)
  numero_capturas
}


##-------Funcion que agrega automaticamente los datos fijos---------

datFijos <- function(mes, dat){
  
  m <- dat$month[1] + mes
  y <- trunc((m-1)/12)
  
  if(m <= 12){
    
    if(is.null(dat$date)){
      dat <- dat %>% mutate(month = m)
    } else {
      dat <- dat %>% mutate(month = m, date = as.Date(paste0(year, "-", month, "-", day)))
    }
    
  } else {
    m <- m-(12*y)
    
    if(is.null(dat$date)){
      dat <- dat %>% mutate(year = year+y , month = m)
    } else {
      dat <- dat %>% mutate(year = year+y , month = m, date = as.Date(paste0(year, "-", month, "-", day)))
    }
  }
  dat
}


##-------Funcion que agrega los datos de las deudas----------


datDeudas <- function(dat, datCapturas){
  
  nDeudas <- length(dat$nCapturasMes)
  
  if(nDeudas > 1){
    
    periodoMax <- max(dat$deuda_periodo)

    Deudas <- lapply(1:nDeudas, deudasFunction,  dat, datCapturas, periodoMax, nDeudas)
    Deudas <- Reduce(rbind, Deudas)
    
    seccion <- "Otros"
    type <- "Gasto Deudas Otros"
    nCapturasMes <- nDeudas
    y <- datCapturas$year
    m <- datCapturas$month + 1
    
    if(m > 12){
      y <- y+1
      m <- m-12
    } 
    
    CapturasGVO <- dfnCaptureCreator(seccion, type, input, nCapturasMes, y, m)
    capturas <- lapply(0:(periodoMax-1), datFijos,  datCapturas)
    capturas <- Reduce(rbind, capturas)
    
    CapturasGVO <- lapply(0:(periodoMax-1), datFijos,  CapturasGVO)
    CapturasGVO <- Reduce(rbind, CapturasGVO)
    capturas <- rbind(capturas, CapturasGVO)
    
    
    Deudas <- list(
      Deudas = Deudas,
      capturas = capturas
    )
    
  } else {
    
    periodoMax <- max(dat$deuda_periodo)
    Deudas <- deudasFunction(1, dat, datCapturas, periodoMax, nDeudas)
    capturas <- deudasCFunction(1, dat, datCapturas)
    
    seccion <- "Otros"
    type <- "Gasto Deudas Otros"
    nCapturasMes <- nDeudas
    y <- datCapturas$year
    m <- datCapturas$month + 1
    
    if(m > 12){
      y <- y+1
      m <- m-12
    } 
    
    CapturasGVO <- dfnCaptureCreator(seccion, type, input, nCapturasMes, y, m)
    CapturasGVO <- lapply(0:(periodoMax-1), datFijos,  CapturasGVO)
    CapturasGVO <- Reduce(rbind, CapturasGVO)
    capturas <- rbind(capturas, CapturasGVO)
    
    Deudas <- list(
      Deudas = Deudas,
      capturas = capturas
    )
  }  

  Deudas
    
}


deudasFunction <- function(ndeudas, dat, datCapturas, maxPeriodo, nDeudas){
    
  dat <- dat[ndeudas,]
  PagoMes <- dat$captura/dat$deuda_periodo
  nGDO <- ndeudas
  Deudas <- lapply(0:(maxPeriodo-1), deudasRestantes,  PagoMes, dat, nGDO, nDeudas)
  Deudas <- Reduce(rbind, Deudas)
  Deudas
  
}

deudasCFunction <- function(ndeudas, dat, datCapturas){
  
  dat <- dat[ndeudas,]
  capturas <- lapply(0:(dat$deuda_periodo-1), datFijos,  datCapturas)
  capturas <- Reduce(rbind, capturas)
  capturas
}




deudasRestantes <- function(nperiodo, PagoMes, dat, nGDO, nDeudas){

    restante <- dat$captura - PagoMes*nperiodo
    
    if(restante < 0 | is.na(restante) | is.null(restante)){
      restante <- 0
    }
    
    PagoTotal <- (restante*dat$interes/100/12)+PagoMes
    
    m <- dat$month + nperiodo
    y <- trunc((m-1)/12)
    
    Total_capturas <- nGDO
      seccion <- "Otros"
      type <- "Gasto Deudas Otros"
      nCapturasMes <- nDeudas
      nombreCustom <- "GD_Otros"
      capturaCustom <- ifelse(restante == 0, 0, PagoTotal)
      conceptoCustom <- paste("Pago", dat$concepto)
    
    if(m <= 12){
      
      dat <- dat %>% mutate(month = m, captura = restante, deuda_periodo = ifelse(deuda_periodo-nperiodo < 0, 0, deuda_periodo-nperiodo), date = as.Date(paste0(year, "-", month, "-", day)))
      mn <- m+1
      
      if (mn <= 12) {
        fechaCustom <- as.Date(paste0(dat$year, "-", mn, "-", dat$day))
      } else {
        fechaCustom <- as.Date(paste0(dat$year+1, "-", mn-12, "-", dat$day))
      }
      
    } else {

      m <- m-(12*y)
      dat <- dat %>% mutate(year = year+y , month = m, captura = restante, deuda_periodo = ifelse(deuda_periodo-nperiodo < 0, 0, deuda_periodo-nperiodo), date = as.Date(paste0(year, "-", month, "-", day)))
      mn <- m+1
      
      if (mn <= 12) {
        fechaCustom <- as.Date(paste0(dat$year, "-", mn, "-", dat$day))
      } else {
        fechaCustom <- as.Date(paste0(dat$year+1, "-", mn-12, "-", dat$day))
      }
      
    }

      datPago <- dfCreatorCustom (Total_capturas, seccion, type, input, nCapturasMes, nombreCustom, capturaCustom, conceptoCustom, fechaCustom)
      dat <- rbind(dat, datPago)
  
  dat
  
}

##---------Funcion para crear los datos a parte de los que se capturan----------


dfCreatorCustom <- function(Total_capturas, seccion, type, input, nCapturasMes, nombre, capturaCustom, conceptoCustom, fechaCustom) {
  
  type0 <- ifelse(str_detect(type, " "), str_replace_all(type," ", "_"), type)
  
  
  id_captura <- paste0(type0, Total_capturas)
  n_captura <- paste(type, Total_capturas, "MXP")
  id_concepto <- paste0("Concepto", type0, Total_capturas)
  n_concepto <- paste("Concepto", Total_capturas)
  id_dateCaptura <- paste0("Fecha_", type0, Total_capturas)
  n_dateCaptura <- paste("Fecha", type, Total_capturas)
  
  captura <- capturaCustom
  concepto <- conceptoCustom
  d0 <- fechaCustom
  d <- fechaCustom
  
  if(!is.null(d0)){
    d <- day(d0)
    yn <- year(d0)
    mn <- month(d0)
    date <- as.Date(paste0(yn, "-", mn, "-", d))
    
  } 

  if(nombre %in% c("Inversion", "Bienes_raices", "Propiedad_P")){
    id_interes <- NA
    interes <- NA
    id_deuda_periodo <- NA
    deuda_periodo <- NA
    id_rendimiento <- paste0("Rendimiento", type0, Total_capturas)
    rendimiento <- input[[id_rendimiento]]
    id_rend_periodo <- paste0("Periodo", type0, Total_capturas)
    rend_periodo <- input[[id_rend_periodo]]
    
  } else if (nombre == "Deudas") {
    id_interes <- paste0("Interes", type0, Total_capturas)
    interes <- input[[id_interes]]
    id_deuda_periodo <- paste0("Periodo", type0, Total_capturas, "Meses")
    deuda_periodo <- input[[id_deuda_periodo]]
    id_rendimiento <- NA
    rendimiento <- NA
    id_rend_periodo <- NA
    rend_periodo <- NA
    
  } else {
    id_interes <- NA
    interes <- NA
    id_deuda_periodo <- NA
    deuda_periodo <- NA
    id_rendimiento <- NA
    rendimiento <- NA
    id_rend_periodo <- NA
    rend_periodo <- NA
  }
  
  
  
  if(!is.null(captura) & !is.null(concepto) & !is.null(d)){
    finanzas <- data.frame(
      Seccion = seccion,
      Type = type,
      nCaptura = Total_capturas,
      nCapturasMes = nCapturasMes,
      id_dateCaptura = id_dateCaptura,
      # year = y,
      # month = m,
      year = yn,
      month = mn,
      day = d,
      date = date,
      #IMF = IMF,
      id_concepto = id_concepto,
      concepto = concepto,
      id_captura = id_captura,
      captura = captura,
      id_interes = id_interes,
      interes = interes,
      id_deuda_periodo = id_deuda_periodo,
      deuda_periodo = deuda_periodo,
      id_rendimiento = id_rendimiento,
      rendimiento = rendimiento,
      id_rend_periodo = id_rend_periodo,
      rend_periodo = rend_periodo
    )

    finanzas
    
  }
  
}


##---------Funcion para los activos----------


datActivos <- function(dat, datCapturas){
  
  nActivos <- length(dat$nCapturasMes)
  periodoInversion <- max(dat$rend_periodo)
  
  
  if(nActivos > 1){

    Activos <- lapply(1:nActivos, activosFunction,  dat, datCapturas, periodoInversion, nActivos)
    Activos <- Reduce(rbind, Activos)

    capturas <- lapply(0:(periodoInversion-1), datFijos,  datCapturas)
    capturas <- Reduce(rbind, capturas)

    Activos <- list(
      Activos = Activos,
      capturas = capturas
    )
    
  } else {
    
    Activos <- activosFunction(1, dat, datCapturas, periodoInversion, nActivos)
    capturas <- lapply(0:(periodoInversion-1), datFijos,  datCapturas)
    capturas <- Reduce(rbind, capturas)

    Activos <- list(
      Activos = Activos,
      capturas = capturas
    )
  }  
  
  Activos
  
}


activosFunction <- function(nactivos, dat, datCapturas, periodoInversion, nActivos){
  
  dat <- dat[nactivos,]
  
  Activos <- lapply(0:(periodoInversion-1), activosIncremento, dat, nActivos)
  Activos <- Reduce(rbind, Activos)
  Activos

}

activosIncremento <- function(nperiodo, dat, nActivos){

  
  Incremento <- dat$captura*(1 + (dat$rendimiento/100))^(nperiodo/12)
  
  if (Incremento < 0 | is.na(Incremento)){
    Incremento <- 0
  }

  m <- dat$month + nperiodo
  y <- trunc((m-1)/12) 
  
  if(m <= 12){
    
    dat <- dat %>% mutate(month = m, captura = Incremento, rend_periodo = ifelse(rend_periodo-nperiodo < 0, 0, rend_periodo-nperiodo), date = as.Date(paste0(year, "-", month, "-", day)))

  } else {
    m <- m-(12*y)
    dat <- dat %>% mutate(year = year+y , month = m, captura = Incremento, rend_periodo = ifelse(rend_periodo-nperiodo < 0, 0, rend_periodo-nperiodo), date = as.Date(paste0(year, "-", month, "-", day)))
  }

  dat

}


## Funcion para ver los presupuestos

presupuesto <- function(index, datPres, output){
  
  nombre0 <- names(datPres)[index]
  nombre <- paste0("Pres", nombre0)
    
    output[[nombre]] <- renderText({
      
      dat <- datPres
      budget <- round(dat[[index]], 2)
      egreso <- dat$Egreso
      
      if(is.data.frame(egreso)){
        
        if(nombre0 == "Mantencion"){
          nombre01 <- "Mantención"
        } else if (nombre0 == "Diversion"){
          nombre01 <- "Diversión"
        } else {
          nombre01 <- nombre0
        }
        
        egreso <- egreso %>% filter(Seccion == nombre01)
        
        if(length(egreso$Egreso) == 0){
          egreso <- 0
        } else {
          egreso <- egreso$Egreso
        }
        
      }
      
      rest <- round(budget-egreso, 2)
      
      budget <- strong(budget, style = "color:green")
      
      if(rest <= 0){
        rest <- strong(rest, style = "color:red")
      } else {
        rest <- strong(rest, style = "color:green")
      }
      
      if(nombre0 == "Mantencion"){
        nombre0 <- "MANTENCI&OacuteN 30%"
      } else if (nombre0 == "Diversion"){
        nombre0 <- "DIVERSI&OacuteN 20%"
      } else if (nombre0 == "Aprender"){
        nombre0 <- "APRENDER 15%"
      } else if (nombre0 == "Viajar"){
        nombre0 <- "VIAJAR 10%"
      } else if (nombre0 == "Invertir"){
        nombre0 <- "INVERTIR 25%"
      } else if (nombre0 == "Otros"){
        nombre0 <- "OTROS"
      } 
      
      paste(nombre0, "Presupuesto:", budget, "   Restante:", rest)
    })
    
  
}






