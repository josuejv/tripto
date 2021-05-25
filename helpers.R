## Por Josué Jiménez Vázquez
##----------Funcion para graficar el balance general---------

BalancePlot <- function(dat){
  
  p <- dat %>% ggplot(aes(x = Fecha, y = Value, col = Feature)) +
    geom_line(size = 1) +
    geom_point(size = 1) +
    ylab("MXP") + xlab("") +
    theme_light() +
    theme(axis.title = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(size = rel(1))) +
    scale_color_discrete(name = "")

  ggplotly(p) %>% layout(xaxis = list(rangeslider = list(),
                                      fixedrange = FALSE,
                                      autorange = TRUE
                                      ),
                         yaxis = list(fixedrange = FALSE,
                                      autorange = TRUE))
}


TypePlot <- function(dat){
  
  datLine <- dat %>% group_by(date) %>% summarise(Egreso = sum(captura), .groups = 'drop')
  
  p <- ggplot() + #dat %>% ggplot(aes(x = date, y = captura, fill = concepto)) +
    geom_line(data = datLine, aes(x = date, y = Egreso, color = "Egreso"), alpha = 0.3) +
    # geom_point(size = 1) +
    geom_col(data = dat, aes(x = date, y = captura, fill = Type)) +
  # 
  # 
  # p <- dat %>% ggplot(aes(x = date, y = captura, fill = Type)) +
  #   #geom_line(size = 1) +
  #   # geom_point(size = 1) +
  #   geom_col() +
    ylab("MXP") + xlab("") +
    theme_light() +
    theme(axis.title = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(size = rel(1))) +
    scale_color_discrete(name = "")
  
  ggplotly(p) %>% layout(xaxis = list(rangeslider = list(),
                                      fixedrange = FALSE,
                                      autorange = TRUE),
                         yaxis = list(fixedrange = FALSE,
                                      autorange = TRUE))
}

ConceptPlot <- function(dat, nombre){
  
  datLine <- dat %>% group_by(date) %>% summarise(!!sym(nombre) := sum(captura), .groups = 'drop')
  
  p <- ggplot() + #dat %>% ggplot(aes(x = date, y = captura, fill = concepto)) +
    geom_line(data = datLine, aes(x = date, y = !!sym(nombre), color = nombre), alpha = 0.3) +
    geom_smooth(data = datLine, aes(x = date, y = !!sym(nombre), color = paste(nombre, "smooth")), alpha = 0.1, span = 1) +
    # geom_point(size = 1) +
    geom_col(data = dat, aes(x = date, y = captura, fill = concepto)) +
    ylab("MXP") + xlab("") +
    theme_light() +
    theme(axis.title = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(size = rel(1))) +
    scale_color_discrete(name = "") +
    scale_fill_discrete(name = "")
    
  
  ggplotly(p) %>% layout(xaxis = list(rangeslider = list(),
                                      fixedrange = FALSE,
                                      autorange = TRUE),
                                      yaxis = list(fixedrange = FALSE,
                                      autorange = TRUE))
}

BarPlot <- function(dat){
  
  p <- dat %>%
    mutate(concepto = reorder(concepto, plyr::desc(cantidad), FUN = sum)) %>%
    ggplot(aes(x = concepto, y = cantidad)) + 
    geom_col(fill = "#4C82A8") +
    theme_light() +
    ylab("MXP") + xlab("") +
    theme(  axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1), angle = 90, vjust = 0.5), 
            axis.text.y = element_text(size = rel(1))) 
  
  ggplotly(p) 
   
}

BarPlot2 <- function(dat){
  
  p <- dat %>%
    mutate(categoria = reorder(categoria, plyr::desc(cantidad), FUN = sum)) %>%
    ggplot(aes(x = categoria, y = cantidad)) + 
    geom_col(fill = "#4C82A8") +
    theme_light() +
    ylab("MXP") + xlab("") +
    theme(  axis.title = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1), angle = 90, vjust = 0.5), 
            axis.text.y = element_text(size = rel(1))) 
  
  ggplotly(p) 
  
}



AreaPlot <- function(dat){
  
  p <- dat %>% ggplot(aes(x = date, y = captura, col = concepto)) +
    geom_line() +
    ylab("MXP") + xlab("") +
    theme_light() +
    theme(axis.title = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1), hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(size = rel(1))) +
    scale_color_discrete(name = "")
  
  ggplotly(p) %>% layout(xaxis = list(rangeslider = list(),
                                      fixedrange = FALSE,
                                      autorange = TRUE),
                         yaxis = list(fixedrange = FALSE,
                                      autorange = TRUE))
}

