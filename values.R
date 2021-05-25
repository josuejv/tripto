## Por Josué Jiménez Vázquez
var_names <- data.frame(
  nombre = c("IF_Ingresos", "IV_Ingresos",
             "GF_Mantencion", "GV_Mantencion",
             "GF_Diversion", "GV_Diversion",
             "GF_Aprender", "GV_Aprender",
             "GF_Viajar", "GV_Viajar",
             "GF_Invertir", "GV_Invertir",
             "GF_Otros", "GV_Otros", "GD_Otros",
             "Act_EF_Inicial",
             "Inversion",
             "Bienes_raices",
             "Propiedad_P",
             "Deudas"
             ),
  seccion = c("Ingresos", "Ingresos",
              "Mantención", "Mantención",
              "Diversión", "Diversión",
              "Aprender", "Aprender",
              "Viajar", "Viajar",
              "Invertir", "Invertir",
              "Otros", "Otros", "Otros",
              "Efectivo Inicial",
              "Inversión",
              "Bienes Raíces",
              "Propiedad Personal",
              "Deudas"
              ),
  Tipo = c("Ingreso Fijo", "Ingreso Variable",
           "Gasto Fijo Mantencion", "Gasto Variable Mantencion",
           "Gasto Fijo Diversion", "Gasto Variable Diversion",
           "Gasto Fijo Aprender", "Gasto Variable Aprender",
           "Gasto Fijo Viajar", "Gasto Variable Viajar",
           "Gasto Fijo Invertir", "Gasto Variable Invertir",
           "Gasto Fijo Otros", "Gasto Variable Otros", "Gasto Deudas Otros",
           "Activo Efectivo Inicial",
           "Inversion",
           "Bienes Raices",
           "Propiedad Personal",
           "Deudas"
  )
  
  
)


## para modificar las tablas de la version anterior

# nombres <- names(Finanzas)
# nombres
# 
# Finanzas <- Finanzas %>% mutate(id_categoria = str_replace(id_concepto, "Concepto", "Categoria"), categoria = NA)
# Finanzas <- Finanzas %>% select(nombres)
# 
# 
# save(Finanzas, file = "rdas/Finanzas.rda")















