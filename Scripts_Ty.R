install.packages("readxl")
install.packages("forcats")

#### Read Data ####
library(readxl)
StrainData <- read_excel("DeChiara_Domestication_life_traits.xlsx", sheet = "Supplementary Table 3", range ="A3:GM1114" )
colnames(StrainData)
TyData <- read_excel("Ty_numbers_1011collection.xlsx", sheet="Sheet1", range = "A1:Q1012")
head(TyData)


#### PLOT TY elements per clade####
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)

TyData_long <- TyData %>%
  pivot_longer(cols = 5:17, names_to = "Variable", values_to = "Valor")

# Crear y guardar un PDF con cada boxplot en una página diferente
pdf("figures_R/Boxplots_Tys_perClade.pdf", width = 10, height = 6)  # Abre el dispositivo PDF

# Iterar sobre cada variable y crear una página por boxplot
unique(TyData_long$Variable) %>%
  walk(function(var) {
    data_subset <- TyData_long %>% filter(Variable == var) %>%
      mutate(Clade = fct_reorder(Clade, Valor, .fun = median))  # Ordenar por mediana
    
    p <- ggplot(data_subset, aes(x = Clade, y = Valor, fill = Clade)) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Boxplot de", var), x = "Clade", y = "Valor")
    
    print(p)  # Imprimir gráfico en la página del PDF
  })

dev.off()  # Cerrar el archivo PDF


#### PLOT Datos de esporulación y supervivencia ####
colnames(StrainData)
TyData <- TyData %>%
  left_join(StrainData %>% select(`Standard name`, 11:23), 
            by = c("STAND" = "Standard name"))


TyData_long <- TyData %>%
  select(Clade, 18:29) %>%  # Mantener solo Clade y las columnas de interés
  pivot_longer(cols = 2:13, names_to = "Variable", values_to = "Valor") %>%
  drop_na(Valor) %>%  # Eliminar NAs
  mutate(Valor = as.numeric(Valor)) %>%  # Convertir valores a numéricos
  filter(!is.na(Valor))  # Asegurar que no haya conversiones fallidas

# Crear el PDF para guardar los boxplots
pdf("figures_R/Boxplots_SPO_and_CLS.pdf", width = 10, height = 6)

# Iterar sobre cada nueva variable numérica y generar una página por boxplot
TyData_long %>%
  group_by(Variable) %>%
  filter(all(is.numeric(Valor))) %>%  # Asegurar que la variable es numérica
  pull(Variable) %>%
  unique() %>%
  walk(function(var) {
    data_subset <- TyData_long %>%
      filter(Variable == var) %>%
      mutate(Clade = fct_reorder(Clade, Valor, .fun = median))  # Ordenar Clade por mediana
    
    # Crear el boxplot
    p <- ggplot(data_subset, aes(x = Clade, y = Valor, fill = Clade)) +
      geom_boxplot() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Boxplot de", var), x = "Clade", y = "Valor")
    
    print(p)  # Imprimir el gráfico en la página del PDF
  })

dev.off()  # Cerrar el archivo PDF


#### Scatter PLOTS RELACIONANDO CLS Y TYS####
library(gridExtra)

correlation_plot <- function(x_col, y_col, data) {
  # Seleccionar y limpiar datos (eliminar NAs y convertir a numérico)
  clean_data <- data %>%
    select(all_of(c(x_col, y_col))) %>%
    drop_na() %>%
    mutate(across(everything(), as.numeric))  # Convertir a numérico
  
  # Verificar que hay suficientes datos después de la limpieza
  if (nrow(clean_data) < 3) {
    return(ggplot() + ggtitle(paste("Insuficientes datos para", x_col, "vs", y_col)))
  }
  
  # Extraer valores limpios
  x_values <- clean_data[[x_col]]
  y_values <- clean_data[[y_col]]
  
  # Calcular correlación
  cor_test <- cor.test(x_values, y_values, use = "complete.obs", method = "pearson")
  
  # Extraer valores de interés
  corr_coef <- round(cor_test$estimate, 3)
  p_value <- signif(cor_test$p.value, 3)
  slope <- round(lm(y_values ~ x_values)$coefficients[2], 3)
  
  # Crear scatterplot con línea de tendencia
  ggplot(clean_data, aes(x = x_values, y = y_values)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = paste(x_col, "vs", y_col),
         subtitle = paste("r =", corr_coef, " | p =", p_value, " | m =", slope),
         x = x_col, y = y_col)
}

# Guardar los gráficos en un PDF
pdf("figures_R/Correlaciones_TYs_VS_CLSandSPO.pdf", width = 12, height = 9)

# Iterar sobre las columnas 5:17 (X variables)
for (x_var in names(TyData)[5:17]) {
  plots <- list()  # Lista para guardar los gráficos temporales
  
  # Iterar sobre las columnas 18:29 (Y variables)
  for (y_var in names(TyData)[18:29]) {
    plots[[length(plots) + 1]] <- correlation_plot(x_var, y_var, TyData)
  }
  
  # Dividir en páginas de 3x4 gráficos
  grid.arrange(grobs = plots, ncol = 4, nrow = 3, top = paste("Correlaciones para", x_var))
  
}
dev.off()  # Cerrar el archivo PDF


#### boxplots de para cada life history trait ordenados por las cantidades de Tys ####
for (x_var in names(TyData)[5:17]) {
  # Crear un nuevo archivo PDF para la variable actual
  pdf(paste0("figures_R/Boxplots_Ordenados_", x_var, ".pdf"), width = 10, height = 6)
  
  # Calcular el orden de los clados según la mediana de la variable x_var
  clade_order <- TyData %>%
    group_by(Clade) %>%
    summarise(mediana = median(!!sym(x_var), na.rm = TRUE)) %>%
    arrange(mediana) %>%
    pull(Clade)
  
  # Primera página: Boxplot de la variable x_var
  boxplot_x_var <- TyData %>%
    select(Clade, all_of(x_var)) %>%
    drop_na() %>%  # Eliminar NAs
    mutate(Clade = factor(Clade, levels = clade_order))  # Ordenar clados por mediana de x_var
  
  p_first <- ggplot(boxplot_x_var, aes(x = Clade, y = !!sym(x_var), fill = Clade)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Boxplot de", x_var, "\nOrdenado por mediana"),
         x = "Clade (ordenado por mediana)", 
         y = x_var)
  
  print(p_first)  # Primera página del PDF
  
  # Transformar TyData a formato largo con las columnas 18:29
  TyData_long <- TyData %>%
    select(Clade, all_of(x_var), 18:29) %>%
    pivot_longer(cols = 3:14, names_to = "Variable", values_to = "Valor") %>%
    drop_na(Valor) %>%  # Eliminar NAs
    mutate(Valor = as.numeric(Valor)) %>%  # Convertir a numérico
    filter(!is.na(Valor))  # Asegurar que no haya conversiones fallidas
  
  # Iterar sobre cada variable en 18:29 y generar boxplots en páginas separadas
  unique(TyData_long$Variable) %>%
    walk(function(var) {
      data_subset <- TyData_long %>%
        filter(Variable == var) %>%
        mutate(Clade = factor(Clade, levels = clade_order))  # Ordenar clados por mediana de x_var
      
      # Crear el boxplot
      p <- ggplot(data_subset, aes(x = Clade, y = Valor, fill = Clade)) +
        geom_boxplot() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Boxplot de", var, "\nOrdenado por mediana de", x_var),
             x = "Clade (ordenado por mediana de " ~ x_var ~ ")", 
             y = "Valor")
      
      print(p)  # Imprimir el gráfico en la página del PDF
    })
  
  dev.off()  # Cerrar el archivo PDF
}




#### Correlation scatters para cada clado para todos los 5:17 vs 18:29 ####ot
correlation_plot_clade <- function(x_col, y_col, data, clade) {
  # Filtrar los datos para el clado específico
  clean_data <- data %>%
    filter(Clade == clade) %>%
    select(all_of(c(x_col, y_col))) %>%
    drop_na() %>%  # Eliminar NAs
    mutate(across(everything(), as.numeric))  # Convertir a numérico
  
  # Verificar que hay suficientes datos después de la limpieza
  if (nrow(clean_data) < 3) {
    return(ggplot() + 
             ggtitle(paste("Insuficientes datos para", x_col, "vs", y_col, "en", clade)) +
             theme_void())
  }
  
  # Extraer valores limpios
  x_values <- clean_data[[x_col]]
  y_values <- clean_data[[y_col]]
  
  # Calcular correlación
  if (sum(!is.na(y_values)) > 3) {
    cor_test <- cor.test(x_values, y_values, use = "complete.obs", method = "pearson")

  
  
  # Extraer valores de interés
  corr_coef <- round(cor_test$estimate, 3)
  p_value <- signif(cor_test$p.value, 3)
  slope <- round(lm(y_values ~ x_values)$coefficients[2], 3)
  
  # Crear scatterplot con línea de tendencia
  ggplot(clean_data, aes(x = x_values, y = y_values)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    theme_minimal() +
    labs(title = paste(x_col, "vs", y_col, "\nClado:", clade),
         subtitle = paste("r =", corr_coef, " | p =", p_value, " | m =", slope),
         x = x_col, y = y_col)
  }
  
}
for (x_var in names(TyData)[6:17]) { # 5:17
  print(x_var)
  pdf(paste0("figures_R/CorrelationPlots_Clado_", x_var, ".pdf"), width = 10, height = 7)
  
  clades <- unique(TyData$Clade)
  
  for (y_var in names(TyData)[18:29]) {
    print(y_var)
    for (clade in clades) {
      print(clade)
      p <- correlation_plot_clade(x_var, y_var, TyData, clade)
      print(p)
    }
  }
  dev.off()  # Cerrar el archivo PDF
} 

