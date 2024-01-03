library(shiny)
library(DT)

chemin_hepatite_data <- "C:/Users/guoti/Documents/hepatitis/hepatite_data_correct.csv"

donnees_hepatite <- read.csv(chemin_hepatite_data, header = TRUE)


ui <- fluidPage(
  titlePanel('Shiny App'),
  tabsetPanel(
    tabPanel('Importation et visualisation de données initiales ',
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   fileInput('file', 'Choisissez votre fichier'),
                   actionButton('load_data', 'Charger les données')
                 ),
                 fluidRow(
                   actionButton('summary_button', "Sommaire")
                 ),
                 fluidRow(
                   actionButton('structure_button', "Structure")
                 )
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Tableau de données',
                            dataTableOutput('data_table'),
                            verbatimTextOutput('data_dimensions')
                   ),
                   tabPanel('Sommaire des données',
                            verbatimTextOutput('summary')
                   ),
                   tabPanel('Structure des données',
                            verbatimTextOutput('structure')
                   )
                 )
               )
             )
    ),
    tabPanel('Prétraitement de données',
             sidebarLayout(
               sidebarPanel(
                 selectInput("imputation_option", "Méthodes d'imputation:",
                             choices = c("Supprimer les lignes de NA", "Imputation par moyenne", "Imputation par médiane"))
                 ,
                 selectInput("variable_type", "Variable Type:", choices = c("Qualitative", "Quantitative"),selected = NULL)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel('Tableau avec imputation',
                            dataTableOutput('data_imputed')
                   ),
                   tabPanel('Type de variable',
                            verbatimTextOutput('selected_variables')
                   )
                   
               )
             )
           )
    ),
    tabPanel('Analyse exploratoire des données',
             mainPanel(
               # Contenu pour l'onglet 'Analyse exploratoire des données'
             )
    ),
    tabPanel('Choix et entrainement du modèle',
             sidebarLayout(
               sidebarPanel(
                 selectInput('model_selection', 'Choisir un modèle', choices = c('Modèle 1', 'Modèle 2', 'Modèle 3')),
                 actionButton('train_model_button', 'Entraîner le modèle')
               ),
               mainPanel(
                 # Contenu pour l'onglet 'Choix et entrainement du modèle'
               )
             )
    )
  )
)

server <- function(input, output){
#Partie importation et visualisation du data initial  
  #Importer et charger les données
  data_loaded <- reactiveVal(FALSE)
  file_data <- reactiveVal(NULL)
  imputed_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    inFile <- input$file
    if (!is.null(inFile)) {
      file_data(read.table(inFile$datapath, header = TRUE,sep=","))
      data_loaded(TRUE)
    }
  })
  
  output$data_table <- renderDataTable({
    if (data_loaded()) {
     file_data() }
  })
  
  #Afficher la dimension du data
  output$data_dimensions <- renderText({
    if (data_loaded()) {
      file_dim <- dim(file_data())
      dim_text <- paste("Dimensions du jeu de données :", paste(file_dim[1], "x", file_dim[2]))
      return(dim_text)
    }
  })
  
  #Summary
  output$summary <- renderPrint({
    req(input$summary_button)
    if (is.null(file_data())) return(NULL)
    # Résumé statistique des données
    summary_donnees <- summary(file_data())
    # Affichage du résumé et de la structure
    cat("Résumé des données :\n")
    print(summary_donnees)
  })
  
  #Structure
  output$structure <- renderPrint({
    req(input$structure_button)
    if (is.null(file_data())) return(NULL)
    # Structure des données
    type_donnees <- capture.output(str(file_data()))
    cat("\nStructure des données :\n")
    print(type_donnees)
  })

#Partie prétraitement

  # Fonction d'imputation par moyenne
  impute_mean <- function(data) {
    as.data.frame(apply(data, 2, function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE), 2), x)))
  }
  # Fonction d'imputation par médiane
  impute_median <- function(data) {
    as.data.frame(apply(data, 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))
  }
  output$data_imputed <- renderDataTable({
    if (data_loaded()) {
      imputed_data_val <- switch(input$imputation_option,
                             "Supprimer les lignes de NA" = na.omit(file_data()),
                             "Imputation par moyenne" = impute_mean(file_data()),
                             "Imputation par médiane" = impute_median(file_data()))
      imputed_data(imputed_data_val)
      return(imputed_data_val)
    }
  })
  
  output$selected_variables <- renderText({
    if (data_loaded()) {
      valeurs_uniques_par_colonne_importees <- apply(imputed_data(), 2, unique)
      seuil <- 5
      if (input$variable_type == "Qualitative") {
        selected_vars <- names(valeurs_uniques_par_colonne_importees[sapply(valeurs_uniques_par_colonne_importees, length) <= seuil])
        result <- "Variables qualitatives :\n"
        for (var in selected_vars) {
          donnees_qualitatives <- imputed_data()[, var]
          nombre_categories <- length(unique(donnees_qualitatives))
          result <- paste0(result, paste(var, " : ", nombre_categories, " catégories\n"))
        }
        return(result)
      } else if (input$variable_type == "Quantitative") {
        selected_vars <- names(valeurs_uniques_par_colonne_importees[sapply(valeurs_uniques_par_colonne_importees, length) > seuil])
    
        # Séparer les variables quantitatives en discrètes et continues
        var_quant_discretes <- character(0)
        var_quant_continues <- character(0)
        
        for (var in selected_vars) {
          donnees_quantitatives <- imputed_data()[, var]
          
          # Conditions pour distinguer entre variable quantitative discrète et continue
          if (is.numeric(donnees_quantitatives) && all.equal(donnees_quantitatives, round(donnees_quantitatives)) == TRUE) {
            var_quant_discretes <- c(var_quant_discretes, var)
          } else {
            var_quant_continues <- c(var_quant_continues, var)
          }
        }
        result <- "Variables quantitatives discrètes :\n"
        result <- paste0(result, paste(var_quant_discretes, collapse = "\n"))
        result <- paste0(result, "\n\nVariables quantitatives continues :\n")
        result <- paste0(result, paste(var_quant_continues, collapse = "\n"))
        return(result)
      }
    }
  })
  

  
  # Ajoutez le code pour l'analyse exploratoire des données et l'entraînement des modèles ici
  
}

shinyApp(ui = ui, server = server)

                