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
    tabPanel('Analyse exploratoire des données',
             sidebarLayout(
               sidebarPanel(
                 selectInput("imputation_option", "Méthodes d'imputation:",
                             choices = c("Supprimer les lignes de NA", "Imputation par moyenne", "Imputation par médiane")),
                 selectInput("variable_type", "Variable Type:", choices = c("Qualitative", "Quantitative"),selected = NULL),
                 selectizeInput("variable_single", "Choisir une variable",
                             choices = NULL, selected = NULL),
                 selectizeInput("variable_dual", "Choisir deux variables",
                                choices = NULL, selected = NULL, multiple = TRUE, options = list(maxItems = 2))
              ),
               mainPanel(
                 tabsetPanel(
                   
                   tabPanel('Tableau avec imputation',
                            dataTableOutput('data_imputed')),
                   tabPanel('Type de variable',
                            verbatimTextOutput('selected_variables')),
                   tabPanel('Analyse unidimensionnelle',
                            uiOutput('univariate_analysis_ui')),
                   tabPanel('Analyse bidimensionnelle')
                 )
               )
             )
    ),
    tabPanel('Prétraitement de données'
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

server <- function(input, output,session){
#Partie importation et visualisation du data initial  
  #Importer et charger les données
  data_loaded <- reactiveVal(FALSE)
  file_data <- reactiveVal(NULL)
  imputed_data <- reactiveVal(NULL)
  selected_variable_single_type <- reactiveVal(NULL)
  
  
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

#Partie analyse exploratoire
  # Fonction d'imputation par moyenne
  impute_mean <- function(data) {
    as.data.frame(apply(data, 2, function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE), 2), x)))
  }
  # Fonction d'imputation par médiane
  impute_median <- function(data) {
    as.data.frame(apply(data, 2, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))
  }
  #afficher tableau imputé
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
  
  #afficher la liste des variables par type quantitative ou qualitative
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
  
  #mettre à jour la session pour obtenir la liste des variables après imputation
  observe({
    if (!is.null(input$imputation_option) && data_loaded()) {
      imputed_data_val <- switch(input$imputation_option,
                                 "Supprimer les lignes de NA" = na.omit(file_data()),
                                 "Imputation par moyenne" = impute_mean(file_data()),
                                 "Imputation par médiane" = impute_median(file_data()))
      imputed_data(imputed_data_val)
      
      variables <- colnames(imputed_data())
      updateSelectInput(session, "variable_single", choices = variables)
      updateSelectInput(session, "variable_dual", choices = variables)
    }
  })
  
  # Fonction pour identifier le type de la variable sélectionnée pour "variable single"
  identify_variable_single_type <- function(variable) {
    if (data_loaded()) {
      valeurs_uniques_par_colonne_importees <- apply(imputed_data(), 2, unique)
      seuil <- 5
      variables_qualitatives <- names(valeurs_uniques_par_colonne_importees[sapply(valeurs_uniques_par_colonne_importees, length) <= seuil])
      variables_quantitatives <- names(valeurs_uniques_par_colonne_importees[sapply(valeurs_uniques_par_colonne_importees, length) > seuil])
      
      var_quant_discretes <- character(0)
      var_quant_continues <- character(0)
      for (var in variables_quantitatives) {
        donnees_quantitatives <- imputed_data()[, var]
        
        # Conditions pour distinguer entre variable quantitative discrète et continue
        if (is.numeric(donnees_quantitatives) && all.equal(donnees_quantitatives, round(donnees_quantitatives)) == TRUE) {
          var_quant_discretes <- c(var_quant_discretes, var)
        } else {
          var_quant_continues <- c(var_quant_continues, var)
        }
      }
      
      if (variable %in% var_quant_discretes) {
        return("quantitative_discrète")
      } else if (variable %in% var_quant_continues) {
        return("quantitative_continue")
      } else if (variable %in% variables_qualitatives) {
        return("qualitative")
      }
    }
  }
  
  # Observer pour détecter le changement dans la sélection de la variable
  observeEvent(input$variable_single, {
    var_type <- identify_variable_single_type(input$variable_single)
    selected_variable_single_type(var_type)
  })
  
  # Contenu pour l'onglet 'Analyse unidimensionnelle'
  output$univariate_analysis_ui <- renderUI({
    var_type <- selected_variable_single_type()
    if (var_type == "quantitative_discrète") {
      fluidRow(
        column(6, plotOutput("discreteBarPlot")),
        column(6, plotOutput("discreteCumulativePlot")),
        column(6, plotOutput("discreteBoxPlot")),
        column(6, tableOutput("discreteSummaryTable"))
      )
    } else if (var_type == "quantitative_continue") {
      fluidRow(
        column(6, plotOutput("continueHistogram")),
        column(6, plotOutput("continueCumulativePlot")),
        column(6, plotOutput("continueBoxPlot")),
        column(6, tableOutput("continueSummaryTable"))
      )
    } else if (var_type == "qualitative") {
        tagList()  # Laissez le résultat vide pour l'instant
      }
  })
  
  # Définition du diagramme en bâtons pour variable quantitative discrète
  output$discreteBarPlot <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_discrète") {
      discreteData <- imputed_data()[, input$variable_single]
      barplot(table(discreteData), col = "green4", xlab = input$variable_single, ylab = "Effectifs",
              main = paste("Distribution des effectifs pour",input$variable_single))
    }
  })
  
  # Définition du diagramme cumulatif pour variable quantitative discrète
  output$discreteCumulativePlot <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_discrète") {
      discreteData <- imputed_data()[, input$variable_single]
      plot(cumsum(table(discreteData)), col = "green4", xlab = input$variable_single, ylab = "Effectifs Cumulés",
           main = paste("Fréquences cumulées pour",input$variable_single), type = "b", lty = 2)
    }
  })
  
  # Définition du boxplot pour variable quantitative discrète
  output$discreteBoxPlot <- renderPlot({
    var_type <- selected_variable_single_type()
    
    if (!is.null(var_type) && var_type == "quantitative_discrète") {
      discreteData <- imputed_data()[, input$variable_single]
      boxplot(discreteData, col = "blue", main = input$variable_single)
    }
  })
  
  # Tableau statistique pour variable quantitative discrète
  output$discreteSummaryTable <- renderTable({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_discrète") {
      discreteData <- imputed_data()[, input$variable_single]
      summaryStats <- data.frame(
        Caractéristique = c("Maximum", "Minimum", "Moyenne", "Médiane",
                            "1e quartile", "3e quartile", "Variance", "Ecart-type"),
        Valeur = c(
          max(discreteData),
          min(discreteData),
          mean(discreteData),
          median(discreteData),
          quantile(discreteData)[2],
          quantile(discreteData)[4],
          var(discreteData),
          sd(discreteData)
        )
      )
      colnames(summaryStats) <- c("Caractéristique", "Valeur")
      summaryStats
    }
  })
  
  # Définition de l'histogramme pour variable quantitative continue
  output$continueHistogram <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_continue") {
      continueData <- imputed_data()[, input$variable_single]
      hist(continueData, col = "green4", xlab = input$variable_single, ylab = "Fréquence",
           main = paste("Histogramme de",input$variable_single))
    }
  })
  
  # Définition de la courbe cumulative pour variable quantitative continue
  output$continueCumulativePlot <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_continue") {
      continueData <- imputed_data()[, input$variable_single]
      plot(ecdf(continueData), col = "green4", xlab = input$variable_single, ylab = "Fréquence Cumulative",
           main = paste("Courbe Cumulative de",input$variable_single))
    }
  })
  
  output$continueBoxPlot <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_continue") {
      continueData <- imputed_data()[, input$variable_single]
      boxplot(continueData, col = "red", main = input$variable_single)
    }
  })
  
  # Définition du tableau statistique pour variable quantitative continue
  output$continueSummaryTable <- renderTable({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "quantitative_continue") {
      continueData <- imputed_data()[, input$variable_single]
      summaryStats <- data.frame(
        Caractéristique = c("Maximum", "Minimum", "Moyenne", "Médiane",
                            "1e quartile", "3e quartile", "Variance", "Ecart-type"),
        Valeur = c(
          max(continueData),
          min(continueData),
          mean(continueData),
          median(continueData),
          quantile(continueData)[2],
          quantile(continueData)[4],
          var(continueData),
          sd(continueData)
        )
      )
      colnames(summaryStats) <- c("Caractéristique", "Valeur")
      summaryStats
    }
  })
  
  

  
  # Ajoutez le code pour le prétraitement des données et l'entraînement des modèles ici
  
}

shinyApp(ui = ui, server = server)

                