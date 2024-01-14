library(shiny)
library(DT)
library(reshape2)
library(ggplot2)

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
                   tabPanel('Analyse bidimensionnelle',
                            uiOutput('bivariate_analysis_ui'))
                 )
               )
             )
    ),
    tabPanel('Prétraitement de données',
      sidebarLayout(
         sidebarPanel(
         actionButton("dummify_button", "Dummification"),
         selectInput("normalization_method", "Méthodes de normalisation:", 
                    choices = c("Normalisation Min-Max", "Normalisation standardisée"),selected=NULL),
         actionButton("generate_final_table_button", "Générer le tableau final")
         ),
      mainPanel(
        tabsetPanel(
          tabPanel('Tableau dummifié', dataTableOutput('dummified_table')),
          tabPanel('Tableau normalisé', dataTableOutput('normalized_table')),
          tabPanel('Tableau final',dataTableOutput('final_table'))
        )
      )
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

server <- function(input, output,session){
#Partie importation et visualisation du data initial  
  #Importer et charger les données
  data_loaded <- reactiveVal(FALSE) 
  file_data <- reactiveVal(NULL)
  imputed_data <- reactiveVal(NULL)
  selected_variable_single_type <- reactiveVal(NULL)
  selected_variable_dual_type <- reactiveVal(list(NULL,NULL))
  normalized_data <- reactiveVal(NULL)
  dummified_data <- reactiveVal(NULL)
  
  
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
    if (data_loaded() & any(is.na(file_data()))) {
      imputed_data_val <- switch(input$imputation_option,
                             "Supprimer les lignes de NA" = na.omit(file_data()),
                             "Imputation par moyenne" = impute_mean(file_data()),
                             "Imputation par médiane" = impute_median(file_data()))
      imputed_data(imputed_data_val)
      return(imputed_data_val)
    }
    else {
      imputed_data_val <- file_data()
      imputed_data(file_data())
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
  
  # Analyse unidimensionnelle
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
        fluidRow(
          column(6,plotOutput("qualitativecolonne")),
          column(6,plotOutput("qualitativesecteur")),
          column(12,offset=3,tableOutput("qualitativetable"))
        )  
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
      # Tracé de la courbe des fréquences cumulées
      plot(ecdf(discreteData), col = "green4", 
           xlab = input$variable_single, 
           ylab = "Fréquences Cumulées",
           main = paste("Fréquences cumulées pour", input$variable_single),
      )
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
      hist(continueData, probability = TRUE, col = "green4", 
           xlab = input$variable_single, ylab = "Fréquence",
           main = paste("Histogramme de", input$variable_single))
      lines(density(continueData,bw=0.1), col = "red", lwd = 2)
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
  
  #variable qualitative
  output$qualitativecolonne <- renderPlot({
   var_type <- selected_variable_single_type()
   if (!is.null(var_type) && var_type == "qualitative") {
      qualitativeData <- imputed_data()[, input$variable_single]
      barplot(table(qualitativeData), main = paste("Catégories",input$variable_single), 
            ylab = "Effectifs", las = 1,
            names.arg =names(table(qualitativeData)))
    }
  })
  
  output$qualitativesecteur <- renderPlot({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "qualitative") {
      qualitativeData <- imputed_data()[, input$variable_single]
      pie(table(qualitativeData), labels = names(table(qualitativeData)),
        main = paste("Catégories", input$variable_single), col = c())
    }
  })
  
  output$qualitativetable <- renderTable({
    var_type <- selected_variable_single_type()
    if (!is.null(var_type) && var_type == "qualitative") {
      qualitativeData <- imputed_data()[, input$variable_single]
    effectifs <- table(qualitativeData)
    effectifs
  }}, colnames = FALSE)
  
  # Ajoutez le code pour le prétraitement des données et l'entraînement des modèles ici
  output$dummified_table <- renderDataTable({
      req(input$dummify_button)
      if (data_loaded()) {
      dummified_data_val <- imputed_data()
      var_qualitative_dummified <- model.matrix(~ . + 0, data = imputed_data()[, variables_qualitatives])
      dummified_data_val[,variables_qualitatives] <- var_qualitative_dummified
      dummified_data(dummified_data_val)
      return(dummified_data_val)
      }
  })
  
  output$normalized_table <- renderDataTable({
    req(input$normalization_method)
    if (data_loaded()) {
      # Normalisation Min-Max
      normalized_minmax_data <- imputed_data()
      normalized_minmax_data[, variables_quantitatives] <- round(apply(imputed_data()[, variables_quantitatives], 2, function(x) (x - min(x)) / diff(range(x))),2)
      
      # Normalisation standardisée
      normalized_standard_data <- imputed_data()
      normalized_standard_data[,variables_quantitatives] <- round(scale(imputed_data()[,variables_quantitatives]),2)
      normalized_data_val <- switch(input$normalization_method,
                                "Normalisation Min-Max" = normalized_minmax_data,
                                "Normalisation standardisée" = normalized_standard_data)
      normalized_data(normalized_data_val)
      return(normalized_data_val)
    }
  })
  
  output$final_table <- renderDataTable({
    req(input$dummify_button,input$normalization_method,input$generate_final_table_button)
    if (data_loaded()) {
      standard_dummified_data <- imputed_data()
      standard_dummified_data[,variables_quantitatives] <- normalized_data()[,variables_quantitatives]
      standard_dummified_data[,variables_qualitatives] <- dummified_data()[,variables_qualitatives]
      return(standard_dummified_data)
    }
  })
  
  # Analyse bidimensionnelle
  
  
  # Observer pour détecter le changement dans la sélection de la variable
  observeEvent(input$variable_dual, {
    variables_selectionnees <- input$variable_dual
    
    if (length(variables_selectionnees) == 2) {
      types <- lapply(variables_selectionnees, identify_variable_single_type)
      selected_variable_dual_type(types)
      }
    })
  
  # Contenu pour l'onglet 'Analyse bidimensionnelle'
  output$bivariate_analysis_ui <- renderUI({
    vars_types <- selected_variable_dual_type() #mettre à jour les types des 2 variables sélectionnées
    #dans vars_types y avait trois valeurs "quant discrète" "quant continue" et "qualit",mais là on veut que 2
    vars_types <- lapply(vars_types, function(type) if (type != "qualitative") "quantitative" else type)
    if (vars_types[1] == "quantitative" & vars_types[2]=="quantitative") {
      fluidRow(
        column(6,offset=2, plotOutput("NuagePointDroite")),
        column(6,offset=2, tableOutput("quantSummaryTable"))
      )
     } else if ( (vars_types[1] == "quantitative" & vars_types[2]=="qualitative") | 
                (vars_types[1] == "qualitative" & vars_types[2] == "quantitative") ) {
      fluidRow(
        column(6, plotOutput("boxplotGgplot")),
        column(6, tableOutput("quantqualitable"))
      )
    } else if (vars_types[1] == "qualitative" & vars_types[2]== "qualitative") {
      fluidRow(
        column(6,plotOutput("barplotprofil")),
        column(6,tableOutput("contingency")),
        column(6,tableOutput("correlations"))
      )  
    }
  })
  
  # quant vs quant 
  
  #Nuage de point et droité régression
  output$NuagePointDroite <- renderPlot({
    x.var = input$variable_dual[1]
    y.var = input$variable_dual[2]
    plot(x = imputed_data()[, x.var], y = imputed_data()[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    #Droite de régression linéaire (y~x) 
    abline(lm(imputed_data()[, y.var]~imputed_data()[, x.var]), col="red", lwd = 2)
  })
  
  #table statistique
  output$quantSummaryTable <- renderTable({
  x.var <- input$variable_dual[1]
  y.var <- input$variable_dual[2]
  mean_variable1 <- mean(imputed_data()[,x.var])
  sd_variable1 <- sd(imputed_data()[,x.var])
  mean_variable2 <- mean(imputed_data()[,y.var])
  sd_variable2 <- sd(imputed_data()[,y.var])
  correlation_coefficient <- cor(imputed_data()[,x.var], imputed_data()[,y.var])
  
  ligne1 <- c("moy. X", "moy. Y", "sd.X", "sd.Y", "corr(X,Y)")
  ligne2 <- c(mean_variable1,mean_variable2,sd_variable1,sd_variable2,correlation_coefficient)
  numeric_values <- t(as.matrix(ligne2))
  df <- data.frame(ligne1, ligne2, check.names = FALSE)
  colnames(df) <- NULL
  df <- as.data.frame(t(df), stringsAsFactors = FALSE)
  colnames(df) <- NULL
  return(df)
  })
  
  output$boxplotGgplot <- renderPlot({
    var1 <- input$variable_dual[1]
    var2 <- input$variable_dual[2]
    type_var1 <- identify_variable_single_type(var1)
    type_var2 <- identify_variable_single_type(var2)
    if (type_var1 == "qualitative" && type_var2 == "quantitative") {
      var_qualit <- var1
      var_quant <- var2
    } else if (type_var1 == "quantitative" && type_var2 == "qualitative") {
      var_qualit <- var2
      var_quant <- var1
    }
    selected_data <- imputed_data()[, c(var_quant, var_qualit)]
    ggplot(selected_data, aes(x = factor(get(var_qualit)), y = get(var_quant), fill = factor(get(var_qualit)))) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(width = 0.2), size = 2) +
      labs(x = "Modalités", y = "Mesures") +
      theme(legend.title = element_blank())
  })
  
  output$quantqualitable <- renderTable({
    vars_types <- selected_variable_dual_type() 
    vars_types <- lapply(vars_types, function(type) if (type != "qualitative") "quantitative" else type)
    indice_quant <- which(vars_types == "quantitative")
    indice_qualit <- which(vars_types == "qualitative")
    var_qualit <- input$variable_dual[indice_qualit]
    var_quant <- input$variable_dual[indice_quant]

    statsSummary <- data.frame()
    categories <- unique(imputed_data()[[var_qualit]])
    # Pour chaque catégorie de la variable qualitative binaire
    for (category in categories) {
      tmp_stats <- c(
        mean(imputed_data()[imputed_data()[[var_qualit]] == category, var_quant]),
        sd(imputed_data()[imputed_data()[[var_qualit]] == category, var_quant])
      )
      statsSummary <- rbind.data.frame(statsSummary, tmp_stats)
    }
    # Statistiques globales
    stats_total <- c(
      mean(imputed_data()[[var_quant]]),
      sd(imputed_data()[[var_quant]])
    )
    statsSummary <- cbind.data.frame(t(statsSummary), stats_total)
    # Définition des row/colnames
    nom_categories <- paste("catégorie",categories)
    colnames(statsSummary) <- c(nom_categories,"Total")
    rownames(statsSummary) <- c("Moyenne", "Écart-type")
    statsSummary},rownames = TRUE, digits = 1)
  
   output$barplotprofil <- renderPlot({
       # Diagramme de profils entre les variables 'x' et 'y'
       x_var <-input$variable_dual[1]
       y_var <-input$variable_dual[2]
       data_subset <- imputed_data()[, c(x_var, y_var)]
       data_subset[,x_var] <- factor(data_subset[,x_var],levels=unique(data_subset[,x_var]))
       data_subset[,y_var] <- factor(data_subset[,y_var],levels=unique(data_subset[,y_var]))
       ggplot(data_subset, aes_string(x = x_var, fill = y_var)) +
         geom_bar(position = "dodge", stat = "count", width = 0.7, color = "black") +
         scale_fill_manual(values = c("lightblue", "lightgreen"))
   })
   
   output$contingency <- renderTable({
       x_var <- input$variable_dual[1]
       y_var <- input$variable_dual[2]
       data_subset <- imputed_data()[,c(x_var,y_var)]
       # Créer une table de contingence croisée
       tab <- table(data_subset[,x_var], data_subset[,y_var])
       tab_df <- as.data.frame(tab)
       names(dimnames(tab)) <- c(x_var, y_var)
       tab
     })
   
   output$correlations <- renderTable({
       x <- input$variable_dual[1]
       y <- input$variable_dual[2]
       data_subset <- imputed_data()[,c(x,y)]
       df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
       rownames(df) = c("X2", "Phi2", "Cramer")
       # La table de contingence des profils observés
       tab = table(data_subset[,x], data_subset[,y])
       # La table de contigence s'il y a indépendence
       tab_indep = tab
       n = sum(tab)
       tab_rowSum = apply(tab, 2, sum)
       tab_colSum = apply(tab, 1, sum)
       
       for(i in c(1:length(tab_colSum))){
         for(j in c(1:length(tab_rowSum))){
           tab_indep[i,j] = tab_colSum[i]*tab_rowSum[j]/n
         }
       }
       
       # Calcul du X²
       df[1,1] = sum((tab-tab_indep)^2/tab_indep)
       # Calcul du Phi²
       df[2,1] = df[1,1]/n
       # Calcul du Cramer
       df[3,1] = sqrt(df[2,1]/(min(nrow(tab), ncol(tab))-1))
       
       df
       
     }, rownames=TRUE, colnames=FALSE)

}



shinyApp(ui = ui, server = server)

                