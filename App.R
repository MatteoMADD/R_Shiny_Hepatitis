library(shiny)
library(shinyjs)


chemin_hepatite_data <- "https://raw.githubusercontent.com/MatteoMADD/R_Shiny_Hepatitis/main/hepatite_data_correct.csv"

donnees_hepatite <- read.csv(chemin_hepatite_data, header = TRUE)

ui <- fluidPage(
  useShinyjs(), 
  titlePanel('Shiny App'),
  tabsetPanel(
    tabPanel('Importation et visualisation de donnnées',
             sidebarLayout(
               sidebarPanel(
                 fileInput('file', 'Choisissez votre fichier',
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 actionButton('load_data',"Charger les données"),
                 actionButton('afficher_donnees_button', 'Afficher les données'),
                 actionButton('summary_button',"Sommaire"),
                 actionButton('valeurs_manquantes_button',"Valeurs manquantes"),
                 actionButton('boite_moustache_button', 'Boite à moustaches'),
                 
               ),
               mainPanel(
                 tableOutput('table'),
                 verbatimTextOutput('text'),
                 verbatimTextOutput('summary'),
                 verbatimTextOutput("valeurs_manquantes"),
                 plotOutput("boite_moustache")
             )
    )
            
  ),
  tabPanel('Prétraitement de données',
             actionButton('remove_missing_button', 'Supprimer les valeurs manquantes'),
             dataTableOutput('preprocessed_data_table')
    ),
  tabPanel('Choix et entraînement du modèle',
             selectInput('model_selection', 'Choisir un modèle', choices = c('Modèle 1', 'Modèle 2', 'Modèle 3')),
             actionButton('train_model_button', 'Entraîner le modèle')
             ),
  tabPanel('Exemple avec les données hepatitis', 
             dataTableOutput('data_table')
    )
  )
)

server <- function(input, output){
  
  # Partie Exemple avec les données hepatitis
  
  output$data_table <- renderDataTable({
    donnees_hepatite
  })
  
  
  # Partie Importation et visualisation de donnnées 
  afficher_message <- reactiveVal(FALSE)
  
  file_data <- eventReactive(input$load_data, {
    inFile <- isolate(input$file)
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = TRUE)
    
  })
  observeEvent(file_data(), {
    if (!is.null(file_data())) {
      afficher_message(TRUE)
    } else {
      afficher_message(FALSE)
    }
  })
  output$text <- renderText({
    req(afficher_message() == TRUE)
    "Les données ont été chargées correctement."
  })
  
  output$table <- renderTable({
    req(input$afficher_donnees_button)
    if (is.null(file_data())) return(NULL)
    head(file_data(),10 )
  })
  summary_stats <- reactive({
    if (is.null(file_data())) return(NULL)
    summary(file_data())
  })
  output$summary <- renderPrint({
    req(input$summary_button)
    if (is.null(file_data())) return(NULL)
    summary_stats()
    type_donnes <- str(file_data())
    print(type_donnes)
    valeurs_uniques_par_colonne <- apply(file_data(), 2, unique)
  })
  output$valeurs_manquantes <- renderPrint({
    req(input$valeurs_manquantes_button)
    if (is.null(file_data())) return(NULL)
    valeurs_manquantes <- is.na(file_data())
    nb_valeurs_manquantes_par_colonne <- colSums(valeurs_manquantes)
    print(nb_valeurs_manquantes_par_colonne)
  })
  output$boite_moustache <- renderPlot({
    req(input$boite_moustache_button)
    if (is.null(file_data())) return(NULL)
    boite_moustache <- boxplot(file_data(), las = 2, main = "Boîte à moustaches pour chaque variable")
  })
  
  observe({
    shinyjs::toggle("table", condition = input$afficher_donnees_button %% 2 == 1)
  })
  
  observe({
    shinyjs::toggle("summary", condition = input$summary_button %% 2 == 1)
  })
  observe({
    shinyjs::toggle("valeurs_manquantes", condition = input$valeurs_manquantes_button %% 2 == 1)
  })
  observe({
    shinyjs::toggle("boite_moustache", condition = input$boite_moustache_button %% 2 == 1)
  })
  observeEvent(c(input$afficher_donnees_button, input$summary_button, input$valeurs_manquantes_button, input$boite_moustache_button), {
    afficher_message(FALSE)
  })
  
  # Partie Prétraitement de données
  
  file_data_processed <- reactive({
    req(input$remove_missing_button, file_data())
    na.omit(file_data())
  })
  
  output$preprocessed_data_table <- renderDataTable({
    file_data_processed()
  })
  
  observe({
    shinyjs::toggle("preprocessed_data_table", condition = input$remove_missing_button %% 2 == 1)
  })
}

shinyApp(ui = ui, server = server)