library(shiny)

chemin_hepatite_data <- "C:\\Users\\carlo\\Desktop\\R_Shiny_Hepatitis\\hepatite_data_correct.csv"

donnees_hepatite <- read.csv(chemin_hepatite_data, header = TRUE)

ui <- fluidPage(
  titlePanel('Shiny App'),
  tabsetPanel(
    tabPanel('Importation et visualisation de donnnees',
             sidebarLayout(
               sidebarPanel(
                 fileInput('file', 'Choisissez votre archive',
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 actionButton('load_data',"Charger les données"),
                 actionButton('summary_botton',"summary"),
               ),
               mainPanel(
                 tableOutput('table'),
                 verbatimTextOutput('summary'),
             )
    )
            
  ),
  tabPanel('Pretraitement de donnees',
             actionButton('remove_missing_button', 'Supprimer les valeurs manquantes'),
             dataTableOutput('preprocessed_data_table')
    ),
  tabPanel('Choix et entrainement du modele',
             selectInput('model_selection', 'Choisir un modèle', choices = c('Modèle 1', 'Modèle 2', 'Modèle 3')),
             actionButton('train_model_button', 'Entraîner le modèle')
             ),
  tabPanel('Exemple avec les donnees hepatitis', 
             dataTableOutput('data_table')
    )
  )
)

server <- function(input, output){

  output$data_table <- renderDataTable({
    donnees_hepatite
  })
  file_data <- eventReactive(input$load_data, {
    # Initialement, class(input$file1) = NULL
    # Après chargement, class(input$file1) = data.frame
    # avec les colonnes 'size', 'type', and 'datapath' columns. 
    inFile <- isolate(input$file)
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = FALSE)
  })
  output$table <- renderTable({
    head(file_data(),10 )
  })
  summary_stats <- reactive({
    summary(file_data())
  })
  output$summary <- renderPrint({
    req(input$summary_botton)
    summary_stats()
  })
  
}

shinyApp(ui = ui, server = server)