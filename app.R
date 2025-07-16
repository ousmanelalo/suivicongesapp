# app.R

# 1. Chargement des packages nécessaires
library(shiny)
library(googlesheets4)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)   # Pour la gestion des dates
library(forcats)     # Pour fct_reorder
library(stringr)     # Pour str_extract
library(DT)          # Pour les tableaux interactifs
library(fullcalendar) # Utilisation du package 'fullcalendar'

# --- UI (User Interface) : Ce que l'utilisateur voit ---
ui <- dashboardPage(
  dashboardHeader(
    title = tags$a(href = 'https://docs.google.com/spreadsheets/d/1cCDD5tIA23P7-ftVwfu-lI_Jk8zbO2T-fcWeNJPPZFc/edit?usp=sharing', # Lien facultatif si vous cliquez sur le logo
                   tags$img(src = 'Logo transparent Blanc GeoA.png', # REMPLACEZ PAR LE NOM DE VOTRE FICHIER LOGO
                            height = '40', # Ajustez la hauteur selon vos besoins
                            width = 'auto', # Conserve les proportions
                            style = "margin-top: 0px; margin-left: 0px;")) # Ajustez la position si nécessaire
  ),
  dashboardSidebar(
    selectInput(
      inputId = "select_employe",
      label = "Sélectionner un employé :",
      choices = NULL,
      selected = "Tous les employés"
    )
  ),
  dashboardBody(
    tags$head(
      # NOUVEAU : Chargement des bibliothèques JavaScript externes pour le calendrier
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.1/moment.min.js"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/qtip2/3.0.3/jquery.qtip.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/qtip2/3.0.3/jquery.qtip.min.js"),
      
      tags$style(HTML("
        /* Style pour le corps principal de la page */
        body {
          background-color: #f0f2f5;
          font-family: 'tahoma';
        }

        /* Style pour le header (barre de titre) */
        .main-header .logo {
          background-color: #a26328 !important;
          color: #ecf0f1 !important;
          font-weight: bold;
          font-size: 20px;
        }
        .main-header .navbar {
          background-color: #996633 !important;
        }
        .main-header .logo:hover {
          background-color: #654422 !important;
        }
        .main-header .sidebar-toggle:before {
          color: #ecf0f1 !important;
        }

        /* Style pour la sidebar (barre latérale) */
        .main-sidebar {
          background-color: #a26328 !important;
        }
        .sidebar-menu li a {
          color: #ecf0f1 !important;
        }
        .sidebar-menu li.active a {
          border-left-color: #000 !important;
        }
        .sidebar-menu li a:hover {
          background-color: #654422 !important;
          color: #ffffff !important;
        }
        .irs-bar, .irs-bar-edge, .irs-single, .irs-bar-horizontal {
          background: #000 !important;
          border-top-color: #000 !important;
          border-bottom-color: #000 !important;
        }
        .irs-from, .irs-to, .irs-min, .irs-max {
          background-color: #000 !important;
        }

        /* Style général des boîtes (box) */
        .box {
          border-radius: 0px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          background-color: #ffffff;
          padding: 15px;
          margin-bottom: 15px;
        }

        /* Correction des marges horizontales entre les colonnes */
        .col-sm-3, .col-sm-4, .col-sm-6, .col-sm-12 {
          padding-left: 5px !important;
          padding-right: 5px !important;
        }
        
        /* Ajustement des titres h2/h3 */
        h2, h3 {
          padding-left: 10px;
          padding-top: 10px;
          padding-bottom: 10px;
          font-size: 20px;
          font-weight: bold;
          color: #000;
        }

        /* Style pour les icônes à côté du texte */
        .info-icon {
          font-size: 30px;
          vertical-align: middle;
          margin-right: 15px;
        }
        /* Style pour les valeurs numériques */
        .info-value {
            font-size: 20px;
            font-weight: bold;
            color: #000;
        }
        /* Style pour le titre des boîtes (y compris celle du graphique) */
        .box-header .box-title {
          color: # !important; /* Cette ligne semble vide, peut être supprimée ou définie */
          font-weight: bold;
        }
        /* Style pour l'en-tête des boîtes solidHeader comme celle du graphique */
        .box.box-solid.box-primary > .box-header {
          background-color: #a26328 !important;
          color: #FFF !important;
        }

        /* Style pour les onglets du tabBox */
        .nav-tabs-custom > .nav-tabs > li.active > a {
          border-top-color: #1abc9c !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a,
        .nav-tabs-custom > .nav-tabs > li.active > a:hover,
        .nav-tabs-custom > .nav-tabs > li.active > a:active {
          background-color: #ffffff !important;
          color: #34495e !important;
        }
        .nav-tabs-custom > .nav-tabs > li > a {
          color: #a26328 !important;
        }

        /* NOUVEAU : CSS pour le calendrier FullCalendar */
        /* Assure que le fond du calendrier est blanc */
        .fc-view-container, .fc-view, .fc-scroller, .fc-bg, .fc-unthemed td.fc-today {
          background-color: #ffffff !important;
        }
        /* Assure que le conteneur principal du calendrier est également blanc */
        .fc {
          background-color: #ffffff !important;
        }
        /* Assure que le conteneur du calendrier ne déborde pas */
        .fc-scroller {
          overflow-y: auto !important; /* Permet le défilement si le contenu dépasse */
          overflow-x: hidden !important; /* Cache le défilement horizontal */
        }
      "))
    ),
    
    fluidRow(
      h2("Suivi congés Staff"),
      # Chaque information est dans une 'box' individuelle avec sa propre sortie UI
      column(width = 3, uiOutput("info_fonction")),
      column(width = 3, uiOutput("info_jours_pris")),
      column(width = 3, uiOutput("info_jours_dus")),
      column(width = 3, uiOutput("info_jours_restants"))
    ),
    
    # NOUVEAU : tabBox pour organiser les sections Graphique, Historique et Calendrier
    fluidRow(
      tabBox(
        title = "Détails et Historique des congés", # Titre du tabBox
        id = "tabset1", height = "600px", width = 12, # Ajustez la hauteur si nécessaire
        tabPanel("Graphique jours pris",
                 h3("Détail des Jours Pris par Période"),
                 box(
                   title = "Jours pris par mois et semaine",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   plotOutput("jours_pris_detail_plot")
                 )
        ),
        tabPanel("Historique des congés",
                 h3("Historique détaillé des congés"),
                 DT::dataTableOutput("conges_historique_table")
        ),
        tabPanel("Calendrier des congés",
                 h3("Calendrier interactif des congés"),
                 fullcalendarOutput("conges_calendar", height = "100%")
        )
      )
    )
  )
)

# --- Server : La logique de l'application ---
server <- function(input, output, session) {
  
  # URL de votre Google Sheet (pour les deux onglets, si c'est le même fichier)
  sheet_url <- "https://docs.google.com/spreadsheets/d/1cCDD5tIA23P7-ftVwfu-lI_Jk8zbO2T-fcWeNJPPZFc/edit?usp=sharing"
  
  # Nom de votre onglet principal (Ex: "app")
  sheet_name_main <- "app"
  # Nom de votre nouvel onglet pour les détails des congés (Ex: "app2")
  sheet_name_details <- "app2" # Assurez-vous que cet onglet existe dans votre Google Sheet
  
  period_order_levels <- c(
    paste0("JAN_S", 1:4), paste0("FEV_S", 1:4), paste0("MAR_S", 1:4),
    paste0("AVR_S", 1:4), paste0("MAI_S", 1:4), paste0("JUI_S", 1:4),
    paste0("JUL_S", 1:4), paste0("AOU_S", 1:4), paste0("SEP_S", 1:4),
    paste0("OCT_S", 1:4), paste0("NOV_S", 1:4), paste0("DEC_S", 1:4)
  )
  
  data_conges_raw <- reactive({
    req(sheet_url)
    tryCatch({
      data <- read_sheet(sheet_url, sheet = sheet_name_main, col_names = TRUE, gs4_deauth())
      
      data <- data %>%
        rename_with(~ case_when(
          .x == "Nom et Prénoms" ~ "Nom_Prenoms",
          .x == "Nbr jours pris" ~ "Jours_Pris_Total",
          .x == "jours dûs" ~ "Jours_Dus",
          .x == "Jours restants" ~ "Jours_Restants",
          TRUE ~ .x
        )) %>%
        mutate(
          Jours_Pris_Total = as.numeric(Jours_Pris_Total),
          Jours_Dus = as.numeric(Jours_Dus),
          Jours_Restants = as.numeric(Jours_Restants)
        )
      return(data)
    }, error = function(e) {
      showNotification(paste("Erreur de lecture du Google Sheet (Principal):", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Lecture des données détaillées des congés (onglet 'app2')
  data_conges_details <- reactive({
    req(sheet_url)
    tryCatch({
      data <- read_sheet(sheet_url, sheet = "app2", col_names = TRUE, gs4_deauth())
      
      data <- data %>%
        rename_with(~ case_when(
          .x == "Nom et Prénoms" ~ "Nom_Prenoms",
          .x == "Type de congé" ~ "Type_Conge",
          .x == "Date de début" ~ "Date_Debut",
          .x == "Date de fin" ~ "Date_Fin",
          .x == "Jours ouvrés pris" ~ "Jours_Ouvres_Pris",
          .x == "Commentaire" ~ "Commentaire",
          TRUE ~ .x
        ))
      
      data <- data %>%
        mutate(
          Date_Debut = ymd(Date_Debut),
          Date_Fin = ymd(Date_Fin),
          Jours_Ouvres_Pris = as.numeric(Jours_Ouvres_Pris)
        )
      return(data)
    }, error = function(e) {
      showNotification(paste("Erreur de lecture du Google Sheet (Détails):", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  observe({
    data <- data_conges_raw()
    if (!is.null(data) && "Nom_Prenoms" %in% names(data)) {
      employes <- sort(unique(data$Nom_Prenoms))
      updateSelectInput(session, "select_employe", choices = c("Tous les employés", employes), selected = "Tous les employés")
    }
  })
  
  selected_data_for_display <- reactive({
    data <- data_conges_raw()
    # CORRECTION ICI : Remplacé is.is.null par is.null
    if (is.null(data) || is.null(input$select_employe) || input$select_employe == "") {
      return(NULL)
    }
    
    if (input$select_employe == "Tous les employés") {
      data %>%
        summarise(
          Nom_Prenoms = "Tous les employés",
          Fonction = "Global",
          Jours_Pris_Total = sum(Jours_Pris_Total, na.rm = TRUE),
          Jours_Dus = sum(Jours_Dus, na.rm = TRUE),
          Jours_Restants = sum(Jours_Restants, na.rm = TRUE)
        )
    } else {
      data %>% filter(Nom_Prenoms == input$select_employe) %>% head(1)
    }
  })
  
  selected_employe_detail_data <- reactive({
    data_raw <- data_conges_raw()
    if (is.null(data_raw) || is.null(input$select_employe) || input$select_employe == "") {
      return(NULL)
    }
    
    if (input$select_employe != "Tous les employés") {
      data_filtered <- data_raw %>% filter(Nom_Prenoms == input$select_employe)
    } else {
      data_filtered <- data_raw
    }
    
    detail_cols <- names(data_filtered)[!names(data_filtered) %in% c("Nom_Prenoms", "Fonction", "Jours_Pris_Total", "Jours_Dus", "Jours_Restants")]
    
    if (length(detail_cols) == 0) {
      return(NULL)
    }
    
    employe_data_long <- data_filtered %>%
      select(Nom_Prenoms, all_of(detail_cols)) %>%
      pivot_longer(
        cols = all_of(detail_cols),
        names_to = "Periode",
        values_to = "Jours_Pris_Detail"
      ) %>%
      mutate(Jours_Pris_Detail = as.numeric(Jours_Pris_Detail)) %>%
      mutate(Jours_Pris_Detail = ifelse(is.na(Jours_Pris_Detail), 0, Jours_Pris_Detail))
    
    if (input$select_employe == "Tous les employés") {
      employe_data_long <- employe_data_long %>%
        group_by(Periode) %>%
        summarise(Jours_Pris_Detail = sum(Jours_Pris_Detail, na.rm = TRUE)) %>%
        ungroup()
    }
    
    employe_data_long <- employe_data_long %>%
      mutate(Periode_Ordered = factor(Periode, levels = period_order_levels)) %>%
      filter(!is.na(Periode_Ordered)) %>%
      arrange(Periode_Ordered)
    
    return(employe_data_long)
  })
  
  # --- Rendu des éléments UI (contenu des box individuelles) ---
  output$info_fonction <- renderUI({
    employe_info <- selected_data_for_display()
    if (is.null(employe_info)) { return(box(title = "Fonction", width = NULL, p("Sélectionnez un employé"))) }
    title_text <- if (input$select_employe == "Tous les employés") "du personnel" else paste("de", input$select_employe)
    box(
      title = HTML(paste('<i class="fa fa-briefcase info-icon" style="color:#6a1b9a;"></i> Fonction', title_text)),
      width = NULL, p(class = "info-value", employe_info$Fonction)
    )
  })
  
  output$info_jours_pris <- renderUI({
    employe_info <- selected_data_for_display()
    if (is.null(employe_info)) { return(box(title = "Jours pris (Total)", width = NULL, p("Sélectionnez un employé"))) }
    title_text <- if (input$select_employe == "Tous les employés") "du personnel" else paste("de", input$select_employe)
    box(
      title = HTML(paste('<i class="fa fa-calendar-check info-icon" style="color:#d32f2f;"></i> Jours pris (Total)', title_text)),
      width = NULL, p(class = "info-value", employe_info$Jours_Pris_Total)
    )
  })
  
  output$info_jours_dus <- renderUI({
    employe_info <- selected_data_for_display()
    if (is.null(employe_info)) { return(box(title = "Jours dus", width = NULL, p("Sélectionnez un employé"))) }
    title_text <- if (input$select_employe == "Tous les employés") "du personnel" else paste("de", input$select_employe)
    box(
      title = HTML(paste('<i class="fa fa-calendar-plus info-icon" style="color:#388e3c;"></i> Jours dus', title_text)),
      width = NULL, p(class = "info-value", employe_info$Jours_Dus)
    )
  })
  
  output$info_jours_restants <- renderUI({
    employe_info <- selected_data_for_display()
    if (is.null(employe_info)) { return(box(title = "Jours restants", width = NULL, p("Sélectionnez un employé"))) }
    title_text <- if (input$select_employe == "Tous les employés") "du personnel" else paste("de", input$select_employe)
    box(
      title = HTML(paste('<i class="fa fa-calendar-alt info-icon" style="color:#1976d2;"></i> Jours restants', title_text)),
      width = NULL, p(class = "info-value", employe_info$Jours_Restants)
    )
  })
  
  # Rendu du graphique des jours pris par période
  output$jours_pris_detail_plot <- renderPlot({
    detail_data <- selected_employe_detail_data()
    
    if (is.null(detail_data) || nrow(detail_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée détaillée pour la sélection actuelle ou les données ne sont pas au format attendu.", size = 5, color = "gray"))
    }
    
    plot_title <- if (input$select_employe == "Tous les employés") {
      "Détail des jours pris par le personnel (Total)"
    } else {
      paste("Détail des jours pris par", input$select_employe)
    }
    
    ggplot(detail_data, aes(x = Periode_Ordered, y = Jours_Pris_Detail, fill = Jours_Pris_Detail)) +
      geom_bar(stat = "identity", color = "white") +
      scale_fill_gradient(low = "#a26328", high = "#654422", na.value = "gray") +
      labs(
        title = plot_title,
        x = "Période (Mois_Semaine)",
        y = "Nombre de jours pris"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#000", face = "bold", size = 16),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        legend.position = "none"
      ) +
      geom_text(aes(label = Jours_Pris_Detail), vjust = -0.5, size = 3, color = "black")
  })
  
  # Lecture des données détaillées des congés (onglet 'app2')
  data_conges_details <- reactive({
    req(sheet_url)
    tryCatch({
      data <- read_sheet(sheet_url, sheet = "app2", col_names = TRUE, gs4_deauth())
      
      data <- data %>%
        rename_with(~ case_when(
          .x == "Nom et Prénoms" ~ "Nom_Prenoms",
          .x == "Type de congé" ~ "Type_Conge",
          .x == "Date de début" ~ "Date_Debut",
          .x == "Date de fin" ~ "Date_Fin",
          .x == "Jours ouvrés pris" ~ "Jours_Ouvres_Pris",
          .x == "Commentaire" ~ "Commentaire",
          TRUE ~ .x
        ))
      
      data <- data %>%
        mutate(
          Date_Debut = ymd(Date_Debut),
          Date_Fin = ymd(Date_Fin),
          Jours_Ouvres_Pris = as.numeric(Jours_Ouvres_Pris)
        )
      return(data)
    }, error = function(e) {
      showNotification(paste("Erreur de lecture du Google Sheet (Détails):", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Rendu du tableau historique des congés (onglet 'app2')
  output$conges_historique_table <- DT::renderDataTable({
    details_data <- data_conges_details()
    if (is.null(details_data)) return(NULL)
    
    if (input$select_employe == "Tous les employés") {
      # Afficher tous les congés pour tous les employés
      display_data <- details_data %>%
        select(Nom_Prenoms, Type_Conge, Date_Debut, Date_Fin, Jours_Ouvres_Pris, Commentaire)
    } else {
      # Filtrer par l'employé sélectionné
      display_data <- details_data %>%
        filter(Nom_Prenoms == input$select_employe) %>%
        select(Type_Conge, Date_Debut, Date_Fin, Jours_Ouvres_Pris, Commentaire)
    }
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10, # Nombre de lignes par page
        lengthMenu = c(5, 10, 15, 20), # Options de nombre de lignes
        dom = 'Blfrtip', # Pour afficher les boutons d'exportation
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), # Boutons d'exportation
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json') # Traduction
      ),
      extensions = 'Buttons', # Nécessaire pour les extensions de boutons
      rownames = FALSE,
      selection = 'none',
      class = 'display compact'
    )
  })
  
  # Rendu du calendrier des congés (onglet 'app2')
  output$conges_calendar <- renderFullcalendar({
    details_data <- data_conges_details()
    if (is.null(details_data)) return(NULL)
    
    if (input$select_employe != "Tous les employés") {
      details_data <- details_data %>%
        filter(Nom_Prenoms == input$select_employe)
    }
    
    # Préparer les événements pour fullcalendar
    events_data <- details_data %>%
      mutate(
        id = row_number(),
        title = paste(Nom_Prenoms, "-", Type_Conge),
        start = Date_Debut,
        end = Date_Fin + days(1), # fullcalendar exclut la date de fin, donc +1 jour
        color = case_when( # Couleurs personnalisées pour les types de congés
          Type_Conge == "Annuel" ~ "#1abc9c",     # Vert turquoise
          Type_Conge == "Maladie" ~ "#e74c3c",    # Rouge brique
          Type_Conge == "Sans Solde" ~ "#f39c12", # Orange
          Type_Conge == "Maternité" ~ "#9b59b6",  # Violet
          TRUE ~ "#3498db" # Bleu par défaut
        )
      ) %>%
      select(id, title, start, end, color)
    
    fullcalendar(events_data,
                 settings = list(
                   header = list(
                     left = "prev,next today",
                     center = "title",
                     right = "dayGridMonth,timeGridWeek,timeGridDay"
                   ),
                   locale = "fr",
                   nowIndicator = TRUE,
                   eventRender = JS("function(event, element) {
          $(element).qtip({
            content: '<b>' + event.title + '</b><br/>' +
                     'Du ' + moment(event.start).format('DD/MM/YYYY') +
                     ' au ' + moment(event.end).subtract(1, 'days').format('DD/MM/YYYY'),
            position: {
              my: 'bottom center',
              at: 'top center'
            },
            style: {
              classes: 'qtip-light qtip-shadow'
            }
          });
        }")
                 )
    )
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
