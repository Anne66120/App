# Chargement des packages
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(DT)
library(htmltools)

# Fichiers de sauvegarde
planning_file <- "data/planning.csv"
messages_file <- "data/messages.csv"
patients_file <- "data/patients.csv"
dir.create("data", showWarnings = FALSE)

# Initialisation des fichiers
if (!file.exists(planning_file)) {
  write.csv(data.frame(
    Date=character(), Heure=character(), Professionnel=character(),
    Intervention=character(), Lieu=character(), Patient=character(),
    stringsAsFactors=FALSE
  ), planning_file, row.names=FALSE)
}
if (!file.exists(messages_file)) {
  write.csv(data.frame(
    text=character(), sender=character(), time=character(),
    stringsAsFactors=FALSE
  ), messages_file, row.names=FALSE)
}
if (!file.exists(patients_file)) {
  write.csv(data.frame(
    Nom=character(), Consentement=logical(),
    stringsAsFactors=FALSE
  ), patients_file, row.names=FALSE)
}

# Lecture des fichiers
schedule_data <- read.csv(planning_file, stringsAsFactors=FALSE)
patients_data <- read.csv(patients_file, stringsAsFactors=FALSE)
# Lecture robuste des messages
raw_msgs <- read.csv(messages_file, stringsAsFactors=FALSE)
if ("time" %in% names(raw_msgs) && nrow(raw_msgs)>0) {
  raw_msgs$time <- as.POSIXct(raw_msgs$time)
} else {
  raw_msgs$time <- character(0)
}
message_data <- raw_msgs


# Profils professionnels
professional_profiles <- data.frame(
  Nom = c("Enzo Manin", "Monique Lemoine", "Axel Couderc", "Elise Charpentier", "Anne Fontaine", "Judith Voireau", "Jean Dupuis", "Marion Lemans"),
  Profession = c("Enseignant(e) APA", "Médecin", "Enseignant(e) APA", "Psychologue", "Enseignant(e) APA", "Infirmièr(e)", "Ergothérapeute", "Psychomotricien(ne)"),
  stringsAsFactors = FALSE
)
user_credentials <- list(
  list(username="Anne Fontaine", password="APA"),
  list(username="Enzo Manin", password="APA"),
  list(username="Axel Couderc", password="APA"), 
  list(username="Monique Lemoine", password="APA"),
  list(username="Elise Charpentier", password="APA"),
  list(username="Jean Dupuis", password="APA")
)

# Page de connexion
login_page <- fluidPage(
  tags$head(tags$style(HTML("body { background-color: #f4f6f9; }"))),
  div(style="height:100vh; display:flex; align-items:center; justify-content:center;",
      box(width=4, status="primary", solidHeader=TRUE,
          title="Connexion à Siel Bleu Suivi",
          textInput("login_user","Nom d'utilisateur",placeholder="Entrez votre nom"),
          passwordInput("login_password","Mot de passe",placeholder="Entrez votre mot de passe"),
          actionButton("login_btn","Connexion",class="btn-primary"),
          br(), uiOutput("login_message")
      )
  )
)

# UI principale
get_app_ui <- function(current_user) {
  dashboardPage(
    title="Ciel Bleu Suivi",
    header=dashboardHeader(title=dashboardBrand(title="Siel Bleu Suivi", color="primary")),
    sidebar=dashboardSidebar(
      skin="light", status="primary",
      sidebarMenu(
        menuItem("Tableau de bord", tabName="dashboard", icon=icon("calendar-check")),
        menuItem("Patients", tabName="patients", icon=icon("user-injured")),
        menuItem("Messages", tabName="messages", icon=icon("comments")),
        menuItem("Paramètres", tabName="parametres", icon=icon("cog"))
      ),
      br(),
      div(style="position:absolute; bottom:30px; width:100%; text-align:center;",
          tags$i(class="fas fa-user-circle fa-3x", style="color:#007BFF;"),
          tags$p(current_user, style="margin:0; font-weight:bold;"),
          tags$small("Enseignant APA")
      )
    ),
    body=dashboardBody(
      tabItems(
        tabItem(tabName="dashboard",
                box(width=12, title="Planning des interventions", status="primary",
                    DTOutput("planning_table"), br(),
                    dateInput("new_date","Date"), textInput("new_time","Heure"),
                    selectInput("new_prof","Professionnel",choices=unique(professional_profiles$Profession)),
                    textInput("new_intervention","Intervention"), textInput("new_lieu","Lieu"),
                    textInput("new_patient","Patient"), actionButton("add_row","Ajouter",icon=icon("plus"),class="btn-primary")
                )
        ),
        tabItem(tabName="patients",
                fluidRow(
                  box(width=6, title="Patient", status="primary",
                      selectInput("patient_select","Patient :",choices=unique(schedule_data$Patient))
                  ),
                  box(width=6, title="Consentement et RGPD", status="info",
                      checkboxInput("consentement_check","Le patient autorise le partage de ses données de santé entre les professionnels impliqués dans sa prise en charge (médecin, kinésithérapeute, ergothérapeute, psychomotricien, enseignant APA), dans le respect du secret médical et du RGPD.",value=FALSE)
                  )
                ),
                DTOutput("historique_table")
        ),
        tabItem(tabName="messages",
                fluidRow(
                  box(width=12, title="Messagerie", status="info",
                      uiOutput("chat_history"), br(),
                      textAreaInput("message_box","Message :",rows=3,width="100%"),
                      actionButton("send_message","Envoyer",icon=icon("paper-plane"),class="btn-info")),
                  box(
                    width = 6,
                    title = "Liste des professionnels",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML(paste0(
                      "<ul>",
                      paste0("<li><strong>", professional_profiles$Nom, "</strong> — ", professional_profiles$Profession, "</li>", collapse = ""),
                      "</ul>"
                    ))
                  )
                  

                  )
                ),
        tabItem(tabName="parametres",
                box(width=6, title="Déconnexion", status="danger",
                    actionButton("logout","Se déconnecter",icon=icon("sign-out-alt"),class="btn-danger")
                )
        )
      )
    )
  )
}

# Serveur
server <- function(input, output, session) {
  user_logged <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  planning <- reactiveVal(schedule_data)
  patients <- reactiveVal(patients_data)
  message_history <- reactiveVal(if(nrow(message_data)>0) split(message_data, seq(nrow(message_data))) else list())
  
  observeEvent(input$login_btn, {
    valid <- sapply(user_credentials, function(cred) input$login_user==cred$username && input$login_password==cred$password)
    if(any(valid)) {
      current_user(input$login_user)
      user_logged(TRUE)
    } else {
      output$login_message <- renderUI({ tags$div(style="color:red;","Identifiants incorrects.") })
    }
  })
  
  output$main_ui <- renderUI({
    if (isTRUE(user_logged())) {
      get_app_ui(current_user())
    } else {
      login_page
    }
  })
  
  observeEvent(input$logout, {
    user_logged(FALSE)
    current_user(NULL)
    session$reload()
  })
  
  # Planning
  output$planning_table <- renderDT({ datatable(planning(),options=list(pageLength=5)) })
  observeEvent(input$add_row, {
    new_r <- data.frame(Date=as.character(input$new_date),Heure=input$new_time,
                        Professionnel=input$new_prof,Intervention=input$new_intervention,
                        Lieu=input$new_lieu,Patient=input$new_patient,stringsAsFactors=FALSE)
    df <- rbind(planning(),new_r); planning(df); write.csv(df,planning_file,row.names=FALSE)
    updateSelectInput(session,"patient_select",choices=unique(df$Patient))
    curr <- patients()
    if(!(input$new_patient%in%curr$Nom)){
      curr <- rbind(curr,data.frame(Nom=input$new_patient,Consentement=FALSE)); patients(curr); write.csv(curr,patients_file,row.names=FALSE)
    }
  })
  
  # Consentement
  observeEvent(input$patient_select, {
    cons <- patients()$Consentement[patients()$Nom==input$patient_select]
    updateCheckboxInput(session,"consentement_check",value=ifelse(length(cons)>0,cons,FALSE))
  })
  observeEvent(input$consentement_check, {
    curr<-patients()
    curr$Consentement[curr$Nom==input$patient_select]<-input$consentement_check
    patients(curr)
    write.csv(curr,patients_file,row.names=FALSE)
  })
  output$historique_table <- renderDT({
    df<-planning(); req(input$patient_select)
    datatable(df[df$Patient==input$patient_select,],options=list(dom='t'))
  })
  
  # Messagerie
  output$chat_history <- renderUI({
    msgs <- message_history()
    if(length(msgs)==0) return(NULL)
    tagList(
      lapply(msgs, function(msg) {
        cls <- if(msg$sender==current_user()) "message-bubble-user" else "message-bubble-professional"
        div(class=cls, strong(msg$sender), br(), msg$text, br(), tags$small(format(msg$time,"%H:%M:%S")))
      }),
      tags$script(HTML('var chatWindow=document.getElementById("chat_window"); if(chatWindow) chatWindow.scrollTop=chatWindow.scrollHeight;'))
    )
  })
  observeEvent(input$send_message, {
    req(input$message_box)
    new_m <- list(text=input$message_box, sender=current_user(), time=Sys.time())
    updated <- c(message_history(), list(new_m))
    message_history(updated)
    msg_df <- do.call(rbind, lapply(updated, as.data.frame))
    write.csv(msg_df, messages_file, row.names=FALSE)
    updateTextAreaInput(session, "message_box", value="")
  })
  
}



# UI wrapper
ui <- fluidPage(
  uiOutput("main_ui")
)

# Lancement de l'app
shinyApp(ui = ui, server = server)
