#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(encoding = "UTF-8")  
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    theme = shinytheme("cerulean"),
    title = "Etude Nhanes",

#----------------------------------------------------------------  
#tabPanel : PRESENTATION
#----------------------------------------------------------------    
    
    tabPanel(
      title = "Présentation",
      
      # Application title
      titlePanel(h1(br(),"Projet",span(strong("Nhanes 2015-2016", style = "color:blue"))," : recherche de modèles diagnostiques pour l'hypertension, le cholestérol et le diabète 
                 à partir de donnéees démographiques, de santé, d'alimentation et d'habitudes de vie",align="center",
                    hr(),
                    br(),
                    h1("")
                    )),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
        radioButtons(inputId = "idRadio", label = "Sommaire", selected = 3,
                     choices = c("Page de garde" = 1, "Les données" = 2, "La démarche" = 3))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        conditionalPanel(condition="input.idRadio == 1",
                         br(),
                         br(),
                         h2("Présentation par:",span("Insaf DRIS HAMED, Yifan ZHENG, Jean-Vincent PALLON",style = "color:blue"),align="center"),
                         br(),
                         h3("Le 27 février 2019",align="center"),
                         br(),
                         br(),
                         img(src="img/nhanes.jpg", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;"), 
                         br(),
                         br(),
                         img(src="img/NHANES Mobile Van_Pic 1.png", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;")),
        
        conditionalPanel(condition="input.idRadio == 2",
                         h2("Les données viennent du site ",
                            a("Centers for Disease Control and Prevention",
                            href="https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015")),
                         br(),
                         h2(span(strong("83 tables"))," XPT (package SASxport)"),
                         h2(span(strong("9971 individus"))," âgés de 0 à 80 ans"),
                         h2(span(strong("1893 variables"))," réparties en 4 catégories:"),
                         br(),
                         h3("-",span(strong("démographiques")),": âge, sexe, niveau d'éducation, revenus..."),
                         h3("-",span(strong("médicales")),":"),
                         h4("* caractéristiques générales: poids, taille, IMC, santé dentaire..."),
                         h4("* analyses de laboratoire: plomb, insuline, test d'urine..."),
                         h4("* maladies déjà connues: hypertension, cholestérol, diabète, problème coronaires, infarctus..."),
                         h3("-", span(strong("alimentaires")),": les menus complets de 2 jours non consécutifs d'alimentation"),
                         h3("-",span(strong("habitudes de vie")),": alcool, drogue, assuré ou non, propriétaire ou locataire, activité physique...")),
                         
                    
                         
        conditionalPanel(condition="input.idRadio == 3",
                         h1("Phase 1: Préparation des données générale"),
                         h3("Regroupement des tables, pré-sélection métier de 150 variables pour les 3 maladies -élimination des data trop techniques, trop détaillées ou non éthiques-,
                            ,web scrapping des libellés des variables qualitatives...1 jeu de données initial commun pour les 3 maladies - 8339X150"),
                         h1("Phase 2: Construction d'un jeu de données spécifique par maladie"),
                         h3("Recodage de variables, élimination des variables avec plus de 10% de NA, imputation des données manquantes -package MICE-"),
                         h1("Phase 3: Modélisation"),
                         h3("Sélection de variables, sélection de modèles"),
                         h1("Phase 4: Construction de l'outil shiny pour la visualisation et d'un package pour synthétiser l'ensemble du projet"),
                         br(),
                         h3("Travail avec Github pour le versioning")
                         
                         
                         )
        
        ))
    ),
    
    
#----------------------------------------------------------------  
#tabPanel : CONTEXTE
#----------------------------------------------------------------    
    
    
    tabPanel(
      title = "Contexte",
      tableOutput(outputId = "tableHypertension"),
      tableOutput(outputId = "tableCholesterol"),
      tableOutput(outputId = "tableDiabetes")
    ),
    
#----------------------------------------------------------------  
#navbarMenu : CHOIX MODELE
#----------------------------------------------------------------    

      navbarMenu(
      title = "Choix modele",
      
      # Choix modele pour Diabete
      tabPanel("modele diabete",
               navlistPanel(widths=c(2,10),
                            tabPanel(title = "choix variable",
                                     fluidRow(
                              column(
                                width = 1,
                                title = "parametre diab",dropdownButton(
                                  tags$h3("List of Input"),
                                  sliderInput("priodia", "Rang d'importance", 1, 10, value = 1),
                                  tooltip = tooltipOptions(title = "Click to see inputs !")
                                )),column(width = 11,
                                          dataTableOutput(outputId = "tabselvardia")))),
                            
                            tabPanel(title = "modele prediction",
                                     fluidRow(
                                       column(
                                         width=4,
                                         pickerInput(
                                           "methode",
                                           label = "Choisir la methode",
                                           choices = colnames(res_chol[, -1]),
                                           multiple = TRUE,
                                           selected = colnames(res_chol[, c(2,6,10)]),
                                           options = list(
                                             `actions-box` = FALSE,
                                             `none-selected-text` = "Selectionner 3!",
                                             `max-options` = 3
                                           )
                                         )
                                       ),
                                       column(width=4,
                                              sliderInput("seuilmod", "Choisir le seuil de discrimination", 0.2,0.8,0.5,0.1)
                                       )
                                     ),
                                     conditionalPanel(
                                       condition="input.methode.length==3",
                                       fluidRow(
                                         plotOutput("choixmethode_dia")),
                                       column(width=4,
                                              HTML("<h4>Les valeurs d'AUC</h4>"),
                                              tableOutput("valAUC_dia"),
                                              HTML("<h4>Les valeurs de precision</h4>"),
                                              tableOutput("matprecision_dia")
                                       ),
                                       column(width=4,
                                              HTML("<h4>Matrice de confusion</h4>"),     
                                              tableOutput("matconf_dia")
                                       )
                                     )))),
                            
                            
               
      
      # Choix modele pour Cholesterol
      tabPanel("modele cholesterol",
               navlistPanel(widths=c(2,10),
                            tabPanel(title = "choix variable",
                                     fluidRow(
                                       column(
                                         width = 1,
                                         title = "parametre chol",
                                         dropdownButton(
                                           tags$h3("List of Input"),
                                           sliderInput("priochol", "Rang d'importance", 1, 10, value = 1),
                                           tooltip = tooltipOptions(title = "Click to see inputs !")
                                         )
                                       ),
                                       column(width = 11,
                                              dataTableOutput(outputId = "tabselvarchol"))
                                     )),
                            tabPanel(title = "modele prediction",
                                     fluidRow(
                                       column(
                                         width=4,
                                         pickerInput(
                                           "methodechol",
                                           label = "Choisir la methode",
                                           choices = colnames(res_chol[, -1]),
                                           multiple = TRUE,
                                           selected = colnames(res_chol[, c(2,6,10)]),
                                           options = list(
                                             `actions-box` = FALSE,
                                             `none-selected-text` = "Selectionner 3!",
                                             `max-options` = 3
                                           )
                                         )
                                       ),
                                       column(width=4,
                                              sliderInput("seuilmodchol", "Choisir le seuil de discrimination", 0.01,0.99,0.5,0.1)
                                       )
                                     ),
                                     conditionalPanel(
                                       condition="input.methodechol.length==3",
                                       fluidRow(
                                         plotOutput("choixmethode_chol")),
                                       column(width=4,
                                              HTML("<h4>Les valeurs d'AUC</h4>"),
                                              tableOutput("valAUC_chol"),
                                              HTML("<h4>Les valeurs de precision</h4>"),
                                              tableOutput("matprecision_chol")
                                       ),
                                       column(width=4,
                                              HTML("<h4>Matrice de confusion</h4>"),     
                                              tableOutput("matconf_chol")
                                       )
                                     ))
                    )
               ),
      
      # Choix modele pour Hypertension
      tabPanel("modele hypertension",
               navlistPanel(widths=c(2,10),
                 tabPanel(title = "choix variable",
                          fluidRow(
                            column(
                              width = 1,
                              title = "parametre hyp",
                              dropdownButton(
                                tags$h3("List of Input"),
                                sliderInput("priohyp", "Rang d'importance", 1, 10, value = 1),
                                tooltip = tooltipOptions(title = "Click to see inputs !")
                              )
                            ),
                            column(width = 11,
                                   dataTableOutput(outputId = "tabselvarhyp"))
                          )),
                 tabPanel(title = "modele prediction",
                          fluidRow(
                            column(
                              width=4,
                              pickerInput(
                                "methodehyp",
                                label = "Choisir la methode",
                                choices = colnames(res_hyp[, -1]),
                                multiple = TRUE,
                                selected = colnames(res_hyp[, c(2,6,10)]),
                                options = list(
                                  `actions-box` = FALSE,
                                  `none-selected-text` = "Selectionner 3!",
                                  `max-options` = 3
                                )
                              )
                            ),
                            column(width=4,
                            sliderInput("seuilmod", "Choisir le seuil de discrimination", 0.2,0.8,0.5,0.1)
                            )
                          ),
                          conditionalPanel(
                            condition="input.methodehyp.length==3",
                          fluidRow(
                            plotOutput("choixmethode")),
                          column(width=4,
                            HTML("<h4>Les valeurs d'AUC</h4>"),
                            tableOutput("valAUC"),
                            HTML("<h4>Les valeurs de precision</h4>"),
                            tableOutput("matprecision")
                            ),
                          column(width=4,
                            HTML("<h4>Matrice de confusion</h4>"),     
                            tableOutput("matconf")
                            )
                          ))
               ))
    ), 

#----------------------------------------------------------------  
#tabPanel : PREDICTION
#----------------------------------------------------------------  
    tabPanel(title = "Prediction",
             sidebarLayout(
               sidebarPanel(
                 "Les donnees sur l'individu",
                 selectInput(
                   inputId = "sexe",
                   label = "Selectionner votre sexe",
                   selected = "M",
                   choices = c("M" = "Male", "F" = "Female")
                 ),
                 checkboxGroupInput(
                   inputId = "trouble_sommeil",
                   label = "Trouble de sommeil?",
                   choices = c("Oui" = "Yes", "Non" =
                                 "No"),
                   selected = "No"
                 ),
                 checkboxGroupInput(
                   inputId = "cholesterol",
                   label = "Taux eleve de cholesterol?",
                   choices = c("Oui" = "Yes", "Non" =
                                 "No"),
                   selected = "No"
                 ),
                 checkboxGroupInput(
                   inputId = "surpoids",
                   label = "obÃ¨se?",
                   choices = c("Oui" = "Yes", "Non" =
                                 "No"),
                   selected = "No"
                 ),
                 numericInput(
                   inputId = "bmi",
                   label = "votre indice masse corporel",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 numericInput(
                   inputId = "age",
                   label = "Entrer votre age",
                   value = 18,
                   min = 16,
                   max = 150
                 ),
                 numericInput(
                   inputId = "pression_sys",
                   label = "Entrer votre pression systolique",
                   value = 120,
                   min = 0,
                   max = 300
                 ),
                 numericInput(
                   inputId = "pression_dia",
                   label = "Entrer votre pression diastolique",
                   value = 180,
                   min = 0,
                   max = 300
                 ),
                 numericInput(
                   inputId = "phosphorus",
                   label = "consommation phosphorus en mg",
                   value = 2000,
                   min = 0,
                   max = 3000
                 ),
                 numericInput(
                   inputId = "sodium",
                   label = "consommation sodium en mg",
                   value = 2000,
                   min = 0,
                   max = 3000
                 ),
                 numericInput(
                   inputId = "pobretefamille",
                   label = "pobretefamille",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 checkboxGroupInput(
                   inputId = "travail",
                   label = "travail?",
                   choices = c("oui" = "oui", "non" =
                                 "non"),
                   selected = "non"
                 ),
                 numericInput(
                   inputId = "hauteur",
                   label = "votre hauteur",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 numericInput(
                   inputId = "poids",
                   label = "votre poids",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 checkboxGroupInput(
                   inputId = "risquehypertension",
                   label = "risquehypertension?",
                   choices = c("1" = "Yes", "2" =
                                 "No"),
                   selected = "No"
                 ),
                 checkboxGroupInput(
                   inputId = "risquediabetes",
                   label = "risquediabetes?",
                   choices = c("1" = "Yes", "2" =
                                 "No"),
                   selected = "No"
                 ),
                 # checkboxGroupInput(
                 #   inputId = "dentaire",
                 #   label = "dentaire?",
                 #   choices = c("Oui" = 4, "Non" =
                 #                 3, "Unpeu"=2),
                 #   selected = 2
                 # # ),
                 # numericInput(
                 #   inputId = "dentaire",
                 #   label = "dentaire",
                 #   value = 3,
                 #   min = 2,
                 #   max = 4
                 # ),
                 selectInput(
                   inputId = "dentaire",
                   label = "dentaire",
                   selected = "2",
                   choices = c("1" = "4", "2" = "3", "3" = "2")
                 ),
                 checkboxGroupInput(
                   inputId = "diete",
                   label = "diete?",
                   choices = c("1" = "Yes", "2" =
                                 "No"),
                   selected = "No"
                 ),
                  numericInput(
                   inputId = "dietefibre",
                   label = "dietefibre",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 numericInput(
                   inputId = "alcool",
                   label = "alcool",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 numericInput(
                   inputId = "foodfolate",
                   label = "foodfolate",
                   value = 20,
                   min = 0,
                   max = 50
                 ),
                 numericInput(
                   inputId = "waterdrank",
                   label = "waterdrank",
                   value = 20,
                   min = 0,
                   max = 50
                 )
               ),
               mainPanel(
                 div(HTML("<b>Les resultats de la prediction</b>"), align = "center"),
                 br(),
                 fluidRow(column(4,
                   wellPanel(
                   switchInput(inputId = "predict", value = FALSE)
                 ),
                   wellPanel(
                     sliderTextInput(
                       inputId = "sensibilite", 
                       label = "Comment jugez vous mentalement?", 
                       grid = TRUE, 
                       force_edges = TRUE,
                       choices = c("Fragile","Normale","Fort")
                     ) 
                   )
                 )),
                 conditionalPanel(condition="input.predict==true",
                 fluidRow(
                 column(width=4,
                        div(h4(
                          textOutput(outputId = "resultat_hypertension"), align = "center"
                        )),
                        imageOutput("im_hyp_g",width="10px", height="10px","auto", inline = FALSE),
                        conditionalPanel(condition="output.resultat_hypertension=='Danger!!'",
                        imageOutput("im_hyp_b",width="10px", height="10px","auto", inline = FALSE)
                        )),
                 column(width=4,
                        div(h4(
                          textOutput(outputId = "resultat_cholesterol"), align = "center"
                        )),
                        imageOutput("im_cho_g",width="10px", height="10px","auto", inline = FALSE),
                        conditionalPanel(condition="output.resultat_hypertension=='Danger!!'",
                        imageOutput("im_cho_b",width="10px", height="10px","auto", inline = FALSE)
                        )),
                 column(width=4,
                        div(h4(
                          textOutput(outputId = "resultat_diabetes"), align = "center"
                        )),
                        imageOutput("im_dia_g",width="10px", height="10px","auto", inline = FALSE),
                        conditionalPanel(condition="output.resultat_hypertension=='Danger!!'",
                        imageOutput("im_dia_b",width="10px", height="10px","auto", inline = FALSE)
                         ))
                 )
                 )
               )
             )),

#----------------------------------------------------------------  
#tabPanel : CLASSIFICATION
#----------------------------------------------------------------  
    tabPanel(
      title = "Classification",
      column(width=6,
      div(sliderInput("nb_classe", "Nombre de classe", 3, 8, 1), align="center"),
      plotOutput("inertienutrimentplot"),
      br(),
      plotOutput("acpnutrimentplotkm"),
      hr(),
      dataTableOutput("groupekm")
      ),
      column(width=6,
      fluidRow(width=12,
      div(sliderInput("nivo_cah", "coupure sur l'arbre", 4,8,value=5,step=0.1),align="center"),
      plotOutput("dendrogramme"),
      br(),
      plotOutput("acpnutrimentplotcah"),
      hr(),
      dataTableOutput("groupecah")
      )),
      plotOutput("acpnutrimentplot")
    ),

#----------------------------------------------------------------  
#tabPanel : CONCLUSION
#----------------------------------------------------------------  
    tabPanel(title = "Conclusion")
  )
)
