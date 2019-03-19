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
  fluidPage(
    # Some custom CSS
    tags$head(
      tags$style(HTML("
        /* Smaller font for preformatted text */
        pre, table.table {
          font-size: smaller;
        }

        body {
          min-height: 2000px;
        }

        .option-group {
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: 0px 5px;
          margin: 5px-10px;
          #background-color: #98f5ff;
          background-color: #b2dfee;
        }

        .option-header {
          #color: #80d;
          color: #8b3a3a;
          text-transform: uppercase;
          margin-bottom: 5px;
        }
      "))
    ),
  navbarPage(
    theme = shinytheme("cerulean"),
    title = "Etude Nhanes",
    
    #----------------------------------------------------------------  
    #tabPanel : PRESENTATION
    #----------------------------------------------------------------    
    
    tabPanel(
      title = "Présentation",
      
      # Application title
      titlePanel(h1("Projet",span(strong("Nhanes 2015-2016", style = "color:blue"))," : recherche de modèles diagnostiques pour l'hypertension, le cholestérol et le diabète 
                 à partir de donnéees démographiques, de santé, d'alimentation et d'habitudes de vie",align="center")),
      hr(),br(),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "idRadio", label = "Sommaire", selected = 1,
                       choices = c("Start" = 1, "Les données" = 2, "La démarche" = 3))
          
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(condition="input.idRadio == 1",
                           h2("Présenté par:",span("Insaf DRIS HAMED, Yifan ZHENG, Jean-Vincent PALLON",style = "color:darkblue"),align="center"),
                           br(),
                           h3("Le 29 mars 2019",align="center"),
                           br(),
                           br(),
                           img(src="img/nhanes.jpg", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;"), 
                           br(),
                           br(),
                           img(src="img/NHANES Mobile Van_Pic 1.png", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;")),
          
          conditionalPanel(condition="input.idRadio == 2",
                           h2(span(strong("Source",style = "color:darkblue"))),
                           h3("Site du ",
                              a(span(strong("Centers for Disease Control and Prevention")),
                                href="https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015",style = "color:blue"),"(www.cdc.gov)"),
                           
                           br(),
                           
                           h2(span(strong("Format initial",style = "color:darkblue"))),
                           h3("83 tables au format XPT avec un identifiant commun"),
                           br(),
                           
                           h2(span(strong("Volumétrie",style = "color:darkblue"))),
                           h3(span(strong("9971 individus")," de tous âges")),
                           h3(span(strong("1893 variables"))),
                           br(),
                           h2(span(strong("Variété",style = "color:darkblue"))),
          
          h3("-",span(strong("démographiques")),": âge, sexe, niveau d'éducation, taux de pauvreté..."),
          h3("-",span(strong("médicales")),":"),
          h4("* caractéristiques générales: poids, taille, IMC, santé dentaire..."),
          h4("* analyses de laboratoire: plomb, insuline, test d'urine..."),
          h4("* maladies déjà connues: hypertension, cholestérol, diabète, problème coronaires, infarctus..."),
          h3("-", span(strong("alimentaires")),": les menus complets de 2 jours non consécutifs d'alimentation"),
          h3("-",span(strong("habitudes de vie")),": travail/chômage, qualité du sommeil, activité physique...")),
        
        
        
        conditionalPanel(condition="input.idRadio == 3",
                         h2(span(strong("Phase 1: Regroupement des tables et construction d'un dataset commun 150 variables"),style="color:darkblue")),
                         h3("- Merge des tables",br(),"- Pré-sélection métier de 150 variables pour les 3 maladies (élimination des data trop techniques, trop détaillées ou non éthiques)",
                            br(),"- Web scrapping des libellés des variables qualitatives"),
                         h3("=>1 dataset 8339 individus / 150 variables",style="color:darkblue"),br(),
                         h2(span(strong("Phase 2: Déclinaison du dataset commun en 3 datasets spécifiques (un par maladie)",style="color:darkblue"))),
                         h3("- Recodage de variables",br(),"- Elimination des variables avec plus de 10% de NA",br(),"- Imputation des données manquantes"),
                         h3("=>3 datasets différents ~ 5000 individus / 80 variables",style="color:darkblue"),br(),
                         h2(span(strong("Phase 3: Modélisations spécifiques par maladie"),style="color:darkblue")),
                         h3("- Sélection de variables",br(),"- Sélection de modèles"),br(),
                         h2(span(strong("Phase 4: Outil Shiny + Package NHANESV2"),style="color:darkblue"))
                      
        )
        
      ))
  ),
  
  
  #----------------------------------------------------------------  
  #tabPanel : CONTEXTE
  #----------------------------------------------------------------    
  

  
tabPanel(
    title = "Contexte",
    
    fluidRow(
      column(width = 2,        
             wellPanel(p(span(strong("Hypertension", style = "color:blue"))),
                       
               selectInput(inputId = "varhypx", label = "Table et Graphique 1",
                           choices=colnames(donHyp[,-1]),selected=colnames(donHyp[,-1])[2]),br(),
               
               pickerInput(
                 "varhypxy",
                 label = "Graphique 2",
                 choices = colnames(donHyp[,-1]),
                 multiple = TRUE,
                 selected = colnames(donHyp[, c(3,28)]),
                 options = list(
                   `actions-box` = FALSE,
                   `max-options` = 2
                 )
               ),
                             radioButtons(inputId = "idgraphtypehyp", label = "Type Graphique 2", selected = 3,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
               )),
                       
      column(width = 2,
             wellPanel(tableOutput(outputId="tabstathyp"))),
      column(width = 4,
             plotlyOutput(outputId="graph1hyp")),
      column(width = 4,
             plotlyOutput(outputId="graph2hyp"))
    ),
    
    fluidRow(
      column(width = 2,        
             wellPanel(p(span(strong("Cholestérol", style = "color:blue"))),
                       
                       selectInput(inputId = "varchox", label = "Table et Graphique 1",
                                   choices=colnames(donChol[,-1]),selected=colnames(donChol[,-1])[2]),br(),
                       
                       pickerInput(
                         "varchoxy",
                         label = "Graphique 2",
                         choices = colnames(donChol[,-1]),
                         multiple = TRUE,
                         selected = colnames(donChol[, c(3,28)]),
                         options = list(
                           `actions-box` = FALSE,
                           `max-options` = 2
                         )
                       ),
                       radioButtons(inputId = "idgraphtypecho", label = "Type Graphique 2", selected = 3,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
             )),
      
      column(width = 2,
             wellPanel(tableOutput(outputId="tabstatcho"))),
      column(width = 4,
             plotlyOutput(outputId="graph1cho")),
      column(width = 4,
             plotlyOutput(outputId="graph2cho"))
    ),
  
  
  
    
    fluidRow(
      column(width = 2,        
             wellPanel(p(span(strong("Diabète", style = "color:blue"))),
                       
                       selectInput(inputId = "vardiax", label = "Table et Graphique 1",
                                   choices=colnames(donDia_transco[,-1]),selected=colnames(donDia_transco[,-1])[2]),br(),
                       
                       pickerInput(
                         "vardiaxy",
                         label = "Graphique 2",
                         choices = colnames(donDia_transco[,-1]),
                         multiple = TRUE,
                         selected = colnames(donDia_transco[, c(3,28)]),
                         options = list(
                           `actions-box` = FALSE,
                           `max-options` = 2
                         )
                       ),
                       radioButtons(inputId = "idgraphtype", label = "Type Graphique 2", selected = 1,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
             )),
      
      column(width = 2,
             wellPanel(tableOutput(outputId="tabstatdia"))),
      column(width = 4,
             plotlyOutput(outputId="graph1dia")),
      column(width = 4,
             plotlyOutput(outputId="graph2dia"))
    )
    ),

#----------------------------------------------------------------  
#tabPanel : CLASSIFICATION
#----------------------------------------------------------------  
tabPanel(
  title = "Classification",
  column(
    width=2,
    div(sliderInput("nb_classe", "Nombre de classe Kmeans", 3, 8, value=5), align="center"),
    div(sliderInput("nivo_cah", "Nombre de classe CAH", 3,8,value=5,step=1),align="center"),
    div(sliderTextInput(
      inputId = "choixmaladie", 
      label = "Choisir la maladie:", 
      grid = TRUE, 
      force_edges = TRUE,
      choices = c("cholesterol","diabetes","hypertension")
    ), align="center"),
    div(sliderTextInput(
      inputId = "afficherind", 
      label = "Afficher les libellés des individus?", 
      grid = TRUE, 
      force_edges = TRUE,
      choices = c("oui","non")), align="center"),
    div(sliderTextInput(
      inputId = "choixaxe", 
      label = "Choisir les axes des dimensions", 
      grid = TRUE, 
      force_edges = TRUE,
      choices = c("dim1-dim2","dim3-dim4")), align="center"),
    div(sliderInput("topcontrib", "nombre de contribution", 3, 15, value=5), align="center")
  ),
  column(
    width=10,
    tabsetPanel(
      tabPanel("Kmeans",
               plotOutput("partitionkm"),
               plotOutput("inertienutrimentplot")),
      tabPanel("Kmeans_ind",
               plotOutput("partitionkmbis"),
               plotOutput("inertienutrimentplotbis")),
      tabPanel("CAH",
               plotOutput("dendrogramme"),
               plotOutput("indicehierarchieplot")),
      tabPanel("Comparaison",
               column(width=6,
                      "Kmeans",
                      plotOutput("acpnutrimentplotkm"),
                      plotOutput("contri1")),
               column(width=6,
                      "CAH",
                      plotOutput("acpnutrimentplotcah"),
                      plotOutput("contri2"))),
      tabPanel("Tableau",
               column(width=6,
                      dataTableOutput("groupecah")),
               column(width=6,
                      dataTableOutput("groupekm"))),
      tabPanel("Croisement",
               fluidRow(
                 plotOutput("acpplotind"),
                 plotOutput("acpplotdual")
               ))))
),

  #----------------------------------------------------------------  
  #navbarMenu : CHOIX MODELE
  #----------------------------------------------------------------    
  
  navbarMenu(
    title = "Choix modèle",
    
    # Choix modele pour Diabete
    tabPanel("modèle diabète",
             navlistPanel(widths=c(2,10),
                          "Modèle Diabète",
                          tabPanel(title = "choix variable",
                                   fluidRow(
                                     column(
                                       width = 1,
                                       title = "parametre diab",
                                       dropdownButton(
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
                                         "methodedia",
                                         label = "Choisir la methode",
                                         choices = colnames(res_dia[, -1]),
                                         multiple = TRUE,
                                         selected = colnames(res_dia[, c(2,6,10)]),
                                         options = list(
                                           `actions-box` = FALSE,
                                           `none-selected-text` = "Selectionner 3!",
                                           `max-options` = 3
                                         )
                                       )
                                     ),
                                     column(width=4,
                                            sliderInput("seuilmoddia", "Choisir le seuil de discrimination", 0.2,0.8,0.5,0.1)
                                     )
                                   ),
                                   conditionalPanel(
                                     condition="input.methodedia.length==3",
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
    tabPanel("modèle cholestérol",
             navlistPanel(widths=c(2,10),
                          "Modèle Cholestérol",
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
                                           `none-selected-text` = "Selectionner"
                                         )
                                       )
                                     ),
                                     column(width=4,
                                            sliderInput("seuilmodchol", "Choisir le seuil de discrimination", 0.01,0.99,0.5,0.1)
                                     )
                                   ),
                                     fluidRow(
                                       plotOutput("choixmethode_chol")),
                                     column(width=6,
                                            HTML("<h4>Les valeurs d'AUC</h4>"),
                                            tableOutput("valAUC_chol"),
                                            HTML("<h4>Les valeurs de precision</h4>"),
                                            tableOutput("matprecision_chol")
                                     ),
                                     column(width=6,
                                            HTML("<h4>Matrice de confusion</h4>"),     
                                            tableOutput("matconf_chol")
                                     )
                                   )
             )
    ),
    
    # Choix modele pour Hypertension
    tabPanel("modèle hypertension",
             navlistPanel(widths=c(2,10),
                          "Modèle Hypertension",
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
                                           `none-selected-text` = "Selectionner"
                                         )
                                       )
                                     ),
                                     column(width=4,
                                            sliderInput("seuilmod", "Choisir le seuil de discrimination", 0.2,0.8,0.5,0.1)
                                     )
                                   ),
                                     fluidRow(
                                       plotOutput("choixmethode")),
                                     column(width=6,
                                            HTML("<h4>Les valeurs d'AUC</h4>"),
                                            tableOutput("valAUC"),
                                            HTML("<h4>Les valeurs de precision</h4>"),
                                            tableOutput("matprecision")
                                     ),
                                     column(width=6,
                                            HTML("<h4>Matrice de confusion</h4>"),     
                                            tableOutput("matconf")
                                     )
                                   )
             ))
  ), 
  
  #----------------------------------------------------------------  
  #tabPanel : PREDICTION
  #----------------------------------------------------------------  
  
  tabPanel(title = "Prédiction",
           
           sidebarLayout(
             sidebarPanel("Les données sur l'individu :",align = "center",
                          br(),
                          br(),
           
                          #Premiere colonne du questionnaire
                          #Hypothèses : value = mean, faible = 1q, moyen = median, fort = 3q, min=min, max=max
                          fluidRow(
                            
                            column(width=4,
                                          
                                    div(class = "option-group",
                                      div(class = "option-header", "Habitudes"),
                                          selectInput(
                                            inputId = "sexe",
                                            label = "Votre sexe",
                                            selected = "Female",
                                            choices = c("M" = "Male", "F" = "Female")
                                          ),
                                          numericInput(
                                            inputId = "age",
                                            label = "Votre age",
                                            value = 30,
                                            min = 16,
                                            max = 150
                                          ),
                                      #finalement pas besoin
                                      radioButtons(inputId = "marchevelodixmin", label = "Faites-vous de la marche ou du vélo?", selected = "Yes",
                                                   choices = c("Oui" = "Yes", "Non" = "No")),    
                                      # checkboxGroupInput(
                                      #       inputId = "marchevelodixmin",
                                      #       label = "Faites-vous de la marche ou du vélo?",
                                      #       choices = c("Oui" = "Yes", "Non" =
                                      #                     "No"),
                                      #       selected = "No"
                                      #     ), 
                                      radioButtons(inputId = "travail", label = "Avez-vous travaillez la semaine dernière?", selected = "oui",
                                                   choices = c("Oui" = "oui", "Non" = "non")),   
                                          # checkboxGroupInput(
                                          #   inputId = "travail",
                                          #   label = "Avez-vous travaillez la semaine dernière?",
                                          #   choices = c("Oui" = "oui", "Non" =
                                          #                 "non"),
                                          #   selected = "non"
                                          # ), 
                                      #finalement pas besoin
                                          numericInput(
                                            inputId = "piecesmaison",
                                            label = "Le nbr de pièces chez vous",
                                            value = 2,
                                            min = 0,
                                            max = 10
                                          ),
                                          numericInput(
                                            inputId = "pauvretefamille",
                                            label = "Le taux de pauvreté de votre famille",
                                            value = 2,
                                            min = 0,
                                            max = 5
                                          )
                                    )),
                                    
                              
                            column(width=4,
                                   
                                    div(class = "option-group",
                                        div(class = "option-header", "Santé"),
                                        radioButtons(inputId = "trouble_sommeil", label = "Trouble de sommeil?", selected = "Yes",
                                                     choices = c("Oui" = "Yes", "Non" = "No")),     
                                        # checkboxGroupInput(
                                        #     inputId = "trouble_sommeil",
                                        #     label = "Trouble de sommeil?",
                                        #     choices = c("Oui" = "Yes", "Non" =
                                        #                   "No"),
                                        #     selected = "No"
                                        #   ),
                                        radioButtons(inputId = "cholesterol", label = "Taux élevé de cholestérol?", selected = "Yes",
                                                     choices = c("Oui" = "Yes", "Non" ="No")),    
                                          # checkboxGroupInput(
                                          #   inputId = "cholesterol",
                                          #   label = "Taux élevé de cholestérol?",
                                          #   choices = c("Oui" = "Yes", "Non" =
                                          #                 "No"),
                                          #   selected = "No"
                                          # ),
                                        radioButtons(inputId = "risquehypertension", label = "Taux élevé d'hypertension?", 
                                                     choices = c("Oui" = 1, "Non" = 2),
                                                     selected = 1),
                                          # checkboxGroupInput(
                                          #   inputId = "risquehypertension",
                                          #   label = "Taux élevé d'hypertension?",
                                          #   choices = c("Oui" = 1, "Non" =
                                          #                 2),
                                          #   selected = 2
                                          # ),
                                        radioButtons(inputId = "risquediabete", label = "Diabète?", 
                                                     choices = c("Oui" = 1, "Non" =
                                                                   2),
                                                     selected = 1),  
                                        # checkboxGroupInput(
                                        #     inputId = "risquediabetes",
                                        #     label = "Diabète?",
                                        #     choices = c("Oui" = 1, "Non" =
                                        #                   2),
                                        #     selected = 2
                                        #   ),
                                        radioButtons(inputId = "surpoids",label = "Obèse?", 
                                                     choices = c("Oui" = "Yes", "Non" = "No"), 
                                                     selected = "Yes"),    
                                        # checkboxGroupInput(
                                        #     inputId = "surpoids",
                                        #     label = "Obèse?",
                                        #     choices = c("Oui" = "Yes", "Non" =
                                        #                   "No"),
                                        #     selected = "No"
                                        #   ),
                                          numericInput(
                                            inputId = "hauteur",
                                            label = "Votre hauteur(cm)",
                                            value = 160,
                                            min = 80,
                                            max = 200
                                          ),
                                          numericInput(
                                            inputId = "poids",
                                            label = "Votre poids(kg)",
                                            value = 65,#mean=80
                                            min = 30,
                                            max = 200,
                                            step = 1
                                          ),
                                          helpText ("faible~24?, moyen~28?, élevé~33?"),
                                          numericInput(
                                            inputId = "bmi",
                                            label = "Votre indice masse corporel (kg/m^2)",
                                            value = 34,#mean=30
                                            min = 11,
                                            max = 70,
                                            step = 1
                                          ),
                                          helpText ("faible~112, moyen~122, élevé~134"),
                                          numericInput(
                                            inputId = "pression_sys",
                                            label = "Votre pression systolique (mm Hg)",
                                            value = 120,
                                            min = 70,
                                            max = 300,
                                            step = 10
                                          ),
                                          helpText ("faible~62, moyen~68, élevé~76"),
                                          numericInput(
                                            inputId = "pression_dia",
                                            label = "Votre pression diastolique (mm Hg)",
                                            value = 69,
                                            min = 0,
                                            max = 200,
                                            step = 10
                                          )
                                    )),
                           
                        
                            column(width=4,         
                                        div(class = "option-group",
                                              div(class = "option-header", "Alimentation"),
                                            radioButtons(inputId = "dentaire",label = "Quelle recommendation pour votre santé dentaire ?", 
                                                         choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
                                                                     "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1), 
                                                         selected = 1), 
                                          #   checkboxGroupInput(
                                          #   inputId = "dentaire",
                                          #   label = "Quelle recommendation pour votre santé dentaire ?",
                                          #   choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
                                          #               "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1),
                                          #   selected = 4
                                          # ),
                                          radioButtons(inputId = "diete",label = "Faites-vous un régime?", 
                                                       choices = c("Oui" = 1, "Non" = 2),
                                                       selected = 1), 
                                          # checkboxGroupInput(
                                          #   inputId = "diete",
                                          #   label = "Prenez-vous un type de diète?",
                                          #   choices = c("Oui" = 1, "Non" =
                                          #                 2),
                                          #   selected = 2
                                          # ),
                                          helpText ("faible~10, moyen~15, élevé~22"),
                                          numericInput(
                                            inputId = "fibre",
                                            label = "Consommation en fibre alimentaire(g)",
                                            value = 20,
                                            min = 0,
                                            max = 100
                                          ),
                                          #finalement pas besoin
                                          helpText ("faible~126, moyen~185, élevé~266"),
                                          numericInput(
                                            inputId = "foodfolate",
                                            label = "Consommation en folate alimentaire(g)",
                                            value = 210,
                                            min = 3,
                                            max = 2000,
                                            step = 10
                                          ),
                                          helpText ("faible~390, moyen~900, élevé~1600"),
                                          numericInput(
                                            inputId = "waterdrank",
                                            label = "Consommation en eau plate hier (g)",
                                            value = 1100,
                                            min = 0,
                                            max = 15000,
                                            step = 200
                                          ),
                                          helpText ("faible~1800, moyen~2500, élevé~3400"),
                                          numericInput(
                                            inputId = "humidite",
                                            label = "Consommation en eau contenue dans les aliments (mg)",
                                            value = 2700,
                                            min = 70,
                                            max = 20000,
                                            step = 10
                                          ),
                                          helpText ("faible~?, moyen~?, élevé~?"),
                                          numericInput(
                                            inputId = "alcool",
                                            label = "Consommation d'alcool(g)",
                                            value = 8,
                                            min = 0,
                                            max = 500,
                                            step = 2
                                          ),
                                          helpText ("faible~900, moyen~1200, élevé~1600"),
                                          numericInput(
                                            inputId = "phosphorus",
                                            label = "Consommation en phosphore (mg)",
                                            value = 1300,
                                            min = 50,
                                            max = 8000,
                                            step = 10
                                          ),
                                          helpText ("faible~2300, moyen~3150, élevé~4150"),
                                          numericInput(
                                            inputId = "sodium",
                                            label = "Consommation en sodium (mg)",
                                            value = 3350,
                                            min = 80,
                                            max = 20000,
                                            step = 10
                                          ),
                                          helpText ("faible~57, moyen~88, élevé~128"),
                                          numericInput(
                                            inputId = "sucre",
                                            label = "Consommation en sucre (mg)",
                                            value = 100,
                                            min = 0,
                                            max = 1000,
                                            step = 10
                                          ),
                                          
                                          helpText ("faible~155, moyen~250, élevé~390"),
                                          numericInput(
                                            inputId = "choles",
                                            label = "Consommation en cholestérol (mg)",
                                            value = 290,
                                            min = 0,
                                            max = 2000,
                                            step = 10
                                          ),
                                          helpText ("faible~55, moyen~73, élevé~97"),
                                          numericInput(
                                            inputId = "proteines",
                                            label = "Consommation en protéines (mg)",
                                            value = 79,
                                            min = 0,
                                            max = 500,
                                            step = 10
                                          ),
                                          helpText ("faible~8, moyen~12, élevé~16"),
                                          numericInput(
                                            inputId = "fer",
                                            label = "Consommation en fer (mg)",
                                            value = 13,
                                            min = 0,
                                            max = 100,
                                            step = 10
                                          ),
                                          helpText ("faible~19, moyen~81, élevé~177"),
                                          numericInput(
                                            inputId = "cafeine",
                                            label = "Consommation en caféine (mg)",
                                            value = 128,
                                            min = 0,
                                            max = 5000,
                                            step = 10
                                          ),
                                          #finalement pas besoin
                                          helpText ("faible~26, moyen~57, élevé~108"),
                                          numericInput(
                                            inputId = "vitamineC",
                                            label = "Consommation en vitamine C (mg)",
                                            value = 79,
                                            min = 0,
                                            max = 1000,
                                            step = 10
                                          ),
                                          #finalement pas besoin
                                          helpText ("faible~418, moyen~778, élevé~1462"),
                                          numericInput(
                                            inputId = "LuteineZeaxanthine",
                                            label = "Consommation en Lutéine et Zéaxanthine (mg)",
                                            value = 1550,
                                            min = 0,
                                            max = 80000,
                                            step = 10
                                          ),
                                          helpText ("faible~26, moyen~57, élevé~108"),
                                          numericInput(
                                            inputId = "vitB6",
                                            label = "Consommation en vitamine B6 (mg)",
                                            value = 79,
                                            min = 0,
                                            max = 1000,
                                            step = 10
                                          ),
                                          helpText ("faible~26, moyen~57, élevé~108"),
                                          numericInput(
                                            inputId = "vitB12",
                                            label = "Consommation en vitamine B12 (mg)",
                                            value = 79,
                                            min = 0,
                                            max = 1000,
                                            step = 10
                                          )))
                          )),
             mainPanel(
               div(HTML("<b>Les résultats de la prédiction</b>"), align = "center"),
               br(),
               fluidRow(
                 column(4,
                               div(class = "option-group",
                                   div(class = "option-header"),
                               wellPanel(
                              radioButtons("typepatient", "Choisir votre type de patient",
                                           c("Nouvel individu","Individu avec hypertension","Individu avec cholestérol","Individu avec diabète")))
                              )
               ),
                 column(4,
                               div(class = "option-group",
                                   div(class = "option-header"),
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
                               ))
               
                 )),
               
               conditionalPanel(condition="input.predict==true",
                                fluidRow(
                                  column(width=4,
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         
                                         imageOutput("im_hyp_g",width="10px", height="10px","auto", inline = FALSE),
                                         conditionalPanel(condition="output.resultat_hypertension=='Danger!!'", # <seuilhyp
                                                          imageOutput("im_hyp_b",width="10px", height="10px","auto", inline = FALSE)
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         h4("La probabilité d'avoir de l'hypertension est de ",align="center"),
                                         div(h4(
                                           textOutput(outputId = "resultat_hypertension"), align = "center"
                                         ))),
                                  
                                  column(width=4,
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                        
                                         imageOutput("im_cho_g",width="10px", height="10px","auto", inline = FALSE),
                                         conditionalPanel(condition="output.resultat_cholesterol=='Danger!!'", # <seuilchol
                                                          imageOutput("im_cho_b",width="10px", height="10px","auto", inline = FALSE)
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         h4("La probabilité d'avoir du cholestérol est de ",align="center"),
                                         div(h4(
                                           textOutput(outputId = "resultat_cholesterol"), align = "center"
                                         ))),
                                         
                                  column(width=4,
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                        
                                         imageOutput("im_dia_g",width="10px", height="10px","auto", inline = FALSE),
                                         conditionalPanel(condition="output.resultat_diabetes=='Danger!!'", # <seuildiab
                                                          imageOutput("im_dia_b",width="10px", height="10px","auto", inline = FALSE)
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         h4("La probabilité d'avoir du diabète est de ",align="center"),
                                         div(h4(
                                           textOutput(outputId = "resultat_diabetes"), align = "center"
                                         ))
                                  )
                                )
               )
             )
           )),
  
  #----------------------------------------------------------------  
  #tabPanel : CONCLUSION
  #----------------------------------------------------------------  
  
  tabPanel(
    title = "Conclusion",
    
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
        radioButtons(inputId = "idRadioC", label = "Plan", selected = 3,
                     choices = c("Contexte des données" = 1,"Classification" = 2,"Choix des meilleures variables" = 3, "Choix des meilleurs modèles" = 4, "Prédiction trimaladie" = 5,  "Nouvelles perspectives" = 6))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        conditionalPanel(condition="input.idRadioC == 1",
                         br(),
                         br(),
                         h2("Données Diabète : ...",align="center"),
                         br(),
                         h2("Données Cholestérol : ...",align="center"),
                         br(),
                         br(),
                         h2("Données Hypertension : ...",align="center"),
                         br(),
                         br()),
        conditionalPanel(condition="input.idRadioC == 2",
                         br(),
                         br(),
                         h2("Kmeans : ",align="center"),
                         br(),
                         br(),
                         h2("CAH : ",align="center"),
                         br(),
                         br(),
                         h2("Croisement : ",align="center"),
                         br(),
                         br()),
        conditionalPanel(condition="input.idRadioC == 3",
                         br(),
                         br(),
                         h2("Meilleures variables pour Diabète : ...",align="center"),
                         br(),
                         h2("Meilleures variables pour Cholestérol : ...",align="center"),
                         br(),
                         br(),
                         h2("Meilleures variables pour Hypertension : ...",align="center"),
                         br(),
                         br()),
        conditionalPanel(condition="input.idRadioC == 4",
                         br(),
                         br(),
                         h2("Meilleur modèle pour Diabète : forêt avec ...",align="center"),
                         br(),
                         h2("Meilleur modèle pour Cholestérol : forêt avec ...",align="center"),
                         br(),
                         br(),
                         h2("Meilleur modèle pour Hypertension : forêt avec ...",align="center"),
                         br(),
                         br()),
        
        conditionalPanel(condition="input.idRadioC == 5",
                         br(),
                         br(),
                         h2("Modèle trimaladie : logistique avec step...",align="center"),
                         br(),
                         br(),
                         h2("Meilleures variables répresentatives : age, ...",align="center"),
                         br(),
                         br()),
        conditionalPanel(condition="input.idRadioC == 6",
                         br(),
                         br(),
                         h2("Nutriments versus aliments",align="center"),
                         br(),
                         br(),
                         br(),
                         h2("Données d'autres années (2013, 2011, ...)",align="center"),
                         br(),
                         br(),
                         h2("Modèle français",align="center"),
                         br(),
                         br(),
                         h2("Modèle international",align="center"),
                         br(),
                         br())
      )))
)))
