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
                       
               selectInput(inputId = "varhypx", label = "1 variable",
                           choices=colnames(donHyp[,-1]),selected=colnames(donHyp[,-1])[2]),br(),
               
               pickerInput(
                 "varhypxy",
                 label = "2 variables",
                 choices = colnames(donHyp[,-1]),
                 multiple = TRUE,
                 selected = colnames(donHyp[, c(3,28)]),
                 options = list(
                   `actions-box` = FALSE,
                   `max-options` = 2
                 )
               ),
                             radioButtons(inputId = "idgraphtypehyp", label = "Type graphe 2", selected = 3,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
               )),
                       
      column(width = 2,
             tableOutput(outputId="tabstathyp")),
      column(width = 4,
             plotlyOutput(outputId="graph1hyp")),
      column(width = 4,
             plotlyOutput(outputId="graph2hyp"))
    ),
    
    fluidRow(
      column(width = 2,        
             wellPanel(p(span(strong("Cholestérol", style = "color:blue"))),
                       
                       selectInput(inputId = "varchox", label = "1 variable",
                                   choices=colnames(donChol[,-1]),selected=colnames(donChol[,-1])[2]),br(),
                       
                       pickerInput(
                         "varchoxy",
                         label = "2 variables",
                         choices = colnames(donChol[,-1]),
                         multiple = TRUE,
                         selected = colnames(donChol[, c(3,28)]),
                         options = list(
                           `actions-box` = FALSE,
                           `max-options` = 2
                         )
                       ),
                       radioButtons(inputId = "idgraphtypecho", label = "Type graphe 2", selected = 3,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
             )),
      
      column(width = 2,
             tableOutput(outputId="tabstatcho")),
      column(width = 4,
             plotlyOutput(outputId="graph1cho")),
      column(width = 4,
             plotlyOutput(outputId="graph2cho"))
    ),
  
  
  
    
    fluidRow(
      column(width = 2,        
             wellPanel(p(span(strong("Diabète", style = "color:blue"))),
                       
                       selectInput(inputId = "vardiax", label = "1 variable",
                                   choices=colnames(donDia[,-1]),selected=colnames(donDia[,-1])[2]),br(),
                       
                       pickerInput(
                         "vardiaxy",
                         label = "2 variables",
                         choices = colnames(donDia[,-1]),
                         multiple = TRUE,
                         selected = colnames(donDia[, c(3,28)]),
                         options = list(
                           `actions-box` = FALSE,
                           `max-options` = 2
                         )
                       ),
                       radioButtons(inputId = "idgraphtype", label = "Type graphe 2", selected = 1,
                                    choices = c("Nuage" = 1,"Boxplot" = 2,"Heatmap"=3))
                       
             )),
      
      column(width = 2,
             tableOutput(outputId="tabstatdia")),
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
             sidebarPanel("Les données sur l'individu :",
                          
                          #Premiere colonne du questionnaire
                          #Hypothèses : value = mean, faible = 1q, moyen = median, fort = 3q, min=min, max=max
                          fluidRow(column(6,
                                          selectInput(
                                            inputId = "sexe",
                                            label = "Selectionner votre sexe",
                                            selected = "M",
                                            choices = c("M" = "Male", "F" = "Female")
                                          ),
                                          numericInput(
                                            inputId = "age",
                                            label = "Selectionner votre age",
                                            value = 30,
                                            min = 16,
                                            max = 150
                                          ),
                                          checkboxGroupInput(
                                            inputId = "marchevelodixmin",
                                            label = "Faites-vous de la marche ou du vélo?",
                                            choices = c("Oui" = "Yes", "Non" =
                                                          "No"),
                                            selected = "No"
                                          ), 
                                          checkboxGroupInput(
                                            inputId = "travail",
                                            label = "Avez-vous travaillez la semaine dernière?",
                                            choices = c("Oui" = "oui", "Non" =
                                                          "non"),
                                            selected = "non"
                                          ), 
                                          numericInput(
                                            inputId = "piecesmaison",
                                            label = "Entrez le nbr de pièces chez vous",
                                            value = 2,
                                            min = 0,
                                            max = 10
                                          ),
                                          numericInput(
                                            inputId = "pauvretefamille",
                                            label = "Entrez le taux de pauvreté de votre famille",
                                            value = 2,
                                            min = 0,
                                            max = 5
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
                                            label = "Taux élevé de cholestérol?",
                                            choices = c("Oui" = "Yes", "Non" =
                                                          "No"),
                                            selected = "No"
                                          ),
                                          checkboxGroupInput(
                                            inputId = "risquehypertension",
                                            label = "Taux élevé d'hypertension?",
                                            choices = c("Oui" = 1, "Non" =
                                                          2),
                                            selected = 2
                                          ),
                                          checkboxGroupInput(
                                            inputId = "risquediabetes",
                                            label = "Diabète?",
                                            choices = c("Oui" = 1, "Non" =
                                                          2),
                                            selected = 2
                                          ),
                                          checkboxGroupInput(
                                            inputId = "surpoids",
                                            label = "Obèse?",
                                            choices = c("Oui" = "Yes", "Non" =
                                                          "No"),
                                            selected = "No"
                                          ),
                                          numericInput(
                                            inputId = "hauteur",
                                            label = "Entrez votre hauteur(cm)",
                                            value = 160,
                                            min = 80,
                                            max = 200
                                          ),
                                          numericInput(
                                            inputId = "poids",
                                            label = "Entrez votre poids(kg)",
                                            value = 65,#mean=80
                                            min = 30,
                                            max = 200,
                                            step = 1
                                          ),
                                          helpText ("faible~24?, moyen~28?, élevé~33?"),
                                          numericInput(
                                            inputId = "bmi",
                                            label = "Entrez votre indice masse corporel (kg/m^2)",
                                            value = 34,#mean=30
                                            min = 11,
                                            max = 70,
                                            step = 1
                                          ),
                                          helpText ("faible~112, moyen~122, élevé~134"),
                                          numericInput(
                                            inputId = "pression_sys",
                                            label = "Entrez votre pression systolique (mm Hg)",
                                            value = 120,
                                            min = 70,
                                            max = 300,
                                            step = 10
                                          ),
                                          helpText ("faible~62, moyen~68, élevé~76"),
                                          numericInput(
                                            inputId = "pression_dia",
                                            label = "Entrez votre pression diastolique (mm Hg)",
                                            value = 69,
                                            min = 0,
                                            max = 200,
                                            step = 10
                                          )
                          ),
                          
                          #Deuxieme colonne du questionnaire 
                          fluidRow(column(6,
                                          checkboxGroupInput(
                                            inputId = "dentaire",
                                            label = "Quelle recommendation pour votre santé dentaire ?",
                                            choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
                                                        "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1),
                                            selected = 4
                                          ),
                                          checkboxGroupInput(
                                            inputId = "diete",
                                            label = "Prenez-vous un type de diète?",
                                            choices = c("Oui" = 1, "Non" =
                                                          2),
                                            selected = 2
                                          ),
                                          helpText ("faible~10, moyen~15, élevé~22"),
                                          numericInput(
                                            inputId = "fibre",
                                            label = "Entrez votre niveau de fibre alimentaire(g)",
                                            value = 20,
                                            min = 0,
                                            max = 100
                                          ),
                                          helpText ("faible~126, moyen~185, élevé~266"),
                                          numericInput(
                                            inputId = "foodfolate",
                                            label = "Entrez votre niveau de folate alimentaire(g)",
                                            value = 210,
                                            min = 3,
                                            max = 2000,
                                            step = 10
                                          ),
                                          helpText ("faible~390, moyen~900, élevé~1600"),
                                          numericInput(
                                            inputId = "waterdrank",
                                            label = "Entrez l'eau plate consommée hier(g)",
                                            value = 1100,
                                            min = 0,
                                            max = 15000,
                                            step = 200
                                          ),
                                          helpText ("faible~?, moyen~?, élevé~?"),
                                          numericInput(
                                            inputId = "alcool",
                                            label = "Entrez votre niveau d'alcool(g)",
                                            value = 8,
                                            min = 0,
                                            max = 500,
                                            step = 2
                                          ),
                                          helpText ("faible~900, moyen~1200, élevé~1600"),
                                          numericInput(
                                            inputId = "phosphorus",
                                            label = "Entrez votre consommation en phosphorus(mg)",
                                            value = 1300,
                                            min = 50,
                                            max = 8000,
                                            step = 10
                                          ),
                                          helpText ("faible~2300, moyen~3150, élevé~4150"),
                                          numericInput(
                                            inputId = "sodium",
                                            label = "Entrez votre consommation en sodium (mg)",
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
                                          helpText ("faible~1800, moyen~2500, élevé~3400"),
                                          numericInput(
                                            inputId = "humidite",
                                            label = "Consommation en humidité (mg)",
                                            value = 2700,
                                            min = 70,
                                            max = 20000,
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
                                          helpText ("faible~26, moyen~57, élevé~108"),
                                          numericInput(
                                            inputId = "vitamineC",
                                            label = "Consommation en vitamine C (mg)",
                                            value = 79,
                                            min = 0,
                                            max = 1000,
                                            step = 10
                                          ),
                                          helpText ("faible~418, moyen~778, élevé~1462"),
                                          numericInput(
                                            inputId = "LuteineZeaxanthine",
                                            label = "Consommation en Lutéine et Zéaxanthine (mg)",
                                            value = 1550,
                                            min = 0,
                                            max = 80000,
                                            step = 10
                                          )
                          )))),
             mainPanel(
               div(HTML("<b>Les résultats de la prédiction</b>"), align = "center"),
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
                               ),
                               wellPanel(
                                 sliderTextInput(
                                   inputId = "typepatient", 
                                   label = "Selectionnez votre type de patient", 
                                   grid = TRUE, 
                                   force_edges = TRUE,
                                   choices = c("nouveauPatient", "proHyp","proChol","proDiab")
                                 ),
                                 h4("nouveauPatient : veuillez renseigner les données sur l'individu", align = "center"),
                                 h4("proHyp : c'est un individu avec...", align = "center"),
                                 h4("proChol : c'est un individu avec...", align = "center"),
                                 h4("proDiab : c'est un individu avec...", align = "center") 
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
                                         ),
                                         h4("Précision :...", align = "center") #outputId ="precision_hyp"
                                         ),
                                  column(width=4,
                                         div(h4(
                                           textOutput(outputId = "resultat_cholesterol"), align = "center"
                                         )),
                                         imageOutput("im_cho_g",width="10px", height="10px","auto", inline = FALSE),
                                         conditionalPanel(condition="output.resultat_cholesterol=='Danger!!'",
                                                          imageOutput("im_cho_b",width="10px", height="10px","auto", inline = FALSE)
                                         ),
                                         h4("Précision :...", align = "center") #outputId ="precision_chol"
                                         ),
                                  column(width=4,
                                         div(h4(
                                           textOutput(outputId = "resultat_diabetes"), align = "center"
                                         )),
                                         imageOutput("im_dia_g",width="10px", height="10px","auto", inline = FALSE),
                                         conditionalPanel(condition="output.resultat_diabetes=='Danger!!'",
                                                          imageOutput("im_dia_b",width="10px", height="10px","auto", inline = FALSE)
                                         ),
                                         h4("Précision :...", align = "center") #outputId ="precision_dia"
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
))
