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
                         h2("Phase 1: Préparation des données générale"),
                         h3("- Regroupement des tables",br(),"- Pré-sélection métier de 150 variables pour les 3 maladies -élimination des data trop techniques, trop détaillées ou non éthiques",
                            br(),"- Web scrapping des libellés des variables qualitatives",br(),br()," ce qui aboutit à 1 dataset initial commun pour les 3 maladies 8339 individus et 150 variables"),
                         h2("Phase 2: Construction d'un jeu de données spécifique par maladie"),
                         h3("- Recodage de variables",br(),"- Elimination des variables avec plus de 10% de NA",br(),"- Imputation des données manquantes package MICE"),
                         h2("Phase 3: Modélisation"),
                         h3("- Sélection de variables",br(),"- Sélection de modèles"),
                         h2("Phase 4: Construction de l'outil shiny Github pour la visualisation et d'un package pour synthétiser l'ensemble du projet")
        )
        
        ))
    ),
    
    
#----------------------------------------------------------------  
#tabPanel : CONTEXTE
#----------------------------------------------------------------    
    
    
    tabPanel(
      title = "Contexte",
      titlePanel(h1("Exploration des 3 jeux de données",align="center")),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "varhyp", label = p(span("Hypertension", style = "color:blue"),
                      ": choisissez la variable à explorer"),choices=colnames(donHyp)),
          selectInput(inputId = "varcho", label = p(span("Cholestérol", style = "color:blue"),
                      ": choisissez la variable à explorer"),choices=colnames(donChol[,-c(1,21)])),#enlever le SEQN et le Y dans la liste deroulante
          selectInput(inputId = "vardia", label = p(span("Diabète", style = "color:blue"), 
                      ": choisissez la variable à explorer"),choices=colnames(donDia[,-1]))#je mets -1 pour enlever le SEQN dans la liste deroulante
          ),
        
        # Show a plot of the generated distribution
        mainPanel(p(strong("Dataset hypertension")),plotOutput(outputId="graphhyp"),
                  p(strong("Dataset cholestérol")),plotOutput(outputId="graphcho"),
                  p(strong("Dataset diabète")),plotOutput(outputId="graphdia")
      
      )
      
      )),

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
                                           selected = colnames(res_dia[, c(2,6,9)]),
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
             sidebarPanel("Les donnees sur l'individu",
                          
             #Premiere colonne du questionnaire             
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
                   value = 46,
                   min = 16,
                   max = 150
                 ),
                 checkboxGroupInput(
                   inputId = "travail",
                   label = "Avez-vous travaillez la semaine dernière?",
                   choices = c("Oui" = "oui", "Non" =
                                 "non"),
                   selected = "non"
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
                   label = "Taux eleve de cholesterol?",
                   choices = c("Oui" = "Yes", "Non" =
                                 "No"),
                   selected = "No"
                 ),
                 checkboxGroupInput(
                   inputId = "risquehypertension",
                   label = "Taux eleve d'hypertension?",
                   choices = c("Oui" = 1, "Non" =
                                 2),
                   selected = 2
                 ),
                 checkboxGroupInput(
                   inputId = "risquediabetes",
                   label = "Diabètes?",
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
                   value = 80,
                   min = 3,
                   max = 200,
                   step = 5
                 ),numericInput(
                   inputId = "bmi",
                   label = "Entrez votre indice masse corporel (kg/m^2)",
                   value = 30,
                   min = 11,
                   max = 70,
                   step = 5
                 ),
                 numericInput(
                   inputId = "pression_sys",
                   label = "Entrez votre pression systolique (mm Hg)",
                   value = 120,
                   min = 70,
                   max = 300,
                   step = 10
                 ),
                 numericInput(
                   inputId = "pression_dia",
                   label = "Entrez votre pression diastolique (mm Hg)",
                   value = 70,
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
                numericInput(
                   inputId = "fibre",
                   label = "Entrez votre niveau de fibre alimentaire(g)",
                   value = 20,
                   min = 0,
                   max = 100
                 ),
                 numericInput(
                   inputId = "foodfolate",
                   label = "Entrez votre niveau de folate alimentaire(g)",
                   value = 200,
                   min = 0,
                   max = 2000,
                   step = 30
                 ),                 
                 numericInput(
                   inputId = "waterdrank",
                   label = "Entrez l'eau plate consommée hier(g)",
                   value = 1200,
                   min = 0,
                   max = 15000,
                   step = 200
                 ),                
                 numericInput(
                   inputId = "alcool",
                   label = "Entrez votre niveau d'alcool(g)",
                   value = 8,
                   min = 0,
                   max = 500,
                   step = 2
                 ),
                 numericInput(
                   inputId = "phosphorus",
                   label = "Entrez votre consommation en phosphorus(mg)",
                   value = 2000,
                   min = 0,
                   max = 8000,
                   step = 100
                 ),
                 numericInput(
                   inputId = "sodium",
                   label = "Entrez votre consommation en sodium (mg)",
                   value = 2000,
                   min = 0,
                   max = 20000,
                   step = 100
                 )
               )))),
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
                        conditionalPanel(condition="output.resultat_cholesterol=='Danger!!'",
                        imageOutput("im_cho_b",width="10px", height="10px","auto", inline = FALSE)
                        )),
                 column(width=4,
                        div(h4(
                          textOutput(outputId = "resultat_diabetes"), align = "center"
                        )),
                        imageOutput("im_dia_g",width="10px", height="10px","auto", inline = FALSE),
                        conditionalPanel(condition="output.resultat_diabetes=='Danger!!'",
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
      column(
      width=2,
      div(sliderInput("nb_classe", "Nombre de classe Kmeans", 3, 8, value=5), align="center"),
      div(sliderInput("nivo_cah", "Nombre de classe CAH", 3,8,value=5,step=1),align="center"),
      div(sliderTextInput(
        inputId = "choixmaladie", 
        label = "Choisir la maladie:", 
        grid = TRUE, 
        force_edges = TRUE,
        choices = c("cholesterol","diabete","hypertension")
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
               plotOutput("acpnutrimentplotkm"),
               plotOutput("contri1")),
               column(width=6,
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
                   choices = c("Choix des meilleurs modèles" = 1, "Prédiction trimaladie" = 2, "Classification" = 3, "Perspectives" = 4))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition="input.idRadioC == 1",
                       br(),
                       br(),
                       h2("Meilleur modèle pour Diabètes : forêt avec ...",align="center"),
                       br(),
                       h2("Meilleur modèle pour Cholésterol : forêt avec ...",align="center"),
                       br(),
                       br(),
                       h2("Meilleur modèle pour Hypertension : forêt avec ...",align="center"),
                       br(),
                       br()),
      
      conditionalPanel(condition="input.idRadioC == 2",
                       br(),
                       br(),
                       h2("Modèle trimaladie : logistique avec step...",align="center"),
                       br(),
                       br(),
                       h2("Meilleures variables répresentatives : age, ...",align="center"),
                       br(),
                       br()),
      conditionalPanel(condition="input.idRadioC == 3",
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
      conditionalPanel(condition="input.idRadioC == 4",
                       br(),
                       br(),
                       h2("Nutriments versus aliments",align="center"),
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
