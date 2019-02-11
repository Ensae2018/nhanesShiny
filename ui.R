#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                 à partir de données démographiques, de santé, d'alimentation et d'habitudes de vie",align="center",
                    br(),
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
                         h2("Présenté par:",span("Insaf DRIS HAMED, Yifan ZHENG, Jean-Vincent PALLON",style = "color:blue"),align="center"),
                         br(),
                         h3("Le 27 février 2019",align="center"),
                         br(),
                         br(),
                         img(src="img/nhanes.jpg", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;"), 
                         br(),
                         br(),
                         img(src="img/NHANES Mobile Van_Pic 1.png", title="Popup",width="40%",style="display: block; margin-left: auto; margin-right: auto;")),
      
        conditionalPanel(condition="input.idRadio == 2",h1("A compléter")),
        conditionalPanel(condition="input.idRadio == 3",h1("A compléter"))
        
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
      tabPanel("modele cholesterol"),
      tabPanel("modele diabete"),
      tabPanel("modele hypertension",
               navlistPanel(widths=c(2,10),
                 tabPanel(title = "choix variable",
                          fluidRow(
                            column(
                              width = 1,
                              title = "parametre hyp",
                              dropdownButton(
                                tags$h3("List of Input"),
                                sliderInput("prio", "Rang d'importance", 1, 10, value = 1),
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
                                "methode",
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
                            condition="input.methode.length==3",
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
    tabPanel(title = "Prediction",
             sidebarLayout(
               sidebarPanel(
                 "Les données sur l'individu",
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
                   label = "Taux élevé de cholesterol?",
                   choices = c("Oui" = "Yes", "Non" =
                                 "No"),
                   selected = "No"
                 ),
                 checkboxGroupInput(
                   inputId = "surpoids",
                   label = "obèse?",
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
                 )
               ),
               mainPanel(
                 div(HTML("<b>Les résultats de la prédiction</b>"), align = "center"),
                 br(),
                 fluidRow(column(4, wellPanel(
                   switchInput(inputId = "predict", value = FALSE)
                   #actionButton("predict", "Prédire")
                 ))),
                 hr(),
                 conditionalPanel(condition="input.predict==true",
                 div(h4(
                   textOutput(outputId = "resultat_hypertension"), align = "center"
                 )),
                 imageOutput("im_hyp"),  
                 hr(),
                 div(h4(
                   textOutput(outputId = "resultat_cholesterol"), align = "center"
                 )),
                 hr(),
                 div(h4(
                   textOutput(outputId = "resultat_diabetes"), align = "center"
                 ))
                 )
               )
             )),
    tabPanel(
      title = "Classification",
      plotOutput("inertienutrimentplot"),
      br(),
      plotOutput("acpnutrimentplot")
    ),
    tabPanel(title = "Conclusion")
  )
)
