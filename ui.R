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
    tabPanel(
      title = "Presentation",
      
      # Application title
      titlePanel(strong("Projet Nhanes 2015-2016: construction de modèles diagnostiques pour l'hypertension, le cholestérol et le diabète 
                 à partir de données démographiques, de santé, d'alimentation et d'habitudes de vie")),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(sidebarPanel(
        sliderInput(
          "bins",
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        )
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot")))
    ),
    tabPanel(
      title = "Contexte",
      tableOutput(outputId = "tableHypertension"),
      tableOutput(outputId = "tableCholesterol"),
      tableOutput(outputId = "tableDiabetes")
    ),
    navbarMenu(
      title = "Choix modele",
      tabPanel("modele cholesterol"),
      tabPanel("modele diabete"),
      tabPanel("modele hypertension",
               navlistPanel(widths=c(2,10),
                 tabPanel(title = "choix variable",
                          fluidRow(
                            column(
                              width = 2,
                              title = "parametre hyp",
                              sliderInput("prio", "rangd'importance", 1, 10, value = 1)
                            ),
                            column(width = 10,
                                   dataTableOutput(outputId = "tabselvarhyp"))
                          )),
                 tabPanel(title = "modele prediction",
                          fluidRow(
                            column(
                              width=4,
                              pickerInput(
                                "methode",
                                label = "choisir la methode",
                                choices = colnames(res_hyp[, -1]),
                                multiple = TRUE,
                                selected = colnames(res_hyp[, c(2,6,10)]),
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "effacer tout",
                                  `select-all-text` = "tout selectionner",
                                  `none-selected-text` = "vide",
                                  `max-options` = 3
                                )
                              )
                            )
                          ),
                          conditionalPanel(
                            condition="input.methode.length==3",
                            Sys.sleep(5),
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
                   actionButton("predict", "Prédire")
                 ))),
                 hr(),
                 div(h1(
                   textOutput(outputId = "resultat_hypertension"), align = "center"
                 )),
                 hr(),
                 div(h1(
                   textOutput(outputId = "resultat_cholesterol"), align = "center"
                 )),
                 hr(),
                 div(h1(
                   textOutput(outputId = "resultat_diabetes"), align = "center"
                 ))
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
