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
                 hr(),
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
      ))
      ),
    tabPanel(title = "Conclusion")
  )
)
