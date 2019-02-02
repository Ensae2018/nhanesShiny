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
    tabPanel(title="Presentation",
             
             
             # Application title
             titlePanel("Old Faithful Geyser Data"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             )
    ),
    tabPanel(title="Contexte",
             tableOutput(outputId="tableHypertension")),
    navbarMenu(title="Choix modele",
               tabPanel("modele cholesterol"),
               tabPanel("modele diabete"),
               tabPanel("modele hypertension")
               ),
    tabPanel(title="Prediction",
             sidebarLayout(
             sidebarPanel("Les données sur l'individu",
             selectInput(inputId="sexe", label = "Selectionner votre sexe", 
                         selected = "M", choices = c("M"="Male","F"="Female")),
             checkboxGroupInput(inputId="trouble_sommeil", label= "Trouble de sommeil?",
                                choices=c("Oui"="Yes","Non"="No"),selected = "No"),
             checkboxGroupInput(inputId="cholesterol", label= "Taux élevé de cholesterol?",
                                choices=c("Oui"="Yes","Non"="No"), selected = "No"),
             checkboxGroupInput(inputId="surpoids", label= "obèse?",
                                choices=c("Oui"="Yes","Non"="No"), selected = "No"),
             numericInput(inputId="bmi", label="votre indice masse corporel", value=20, min=0, max=50),
             numericInput(inputId="age", label="Entrer votre age", value=18, min=16, max=150),
             numericInput(inputId="pression_sys", label="Entrer votre pression systolique",
                          value=120, min = 0, max=300),
             numericInput(inputId="pression_dia", label="Entrer votre pression diastolique",
                          value=180, min = 0, max=300),
             numericInput(inputId="phosphorus", label="consommation phosphorus en mg",
                          value=2000, min = 0, max=3000),
             numericInput(inputId="sodium", label="consommation sodium en mg",
                          value=2000, min = 0, max=3000)
             ),
            
             mainPanel("Les résultats de la prédiction",
             textOutput(outputId="resultat_hypertension")   
             )
             ),
             #j'ajoute le button pour lancer la prédiction dans l'onglet PREDICTION
             fluidRow(
               column(4, wellPanel(
                 actionButton("predict", "Prédire")
               )))
             ), 
              
    tabPanel(title="Classification"),
    tabPanel(title="Conclusion")
  )
)

View(don[1:5,])
