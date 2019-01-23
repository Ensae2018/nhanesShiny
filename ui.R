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
    theme = shinytheme("united"),
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
             selectInput(inputId="sexe", label = "Selectionner votre sexe", 
                         selected = "M", choices = c("M","F")),
             checkboxInput(inputId="Regime", label= "Regime suivi"),
             numericInput(inputId="age", label="Entrer votre age", value=0, min=0, max=150)
             ),
    tabPanel(title="Classification"),
    tabPanel(title="Conclusion")
  )
)

