#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- don[, 50] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$resultat_hypertension <- renderText({
  tempo <- predict(mod,data.frame(Age_in_years_at_screening=input$age,
             Systolic_Blood_pres_2nd_rdg_mm_Hg=input$pression_sys,
             high_cholesterol_level=input$cholesterol,
             Body_Mass_Index_kg_m_2=input$bmi,
             Doctor_ever_said_you_were_overweight=input$surpoids,
             Ever_told_doctor_had_trouble_sleeping=input$trouble_sommeil,
             Phosphorus_mg=input$phosphorus,
             Diastolic_Blood_pres_1st_rdg_mm_Hg=input$pression_dia,
             Sodium_mg=input$sodium
             ),type="response")
  ifelse(tempo>0.5,"vous avez de l'hypertension", "vous n'avez pas de l'hypertension")
  })
  
  output$tableHypertension <- renderTable({
    colnames(don)
  })
  
})
