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
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
    
  })
   
  
  tempoHyp <- 999
  tempoChol <- 999
  tempoDiab <- 999
  
  # j'ajoute une observation  sur la button pour lancer la prédiction
  observeEvent(input$predict, {
    
  output$resultat_hypertension <- renderText({
<<<<<<< HEAD
    input$predire
    isolate({
      tempo <- predict(
        mod,
        data.frame(
          Age_in_years_at_screening = input$age,
          Systolic_Blood_pres_2nd_rdg_mm_Hg =
            input$pression_sys,
          high_cholesterol_level = input$cholesterol,
          Body_Mass_Index_kg_m_2 = input$bmi,
          Doctor_ever_said_you_were_overweight =
            input$surpoids,
          Ever_told_doctor_had_trouble_sleeping =
            input$trouble_sommeil,
          Phosphorus_mg = input$phosphorus,
          Diastolic_Blood_pres_1st_rdg_mm_Hg =
            input$pression_dia,
          Sodium_mg = input$sodium
        ),
        type = "response"
      )
      ifelse(tempo > 0.5,
             "vous avez de l'hypertension",
             "vous n'avez pas de l'hypertension")
    })
=======
 
  tempoHyp <- predict(modHyp,data.frame(Age_in_years_at_screening=input$age,
             Systolic_Blood_pres_2nd_rdg_mm_Hg=input$pression_sys,
             high_cholesterol_level=input$cholesterol,
             Body_Mass_Index_kg_m_2=input$bmi,
             Doctor_ever_said_you_were_overweight=input$surpoids,
             Ever_told_doctor_had_trouble_sleeping=input$trouble_sommeil,
             Phosphorus_mg=input$phosphorus,
             Diastolic_Blood_pres_1st_rdg_mm_Hg=input$pression_dia,
             Sodium_mg=input$sodium
             ),type="response")
  ifelse(tempoHyp>0.5,"vous avez de l'hypertension", "vous n'avez pas de l'hypertension")
  #test
  #ifelse(tempo>0.5,"plus de 0.5", "moins de 0.5")
>>>>>>> 36a3c249128b886756494057779fab3bbc280266
  })
  
  output$tableHypertension <- renderTable({
    colnames(donHyp)
  })
  
  
  output$resultat_cholesterol <- renderText({
    
    tempoChol <- predict(modChol,data.frame(Age_in_years_at_screening=input$age,
                                          Systolic_Blood_pres_2nd_rdg_mm_Hg=input$pression_sys,
                                          high_cholesterol_level=input$cholesterol,
                                          Body_Mass_Index_kg_m_2=input$bmi,
                                          Doctor_ever_said_you_were_overweight=input$surpoids,
                                          Ever_told_doctor_had_trouble_sleeping=input$trouble_sommeil,
                                          Phosphorus_mg=input$phosphorus,
                                          Diastolic_Blood_pres_1st_rdg_mm_Hg=input$pression_dia,
                                          Sodium_mg=input$sodium
    ),type="response")
    ifelse(tempoChol>0.5,"vous avez du cholesterol", "vous n'avez pas du cholesterol")
    #test
    #ifelse(tempo>0.5,"plus de 0.5", "moins de 0.5")
  })
  
  output$tableCholesterol <- renderTable({
    colnames(donChol)
  })
  
  output$resultat_diabetes <- renderText({
    
    tempoDiab <- predict(modDiab,data.frame(Age_in_years_at_screening=input$age,
                                          Systolic_Blood_pres_2nd_rdg_mm_Hg=input$pression_sys,
                                          high_cholesterol_level=input$cholesterol,
                                          Body_Mass_Index_kg_m_2=input$bmi,
                                          Doctor_ever_said_you_were_overweight=input$surpoids,
                                          Ever_told_doctor_had_trouble_sleeping=input$trouble_sommeil,
                                          Phosphorus_mg=input$phosphorus,
                                          Diastolic_Blood_pres_1st_rdg_mm_Hg=input$pression_dia,
                                          Sodium_mg=input$sodium
    ),type="response")
    ifelse(tempoDiab>0.5,"vous avez de la diabetes", "vous n'avez pas de la diabetes")
    #test
    #ifelse(tempo>0.5,"plus de 0.5", "moins de 0.5")
  })
  
  output$tableDiabetes <- renderTable({
    colnames(donDiab)
  })
  
  })
  
  output$resultat_hypertension <- renderText({
    ifelse(tempoHyp==999,"simulation pas lancée")
  })
  
  output$resultat_cholesterol <- renderText({
    ifelse(tempoChol==999,"simulation pas lancée")
  })
  
  output$resultat_diabetes <- renderText({
    ifelse(tempoDiab==999,"simulation pas lancée")
  })
  
  output$inertienutrimentplot <- renderPlot({
    ggplot(as.data.frame(partition),
           aes(x = seq(1, length(partition)),
               y = partition)) +
      geom_point() + ggtitle("Evolution inertie/nb de classe") +
      xlab("nombre de classe") + ylab("I inter/I totale")
  })
  
  output$acpnutrimentplot <- renderPlot({
    fviz_pca_var(
      acp,
      col.var = grp,
      palette = c("black", "Blue", "red", "orange"),
      repel = TRUE
    )
  })
  
})
