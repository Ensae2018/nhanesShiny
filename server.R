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
  
  # fonction utilisée pour faire de la matrice confusion
  monerreur <- function(X, Y, seuil=input$seuilmod){
    table(cut(X, breaks = c(0,seuil,1)), Y)
  }
  
  # fonction utilisée pour le calcul de la précision
  precision <- function(X,Y,seuil=input$seuilmod){
    Xc <- cut(X,breaks=c(0,seuil,1),labels=c(0,1))
    round(sum(as.factor(Y)==Xc)/(sum(as.factor(Y)==Xc)+sum(as.factor(Y)!=Xc)),3)
  }
  
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
  observeEvent(input$predict==TRUE, {
    
  output$resultat_hypertension <- renderText({
    
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
  })
  
  output$im_hyp <- renderImage({
    filename <- normalizePath(file.path('./www/img/hypertensiongood.jpg'))
    list(src = filename)},deleteFile = FALSE)
  
  })
  
  output$tableHypertension <- renderTable({
    colnames(donHyp)
  })
  
  output$tableCholesterol <- renderTable({
    colnames(donChol)
  })
  
  output$tableDiabetes <- renderTable({
    colnames(donDiab)
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
  
  output$tabselvarhyp <- renderDataTable({
    datatable(tabselvar_hyp,class = 'cell-border stripe',filter = 'bottom',
              extensions = c('Buttons'), rownames = FALSE,
              options=list(autoWidth = TRUE, 
                           dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                           pageLength=20)
              ) %>% 
      formatStyle(
                columns = 2:length(tabselvar_hyp), 
                backgroundColor = styleEqual(levels = rang_val(), 
                                             values = rep("yellow", length(rang_val())))) 
    })
  
  rang_val <- reactive(tabselvar_hyp[,-1][which(max_val==input$prio, arr.ind=TRUE)])
 
observeEvent(input$methode==3,{
  output$choixmethode <- renderPlot({
    plot(roc(res_hyp[,1],res_hyp[,input$methode[1]]),col="black",main="Courbes ROC")
    lines(roc(res_hyp[,1],res_hyp[,input$methode[2]]), col="red")
    lines(roc(res_hyp[,1],res_hyp[,input$methode[3]]), col="green")
    legend("bottomright",legend = c(input$methode[1],input$methode[2], input$methode[3]), col=c("black","red","green"), lty = 1)
  })

  output$valAUC <- renderTable({
    tabauc <- data.frame(nom1=auc(res_hyp[,1],res_hyp[,input$methode[1]]),
               auc(res_hyp[,1],res_hyp[,input$methode[2]]),
               auc(res_hyp[,1],res_hyp[,input$methode[3]])
    )
    names(tabauc) <- input$methode    
    tabauc
  })
  
  output$matconf <- renderTable({
    tabconf <- cbind(as.data.frame(monerreur(res_hyp[,input$methode[1]],res_hyp[,1])),
          as.data.frame(monerreur(res_hyp[,input$methode[2]],res_hyp[,1]))[,3],
          as.data.frame(monerreur(res_hyp[,input$methode[3]],res_hyp[,1]))[,3]
    )
    names(tabconf)[3:length(names(tabconf))] <- input$methode
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:5] <- tabconf[4,3:5]/(tabconf[4,3:5]+tabconf[3,3:5])
    tabconf[6,3:5] <- tabconf[1,3:5]/(tabconf[1,3:5]+tabconf[2,3:5])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilite"
    tabconf$seuil[6] <- "Specificite"
    tabconf
  })
  
  output$matprecision <- renderTable({
    tabprecision <- data.frame(precision(res_hyp[,input$methode[1]],res_hyp[,1]),
          precision(res_hyp[,input$methode[2]],res_hyp[,1]),
          precision(res_hyp[,input$methode[3]],res_hyp[,1]))
    names(tabprecision) <- input$methode
    tabprecision
  })
})

})
