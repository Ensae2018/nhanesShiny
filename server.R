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
  
  seuil <- reactive({
    switch (input$sensibilite,
      Fragile = 0.4,
      Normal = 0.5,
      fort=0.6
    )
  })
  
  km <- reactive({
    km <- kmeans(var$coord, center=input$nb_classe, nstart = 25)
  })
  
  kmbis <- reactive({
    kmbis <- kmeans(ind$coord, center=input$nb_classe, nstart = 25)
  })
  
  cah <- reactive({
    #hcut(dist(scale(var$coord)),method = "ward.D2")
    hcut(dist(scale(var$coord)),k=input$nivo_cah, method = "ward.D2")
  })
  
  # fonction utilisée pour faire de la matrice confusion
  monerreur <- function(X, Y, seuil=input$seuilmod){
    table(cut(X, breaks = c(0,seuil,1)), Y)
  }
  
  # fonction utilisée pour le calcul de la précision
  precision <- function(X,Y,seuil=input$seuilmod){
    Xc <- cut(X,breaks=c(0,seuil,1),labels=c(0,1))
    round(sum(as.factor(Y)==Xc)/(sum(as.factor(Y)==Xc)+sum(as.factor(Y)!=Xc)),3)
  }
  
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- don[, 50]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x,
  #        breaks = bins,
  #        col = 'darkgray',
  #        border = 'white')
  #   
  # })
  

####
# La Prediction
####  
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
  ifelse(tempoHyp>seuil(),"Danger!!", ";-)")
  })
  
  output$resultat_cholesterol <- renderText({
    
    tempoChol <- predict(modChol,data.frame(RIDAGEYR_demo=input$age,
                                            RIAGENDR_demo=ifelse(input$sexe=="Male", 1, 0),
                                            BPXSY3_bpx=input$pression_sys,
                                            BMXBMI_bmx=input$bmi,
                                            MCQ080_mcq=ifelse(input$surpoids=="Yes",1,0),
                                            SLQ050_slq=ifelse(input$trouble_sommeil=="Yes",1,0),
                                            BPXDI2_bpx=input$pression_dia,
                                            INDFMPIR_demo=input$pauvretefamille,
                                            Var_TRAVAIL=input$travail,
                                            BMXHT_bmx=input$hauteur,
                                            BMXWT_bmx=input$poids,
                                            BPQ020_bpq=as.numeric(input$risquehypertension),
                                            DIQ010_diq=as.numeric(input$risquediabetes),
                                            OHAREC_ohxref=as.numeric(input$dentaire),
                                            DRQSDIET_dr1tot=as.numeric(input$diete),
                                            DR1TFIBE_dr1tot=input$fibre,
                                            DR1TALCO_dr1tot=input$alcool,
                                            DR1TFF_dr1tot=input$foodfolate,
                                            DR1.320Z_dr1tot=input$waterdrank
    ),type="response")
    ifelse(tempoChol>seuil(),"Danger!!", ";-)")
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
    ifelse(tempoDiab>seuil(),"Danger!!", ";-)")
  })
  
  output$im_hyp_g <- renderImage({
    filename <- normalizePath(file.path('./www/img/hypertensiongood.jpg'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  
  output$im_hyp_b <- renderImage({
    filename <- normalizePath(file.path('./www/img/hypertensionbad.jpg'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  
  output$im_cho_g <- renderImage({
    filename <- normalizePath(file.path('./www/img/Cholesterolgood.jpg'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  
  output$im_cho_b <- renderImage({
    filename <- normalizePath(file.path('./www/img/Cholesterolbad.png'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  
  output$im_dia_g <- renderImage({
    filename <- normalizePath(file.path('./www/img/diabetegood.jpg'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  
  output$im_dia_b <- renderImage({
    filename <- normalizePath(file.path('./www/img/diabetebad.png'))
    list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
  })
  
  output$tableHypertension <- renderTable({
    colnames(donHyp)
  })
  
  output$tableCholesterol <- renderTable({
    colnames(donChol)
  })
  
  output$tableDiabetes <- renderTable({
    colnames(donDia)
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
  
  
####
# La Classification
####  
  
  output$inertienutrimentplot <- renderPlot({
    ggplot(as.data.frame(partition),
           aes(x = seq(1, length(partition)),
               y = partition)) +
      geom_bar(stat = "identity") + ggtitle("Evolution inertie/nb de classe") +
      xlab("nombre de classe") + ylab("I inter/I totale")+
      geom_vline(xintercept=input$nb_classe, linetype="dashed", color = "red",size=2)
  })
  
  output$inertienutrimentplotbis <- renderPlot({
    ggplot(as.data.frame(partitionbis),
           aes(x = seq(1, length(partitionbis)),
               y = partitionbis)) +
      geom_bar(stat = "identity") + ggtitle("Evolution inertie/nb de classe") +
      xlab("nombre de classe") + ylab("I inter/I totale")+
      geom_vline(xintercept=input$nb_classe, linetype="dashed", color = "red", size=2)
  })
  
  output$indicehierarchieplot <- renderPlot({
    ggplot(as.data.frame(sort(cah()$height,dec=T)),
           aes(x = seq(1,length(cah()$height)),y=sort(cah()$height,dec=T)))+
      geom_point() + ggtitle("hauteur CAH") +
      xlab("index") + ylab("hauteur")+
      geom_vline(xintercept=input$nivo_cah, linetype="dashed", color = "red",size=2)
  })
  
  output$acpnutrimentplotkm <- renderPlot({
    grp <- as.factor(km()$cluster)
    fviz_pca_var(
      acp,
      col.var = grp,
      palette = 1:input$nb_classe,
      repel = TRUE,
      geom = c(ifelse(input$afficherind=="oui","text","arrow")),
      axes = c(ifelse(input$choixaxe=="dim1-dim2",1,3),
               ifelse(input$choixaxe=="dim1-dim2",2,4))
    )
  })
  
  output$acpnutrimentplotcah <- renderPlot({
    grp <- as.factor(cutree(cah(), h=input$nivo_cah))
    fviz_pca_var(
      acp,
      col.var = grp,
      palette = 1:length(levels(grp)),
      repel = TRUE,
      geom = c(ifelse(input$afficherind=="oui","text","arrow")),
      axes = c(ifelse(input$choixaxe=="dim1-dim2",1,3),
               ifelse(input$choixaxe=="dim1-dim2",2,4))
    )
  })
  
  output$acpplotind <- renderPlot({
    fviz_pca_ind(
      acp,
      habillage = (switch(input$choixmaladie,
                          "cholesterol"=1,"diabete"=2,"hypertension"=3)),
      addEllipses = T,
      geom = c(ifelse(input$afficherind=="oui","text","point"))
    )
  })
  
  output$acpplotdual <- renderPlot({
    grp <- as.factor(km()$cluster)
    fviz_pca_biplot(
      acp,
      habillage = (switch(input$choixmaladie,
                          "cholesterol"=1,"diabete"=2,"hypertension"=3)),
      label="var",
      col.var = grp,
      invisible = "quanti.sup"
    )
  })
  
  output$partitionkm <- renderPlot({
  fviz_cluster(km(),data=var$coord,main = "Partitioning Clustering Plot",
               repel = TRUE,
               geom = c(ifelse(input$afficherind=="oui","text","point")),
               axes = c(ifelse(input$choixaxe=="dim1-dim2",1,3),
                        ifelse(input$choixaxe=="dim1-dim2",2,4)))
  })
  
  output$partitionkmbis <- renderPlot({
    fviz_cluster(kmbis(),data=ind$coord,main = "Partitioning Clustering Plot",
                 geom = c(ifelse(input$afficherind=="oui","text","point")),
                 axes = c(ifelse(input$choixaxe=="dim1-dim2",1,3),
                          ifelse(input$choixaxe=="dim1-dim2",2,4)))
  })

  output$dendrogramme <- renderPlot({
    fviz_dend(
      cah(),
      type="phylogenic",
      rect = TRUE,rect_fill = TRUE,
      cex = 1,
      k_colors = 1:input$nivo_cah,
      rect_border = 1:input$nivo_cah,
      repel = TRUE,
      show_labels = ifelse(input$afficherind=="oui",TRUE,FALSE)
    )
  })
  
  
  output$groupekm <- renderDataTable({
  tempo <- as.data.frame(km()$cluster)
  tempo <- cbind(row.names(tempo),tempo)
  row.names(tempo) <- NULL
  colnames(tempo) <- c("nutriment","classe")
  datatable(tempo,class = 'cell-border stripe',filter = 'bottom',
            extensions = c('Buttons'), rownames = FALSE,
            options=list(autoWidth = TRUE, 
                         dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                         pageLength=20))
  })
  
  output$groupecah <- renderDataTable({
    tempo <- as.data.frame(cutree(cah(), h=input$nivo_cah))
    tempo <- cbind(row.names(tempo),tempo)
    row.names(tempo) <- NULL
    colnames(tempo) <- c("nutriment","classe")
    datatable(tempo,class = 'cell-border stripe',filter = 'bottom',
              extensions = c('Buttons'), rownames = FALSE,
              options=list(autoWidth = TRUE, 
                           dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                           pageLength=20))
  })
  
  output$contri1 <- renderPlot({
    fviz_contrib(acp, choice = "var", axes = 1, top = input$topcontrib)
  })
  
  output$contri2 <- renderPlot({
    fviz_contrib(acp, choice = "var", axes = 2, top = input$topcontrib)
  })

  ####
  # Les choix de variables pour Hypertension
  ####
  
  rang_val_hyp <- reactive(tabselvar_hyp[,-1][which(max_val_hyp==input$priohyp, arr.ind=TRUE)])
  
  output$tabselvarhyp <- renderDataTable({
  
    datatable(tabselvar_hyp,class = 'cell-border stripe',filter = 'bottom',
              extensions = c('Buttons'), rownames = FALSE,
              options=list(autoWidth = TRUE, 
                           dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                           pageLength=20)
              ) %>% 
      formatStyle(
                columns = 2:length(tabselvar_hyp), 
                backgroundColor = styleEqual(levels = rang_val_hyp(), 
                                             values = rep("yellow", length(rang_val_hyp())))) 
    })
  
####
# Les choix de variables pour Cholesterol
####  
  
  rang_val_chol <- reactive(tabselvar_chol[,-1][which(max_val_chol==input$priochol, arr.ind=TRUE)]) 

  output$tabselvarchol <- renderDataTable({
    
    datatable(tabselvar_chol,class = 'cell-border stripe',filter = 'bottom',
              extensions = c('Buttons'), rownames = FALSE,
              options=list(autoWidth = TRUE,
                           dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                           pageLength=20)
    ) %>%
      formatStyle(
        columns = 2:length(tabselvar_chol),
        backgroundColor = styleEqual(levels = rang_val_chol(),
                                     values = rep("yellow", length(rang_val_chol()))))
  })

  
####
# Les choix de variables pour Diabetes
####  
  
  rang_val_dia <- reactive(tabselvar_dia[,-1][which(max_val_dia==input$priodia, arr.ind=TRUE)])
  
  output$tabselvardia <- renderDataTable({
    datatable(tabselvar_dia,class = 'cell-border stripe',filter = 'bottom',
              extensions = c('Buttons'), rownames = FALSE,
              options=list(autoWidth = TRUE,
                           dom = 'flrBtip', buttons=c('copy', 'csv',I('colvis')),
                           pageLength=20)
    ) %>%
      formatStyle(
        columns = 2:length(tabselvar_dia),
        backgroundColor = styleEqual(levels = rang_val_dia(),
                                     values = rep("yellow", length(rang_val_dia()))))
  })
  
  

####
# Les metriques pour Hypertension
####
observeEvent(input$methodehyp==3,{
  output$choixmethode <- renderPlot({
    plot(roc(res_hyp[,1],res_hyp[,input$methodehyp[1]]),col="black",main="Courbes ROC")
    lines(roc(res_hyp[,1],res_hyp[,input$methodehyp[2]]), col="red")
    lines(roc(res_hyp[,1],res_hyp[,input$methodehyp[3]]), col="green")
    legend("bottomright",legend = c(input$methodehyp[1],input$methodehyp[2], input$methodehyp[3]), col=c("black","red","green"), lty = 1)
  })

  output$valAUC <- renderTable({
    tabauc <- data.frame(nom1=auc(res_hyp[,1],res_hyp[,input$methodehyp[1]]),
               auc(res_hyp[,1],res_hyp[,input$methodehyp[2]]),
               auc(res_hyp[,1],res_hyp[,input$methodehyp[3]])
    )
    names(tabauc) <- input$methodehyp    
    tabauc
  })
  
  output$matconf <- renderTable({
    tabconf <- cbind(as.data.frame(monerreur(res_hyp[,input$methodehyp[1]],res_hyp[,1])),
          as.data.frame(monerreur(res_hyp[,input$methodehyp[2]],res_hyp[,1]))[,3],
          as.data.frame(monerreur(res_hyp[,input$methodehyp[3]],res_hyp[,1]))[,3]
    )
    names(tabconf)[3:length(names(tabconf))] <- input$methodehyp
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:5] <- tabconf[4,3:5]/(tabconf[4,3:5]+tabconf[3,3:5])
    tabconf[6,3:5] <- tabconf[1,3:5]/(tabconf[1,3:5]+tabconf[2,3:5])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision <- renderTable({
    tabprecision <- data.frame(precision(res_hyp[,input$methodehyp[1]],res_hyp[,1]),
          precision(res_hyp[,input$methodehyp[2]],res_hyp[,1]),
          precision(res_hyp[,input$methodehyp[3]],res_hyp[,1]))
    names(tabprecision) <- input$methodehyp
    tabprecision
  })
})

####
# Les metriques pour Cholesterol
####
observeEvent(input$methodechol==3,{
  output$choixmethode_chol <- renderPlot({
    plot(roc(res_chol[,1],res_chol[,input$methodechol[1]]),col="black",main="Courbes ROC")
    lines(roc(res_chol[,1],res_chol[,input$methodechol[2]]), col="red")
    lines(roc(res_chol[,1],res_chol[,input$methodechol[3]]), col="green")
    legend("bottomright",legend = c(input$methodechol[1],input$methodechol[2], input$methodechol[3]), col=c("black","red","green"), lty = 1)
  })
  
  output$valAUC_chol <- renderTable({
    tabauc <- data.frame(nom1=auc(res_chol[,1],res_chol[,input$methodechol[1]]),
                         auc(res_chol[,1],res_chol[,input$methodechol[2]]),
                         auc(res_chol[,1],res_chol[,input$methodechol[3]])
    )
    names(tabauc) <- input$methodechol    
    tabauc
  })
  
  output$matconf_chol <- renderTable({
    tabconf <- cbind(as.data.frame(monerreur(res_chol[,input$methodechol[1]],res_chol[,1],seuil=input$seuilmodchol)),
                     as.data.frame(monerreur(res_chol[,input$methodechol[2]],res_chol[,1],seuil=input$seuilmodchol))[,3],
                     as.data.frame(monerreur(res_chol[,input$methodechol[3]],res_chol[,1],seuil=input$seuilmodchol))[,3]
    )
    names(tabconf)[3:length(names(tabconf))] <- input$methodechol
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:5] <- tabconf[4,3:5]/(tabconf[4,3:5]+tabconf[3,3:5])
    tabconf[6,3:5] <- tabconf[1,3:5]/(tabconf[1,3:5]+tabconf[2,3:5])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision_chol <- renderTable({
    tabprecision <- data.frame(precision(res_chol[,input$methodechol[1]],res_chol[,1],seuil=input$seuilmodchol),
                               precision(res_chol[,input$methodechol[2]],res_chol[,1],seuil=input$seuilmodchol),
                               precision(res_chol[,input$methodechol[3]],res_chol[,1],seuil=input$seuilmodchol))
    names(tabprecision) <- input$methodechol
    tabprecision
  })
})


####
# Les metriques pour le diabete
####
observeEvent(input$methodedia==3,{
  output$choixmethode_dia <- renderPlot({
    plot(roc(res_dia[,1],res_dia[,input$methodedia[1]]),col="black",main="Courbes ROC")
    lines(roc(res_dia[,1],res_dia[,input$methodedia[2]]), col="red")
    lines(roc(res_dia[,1],res_dia[,input$methodedia[3]]), col="green")
    legend("bottomright",legend = c(input$methodedia[1],input$methodedia[2], input$methodedia[3]), col=c("black","red","green"), lty = 1)
  })
  
  output$valAUC_dia <- renderTable({
    tabauc <- data.frame(nom1=auc(res_dia[,1],res_dia[,input$methodedia[1]]),
                         auc(res_dia[,1],res_dia[,input$methodedia[2]]),
                         auc(res_dia[,1],res_dia[,input$methodedia[3]])
    )
    names(tabauc) <- input$methodedia    
    tabauc
  })
  
  output$matconf_dia <- renderTable({
    tabconf <- cbind(as.data.frame(monerreur(res_dia[,input$methodedia[1]],res_dia[,1],seuil=input$seuilmoddia)),
                     as.data.frame(monerreur(res_dia[,input$methodedia[2]],res_dia[,1],seuil=input$seuilmoddia))[,3],
                     as.data.frame(monerreur(res_dia[,input$methodedia[3]],res_dia[,1],seuil=input$seuilmoddia))[,3]
    )
    
    names(tabconf)[3:length(names(tabconf))] <- input$methodedia
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:5] <- tabconf[4,3:5]/(tabconf[4,3:5]+tabconf[3,3:5])
    tabconf[6,3:5] <- tabconf[1,3:5]/(tabconf[1,3:5]+tabconf[2,3:5])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision_dia <- renderTable({
    tabprecision <- data.frame(precision(res_dia[,input$methodedia[1]],res_dia[,1],seuil=input$seuilmoddia),
                               precision(res_dia[,input$methodedia[2]],res_dia[,1],seuil=input$seuilmoddia),
                               precision(res_dia[,input$methodedia[3]],res_dia[,1],seuil=input$seuilmoddia))
    names(tabprecision) <- input$methodedia
    tabprecision
  })
})

#=================================
#Graphiques pour l'onglet CONTEXTE
#=================================


output$graphhyp<-renderPlot({
  xhyp<-donHyp[,input$varhyp]
  if (class(xhyp) %in% c("numeric","integer")) {
  ggplot(data=donHyp,aes(x=xhyp))+geom_histogram(binwidth=1,fill="light blue")} else {
      ggplot(data=donHyp,aes(x=xhyp))+geom_bar(fill="light blue")}})

output$graphcho<-renderPlot({
  xcho<-donChol[,input$varcho]
  if (class(xcho) %in% c("numeric","integer")) {
  ggplot(data=donChol,aes(x=xcho))+geom_histogram(binwidth=1,fill="light blue")} else {
    ggplot(data=donChol,aes(x=xcho))+geom_bar(fill="light blue")}})

output$graphdia<-renderPlot({
  xdia<-donDia[,input$vardia]
  if (class(xdia) %in% c("numeric","integer")) {
  ggplot(data=donDia,aes(x=xdia))+geom_histogram(binwidth=1,fill="light blue")} else {
    ggplot(data=donDia,aes(x=xdia))+geom_bar(fill="light blue")
    
  }
    
    
    })


})