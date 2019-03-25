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
shinyServer(function(input, output, session) {
  
  #variables retenues par modele
  
  observeEvent(input$idCheckHyp==TRUE, {
  output$valueHyp <- renderText({ paste0("age,trouble_sommeil,cholesterol,surpoids,bmi,pression_sys,pression_dia,phosphorus,sodium")})
  })
  
  observeEvent(input$idCheckChol==TRUE, {
    output$valueChol <- renderText({ paste0("sexe,age,travail,trouble_sommeil,risquehypertension,risquediabete,surpoids,hauteur,poids,diete,fibre,choles,vitB6,vitB12")})
  })
  
  observeEvent(input$idCheckDiab==TRUE, {
    output$valueDiab <- renderText({ paste0("sexe,age,pauvretefamille,cholesterol,risquehypertension,surpoids,pression_dia,waterdrank,eau dans les aliments,calories,sucre,graisse,carbonhydrate,conso cholesterol,activite")})
  })
  
  
  ####
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
  
  monerreurcho <- function(X, Y, seuil=input$seuilmodchol){
    table(cut(X, breaks = c(0,seuil,1)), Y)
  }
  
  monerreurdia <- function(X, Y, seuil=input$seuilmoddia){
    table(cut(X, breaks = c(0,seuil,1)), Y)
  }
  # fonction utilisée pour le calcul de la précision
  precision <- function(X,Y,seuil=input$seuilmod){
    Xc <- cut(X,breaks=c(0,seuil,1),labels=c(0,1),include.lowest=TRUE)
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
  
  
  
  observe({
    if (input$typepatient=="Nouveau Profil") { #AUCUN PROBLEME
      
      updateSelectInput(session,inputId="sexe",selected = sample(c("Male","Female"),1),
                        choices = c("M" = "Male", "F" = "Female"))
      #updateNumericInput(session,inputId="age",value = sample(18:100,1))                         #40 pour Chol
      updateNumericInput(session,inputId="age",value = 40)                                        #40 pour Chol
      #updateRadioButtons(session,inputId="marchevelodixmin",selected = sample(c("Yes","No"),1),
      #                   choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="travail",selected = sample(c("oui","non"),1),
                         choices = c("Oui" = "oui", "Non" = "non"))
      #updateNumericInput(session,inputId="piecesmaison",value = sample(1:6,1))
      updateNumericInput(session,inputId="pauvretefamille",value = sample(1:5,1))
      # updateRadioButtons(session,inputId="trouble_sommeil",selected =sample(c("Yes","No"),1),
      #                    choices = c("Oui" = "Yes", "Non" = "No"))                              #No pour Chol
      updateRadioButtons(session,inputId="trouble_sommeil",selected ="No",
                         choices = c("Oui" = "Yes", "Non" = "No"))                                #No pour Chol
      updateRadioButtons(session,inputId="cholesterol",selected = sample(c("Yes","No"),1),
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="risquehypertension",choices = c("Oui" = 1, "Non" = 2),
                         selected = sample(1:2,1))
      updateRadioButtons(session,inputId="risquediabete",choices = c("Oui" = 1, "Non" = 2),
                         selected = sample(1:2,1))
      # updateRadioButtons(session,inputId="surpoids",choices = c("Oui" = "Yes", "Non" = "No"),
      #                    selected = sample(c("Yes","No"),1))                                    #No pour Chol
      updateRadioButtons(session,inputId="surpoids",choices = c("Oui" = "Yes", "Non" = "No"),
                         selected = "No")                                                         #No pour Chol
      updateNumericInput(session,inputId="hauteur",value = 174)
      updateNumericInput(session,inputId="poids",value = 57)
      updateNumericInput(session,inputId="bmi",value = 18.8)
      updateNumericInput(session,inputId="pression_sys",value = 120)
      updateNumericInput(session,inputId="pression_dia",value = 80)
      # updateRadioButtons(session,inputId="dentaire",
      #                    choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
      #                                "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1),
      #                    selected = sample(1:4,1))
      updateRadioButtons(session,inputId="diete",choices = c("Oui" = 1, "Non" = 2),
                         selected = sample(1:2,1))
      updateNumericInput(session,inputId="fibre",value = sample(10:22,1))
      #updateNumericInput(session,inputId="foodfolate",value = sample(20:266,1))
      updateNumericInput(session,inputId="waterdrank",value = sample(200:1600,1))
      updateNumericInput(session,inputId="humidite",value = sample(600:3400,1))
      #updateNumericInput(session,inputId="alcool",value = sample(2:200,1))
      updateNumericInput(session,inputId="phosphorus",value = sample(100:1600,1))
      updateNumericInput(session,inputId="sodium",value = sample(1000:4000,1))
      updateNumericInput(session,inputId="sucre",value = sample(10:128,1))
      updateNumericInput(session,inputId="choles",value = sample(100:390,1))
      # updateNumericInput(session,inputId="proteines",value = sample(10:97,1))
      # updateNumericInput(session,inputId="fer",value = sample(4:16,1))
      # updateNumericInput(session,inputId="cafeine",value = sample(20:177,1))
      #updateNumericInput(session,inputId="vitamineC",value = sample(20:108,1))
      #updateNumericInput(session,inputId="LuteineZeaxanthine",value = sample(100:1462,1))
      #updateNumericInput(session,inputId="vitB6",value = sample(10:108,1))                        #33 pour Chol
      updateNumericInput(session,inputId="vitB6",value = 33)                        #33 pour Chol
      #updateNumericInput(session,inputId="vitB12",value = sample(10:108,1))                       #99 pour Chol
      updateNumericInput(session,inputId="vitB12",value = 99)                       #99 pour Chol
      updateRadioButtons(session,inputId="activite",choices = c("Oui" = "actif", "Non" = "inactif"),
                         selected = "actif")
    }
    if (input$typepatient=="Profil à l'instar de Sharon Stone") { #PROBLEME DIABETE

      updateSelectInput(session,inputId="sexe",selected = "Female",
                        choices = c("M" = "Male", "F" = "Female"))
      updateNumericInput(session,inputId="age",value = 61)
      
      updateRadioButtons(session,inputId="travail",selected = "oui",
                         choices = c("Oui" = "oui", "Non" = "non"))
      
      updateNumericInput(session,inputId="pauvretefamille",value = 5)
      updateRadioButtons(session,inputId="trouble_sommeil",selected = "No",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="cholesterol",selected = "No",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="risquehypertension",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateRadioButtons(session,inputId="risquediabete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateRadioButtons(session,inputId="surpoids",choices = c("Oui" = "Yes", "Non" = "No"),
                         selected = "No")
      updateNumericInput(session,inputId="hauteur",value = 174)
      updateNumericInput(session,inputId="poids",value = 57)
      updateNumericInput(session,inputId="bmi",value = 18.8)
      updateNumericInput(session,inputId="pression_sys",value = 120)
      updateNumericInput(session,inputId="pression_dia",value = 70)
      
      updateRadioButtons(session,inputId="diete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateNumericInput(session,inputId="fibre",value = 22)
      
      updateNumericInput(session,inputId="waterdrank",value = 900)
      updateNumericInput(session,inputId="humidite",value = 2500)
      
      updateNumericInput(session,inputId="phosphorus",value = 1200)
      updateNumericInput(session,inputId="sodium",value = 3000)
      updateNumericInput(session,inputId="sucre",value = 50)
      updateNumericInput(session,inputId="choles",value = 250)
      
      updateNumericInput(session,inputId="vitB6",value = 33)                    #33 pour Chol
      updateNumericInput(session,inputId="vitB12",value = 99)
      updateNumericInput(session,inputId="calorie",value = 1500) 
      updateNumericInput(session,inputId="carbo",value = 220) 
      updateNumericInput(session,inputId="graisse",value = 100) 
      updateRadioButtons(session,inputId="activite",choices = c("Oui" = "actif", "Non" = "inactif"),
                         selected = "actif")
      
      
    }
    if (input$typepatient=="Profil à l'instar de David Hasselhoff") { #PROBLEME HYPERTENSION et CHOLESTEROL
      
      updateSelectInput(session,inputId="sexe",selected = "Male",
                        choices = c("M" = "Male", "F" = "Female"))
      updateNumericInput(session,inputId="age",value = 67)
      #updateRadioButtons(session,inputId="marchevelodixmin",selected = "No",
      #                   choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="travail",selected = "oui",
                         choices = c("Oui" = "oui", "Non" = "non"))
      #updateNumericInput(session,inputId="piecesmaison",value = 6)
      updateNumericInput(session,inputId="pauvretefamille",value = 5)
      updateRadioButtons(session,inputId="trouble_sommeil",selected = "Yes",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="cholesterol",selected = "Yes",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="risquehypertension",choices = c("Oui" = 1, "Non" = 2),
                         selected = 1)
      updateRadioButtons(session,inputId="risquediabete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateRadioButtons(session,inputId="surpoids",choices = c("Oui" = "Yes", "Non" = "No"),
                         selected = "No")
      updateNumericInput(session,inputId="hauteur",value = 193)
      updateNumericInput(session,inputId="poids",value = 87)
      updateNumericInput(session,inputId="bmi",value = 23.4)
      updateNumericInput(session,inputId="pression_sys",value = 140)
      updateNumericInput(session,inputId="pression_dia",value = 90)
      #updateRadioButtons(session,inputId="dentaire",
      #                   choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
      #                               "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1),
      #                   selected = 2)
      updateRadioButtons(session,inputId="diete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 1)
      updateNumericInput(session,inputId="fibre",value = 22)
      #updateNumericInput(session,inputId="foodfolate",value = 900)
      updateNumericInput(session,inputId="waterdrank",value = 2500)
      updateNumericInput(session,inputId="humidite",value = 3400)
      #updateNumericInput(session,inputId="alcool",value = 1600)
      updateNumericInput(session,inputId="phosphorus",value = 1200)
      updateNumericInput(session,inputId="sodium",value = 3150)
      updateNumericInput(session,inputId="sucre",value = 250)
      updateNumericInput(session,inputId="choles",value = 390)
      # updateNumericInput(session,inputId="proteines",value = 97)
      # updateNumericInput(session,inputId="fer",value = 16)
      # updateNumericInput(session,inputId="cafeine",value = 177)
      #updateNumericInput(session,inputId="vitamineC",value = 108)
      #updateNumericInput(session,inputId="LuteineZeaxanthine",value = 1462)
      updateNumericInput(session,inputId="vitB6",value = 70)                    #33 pour annuler Chol
      updateNumericInput(session,inputId="vitB12",value = 108)
      updateRadioButtons(session,inputId="activite",choices = c("Oui" = "actif", "Non" = "inactif"),
                         selected = "actif")
    }
    
    if (input$typepatient=="Profil à l'instar de Michael Phelps") { #AUCUN PROBLEME (TRES SPORTIF)
      
      updateSelectInput(session,inputId="sexe",selected = "Male",
                        choices = c("M" = "Male", "F" = "Female"))
      updateNumericInput(session,inputId="age",value = 33)
      #updateRadioButtons(session,inputId="marchevelodixmin",selected = "Yes",
      #                   choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="travail",selected = "non",
                         choices = c("Oui" = "oui", "Non" = "non"))
      #updateNumericInput(session,inputId="piecesmaison",value = 6)
      updateNumericInput(session,inputId="pauvretefamille",value = 5)
      updateRadioButtons(session,inputId="trouble_sommeil",selected = "No",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="cholesterol",selected = "No",
                         choices = c("Oui" = "Yes", "Non" = "No"))
      updateRadioButtons(session,inputId="risquehypertension",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateRadioButtons(session,inputId="risquediabete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 2)
      updateRadioButtons(session,inputId="surpoids",choices = c("Oui" = "Yes", "Non" = "No"),
                         selected = "No")
      updateNumericInput(session,inputId="hauteur",value = 193)
      updateNumericInput(session,inputId="poids",value = 90)
      updateNumericInput(session,inputId="bmi",value = 24.2)
      updateNumericInput(session,inputId="pression_sys",value = 120)
      updateNumericInput(session,inputId="pression_dia",value = 80)
      #updateRadioButtons(session,inputId="dentaire",
      #                   choices = c("Continuez comme ça" = 4, "N'hésitez pas à visiter un dentiste" = 3, 
      #                               "Visitez un dentiste dans les 2 semaines prochaines" = 2, "Visitez un dentiste immédiatement" = 1),
      #                   selected = 2)
      updateRadioButtons(session,inputId="diete",choices = c("Oui" = 1, "Non" = 2),
                         selected = 1)
      updateNumericInput(session,inputId="fibre",value = 22)
      #updateNumericInput(session,inputId="foodfolate",value = 266)
      updateNumericInput(session,inputId="waterdrank",value = 1600)
      updateNumericInput(session,inputId="humidite",value = 3400)
      #updateNumericInput(session,inputId="alcool",value = 100)
      updateNumericInput(session,inputId="phosphorus",value = 1200)
      updateNumericInput(session,inputId="sodium",value = 3150)
      updateNumericInput(session,inputId="sucre",value = 120)
      updateNumericInput(session,inputId="choles",value = 250)
      # updateNumericInput(session,inputId="proteines",value = 97)
      # updateNumericInput(session,inputId="fer",value = 16)
      # updateNumericInput(session,inputId="cafeine",value = 57)
      #updateNumericInput(session,inputId="vitamineC",value = 108)
      #updateNumericInput(session,inputId="LuteineZeaxanthine",value = 1462)
      updateNumericInput(session,inputId="vitB6",value = 55)
      updateNumericInput(session,inputId="vitB12",value = 108)
      updateRadioButtons(session,inputId="activite",choices = c("Oui" = "actif", "Non" = "inactif"),
                         selected = "actif")
    }
    
    output$im_profil <- renderImage({
      if(input$typepatient=="Profil à l'instar de David Hasselhoff"){
        filename <- normalizePath(file.path('./www/img/DavidHasselhoff.jpg'))
      }
      if(input$typepatient=="Profil à l'instar de Michael Phelps"){
        filename <- normalizePath(file.path('./www/img/MichaelPhelps.jpg'))
      }
      if(input$typepatient=="Profil à l'instar de Sharon Stone"){
        filename <- normalizePath(file.path('./www/img/SharonStone.jpg'))
      }
      if(input$typepatient=="Nouveau Profil"){
        filename <- normalizePath(file.path('./www/img/NouveauProfil.jpg'))
      }
      
      list(src = filename,width = 250,height = 250)},deleteFile = FALSE)
    })
  
#-------------------------------------------------------------  
  # j'ajoute une observation  sur la button pour lancer la prédiction
  observeEvent(input$predict==TRUE, {
    tempoHyp <- reactive({predict(modHyp,data.frame(Age_in_years_at_screening=input$age,
                                          Systolic_Blood_pres_2nd_rdg_mm_Hg=input$pression_sys,
                                          high_cholesterol_level=input$cholesterol,
                                          Body_Mass_Index_kg_m_2=input$bmi,
                                          Doctor_ever_said_you_were_overweight=input$surpoids,
                                          Ever_told_doctor_had_trouble_sleeping=input$trouble_sommeil,
                                          Phosphorus_mg=input$phosphorus,
                                          Diastolic_Blood_pres_1st_rdg_mm_Hg=input$pression_dia,
                                          Sodium_mg=input$sodium
    ),type="response", se.fit = T)})
    
    output$resultat_hypertension <- renderText({
    paste0("Proba: ",round(tempoHyp()$fit,3),"  IT: ", round(tempoHyp()$fit-1.96*tempoHyp()$se.fit,3),
          "-", round(tempoHyp()$fit+1.96*tempoHyp()$se.fit,3))
    })
    output$resultat_hypertensionbis <- renderText({
    ifelse(tempoHyp()$fit>seuil_hyp,"Hypertension: Danger!!","Hypertension: :-D")
    })
    
    tempoChol <- reactive({predict(modChol,data.frame(RIDAGEYR_demo=input$age,
                                            #HOD050_hoq=input$piecesmaison,
                                            #PAQ635_paq=ifelse(input$marchevelodixmin=="Yes","1", "2"),
                                            #DR1TLZ_dr1tot=input$LuteineZeaxanthine,
                                            #DR1TVC_dr1tot=input$vitamineC,
                                            #DR1TMOIS_dr1tot=input$humidite,
                                            RIAGENDR_demo=ifelse(input$sexe=="Male", "1", "2"),
                                            #BPXSY3_bpx=input$pression_sys,
                                            #BMXBMI_bmx=input$bmi, #je desactive pq conflit visuel dans les param de linterface prediction
                                            MCQ080_mcq=ifelse(input$surpoids=="Yes","1","2"),
                                            SLQ050_slq=ifelse(input$trouble_sommeil=="Yes","1","2"),
                                            #BPXDI2_bpx=input$pression_dia,
                                            #INDFMPIR_demo=input$pauvretefamille,
                                            Var_TRAVAIL=input$travail,
                                            BMXHT_bmx=input$hauteur,
                                            BMXWT_bmx=input$poids,
                                            BPQ020_bpq=input$risquehypertension,
                                            DIQ010_diq=input$risquediabete,
                                            #OHAREC_ohxref=input$dentaire,
                                            DRQSDIET_dr1tot=input$diete,
                                            DR1TFIBE_dr1tot=input$fibre,
                                            #DR1TALCO_dr1tot=input$alcool,
                                            #DR1TFF_dr1tot=input$foodfolate
                                            #DR1.320Z_dr1tot=input$waterdrank
                                            DR1TVB6_dr1tot=input$vitB6,
                                            DR1TCHOL_dr1tot=input$choles,
                                            DR1TB12A_dr1tot=input$vitB12
    ),type="response", se.fit = T)})
  
    output$resultat_cholesterol <- renderText({
      paste0("Proba: ",round(tempoChol()$fit,3),"  IT: ", round(tempoChol()$fit-1.96*tempoChol()$se.fit,3),
            "-", round(tempoChol()$fit+1.96*tempoChol()$se.fit,3))
    })
    output$resultat_cholesterolbis <- renderText({
      ifelse(tempoChol()$fit>seuil_chol,"Cholesterol: Danger!!","Cholesterol: :-D")
    })
    
    tempoDiab <- reactive({predict(modDiab,
                                   data.frame(RIDAGEYR_demo=input$age,
                                            BPQ080_bpq=ifelse(input$cholesterol=="Yes",c("1"),c("2")),
                                            MCQ080_mcq=ifelse(input$surpoids=="Yes",c("1"),c("2")),
                                            BPQ020_bpq=ifelse(input$risquehypertension==1,c("1"),c("2")), 
                                            DR1TSUGR_dr1tot=input$sucre,
                                            DR1TKCAL_dr1tot=input$calorie,
                                            DR1TCARB_dr1tot=input$carbo,
                                            DR1TTFAT_dr1tot=input$graisse,
                                            DR1TMOIS_dr1tot=input$humidite,
                                            INDFMPIR_demo=input$pauvretefamille,
                                            RIAGENDR_demo=ifelse(input$sexe=="Male",c("1"),c("2")),
                                            DR1TCHOL_dr1tot=input$choles,
                                            DR1.320Z_dr1tot=input$waterdrank,
                                            Var_TENSIONDI=input$pression_dia,
                                            Var_ACTIVITE=input$activite),
                                            type="response",se.fit = T)})
     
    
    output$resultat_diabetes <- renderText({
      paste0("Proba: ",round(tempoDiab()$fit,3),"  IT: ", round(tempoDiab()$fit-1.96*tempoDiab()$se.fit,3),
            "-", round(tempoDiab()$fit+1.96*tempoDiab()$se.fit,3))
    })
    output$resultat_diabetesbis <- renderText({
      ifelse(tempoDiab()$fit>seuil_dia,"Diabete: Danger!!","Diabete: :-D")
    })
    
    output$im_hyp_g <- renderImage({
      ifelse(tempoHyp()$fit<seuil_hyp,
      filename <- normalizePath(file.path('./www/img/hypertensiongood.jpg')),
      filename <- normalizePath(file.path('./www/img/hypertensionbad.jpg')))
      list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
    
    output$im_cho_g <- renderImage({
      ifelse(tempoChol()$fit<seuil_chol,
      filename <- normalizePath(file.path('./www/img/Cholesterolgood.jpg')),
      filename <- normalizePath(file.path('./www/img/Cholesterolbad.png')))
      list(src = filename,width = 200,height = 200)},deleteFile = FALSE)
    
    output$im_dia_g <- renderImage({
      ifelse(tempoDiab()$fit<seuil_dia,
      filename <- normalizePath(file.path('./www/img/diabetegood.jpg')),
      filename <- normalizePath(file.path('./www/img/diabetebad.png')))
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
                          "cholesterol"=1,"diabetes"=2,"hypertension"=3)), #pas utiliser des accents
      addEllipses = T,
      geom = c(ifelse(input$afficherind=="oui","text","point"))
    )
  })
  
  output$acpplotdual <- renderPlot({
    grp <- as.factor(km()$cluster)
    fviz_pca_biplot(
      acp,
      habillage = (switch(input$choixmaladie,
                          "cholesterol"=1,"diabetes"=2,"hypertension"=3)), #pas utiliser des accents
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
    colnames(tempo) <- c("nutriment Kmeans","classe")
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
    colnames(tempo) <- c("nutriment CAH","classe")
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
  output$choixmethode <- renderPlot({
    plot(roc(res_hyp[,1],res_hyp[,input$methodehyp[1]]),col="black",main="Courbes ROC",
         print.thres = "best", print.thres.best.method = "closest.topleft")
    i=1
    traithyp <- c(input$methodehyp[1])
    seuil <- c(round(coords(roc(res_hyp[,1],res_hyp[,input$methodehyp[1]]), "best", best.method = "closest.topleft")[1],3))
    repeat{
      i=i+1
      if(i>length(input$methodehyp)) break
      lines(roc(res_hyp[,1],res_hyp[,input$methodehyp[i]]), col= i,
            print.thres = "best", print.thres.best.method = "closest.topleft")
      traithyp <- c(traithyp,input$methodehyp[i])
      seuil <- c(seuil,round(coords(roc(res_hyp[,1],res_hyp[,input$methodehyp[i]]), "best", best.method = "closest.topleft")[1],3))
      legend("bottomright",legend = paste(traithyp,seuil,sep = "_"), col=1:i, lty = 1)
    }
  })
  
  
  output$valAUC <- renderTable({
    tabauc <- data.frame(nom1=auc(res_hyp[,1],res_hyp[,input$methodehyp[1]]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodehyp)) break
      tabauc <- cbind(tabauc,auc(res_hyp[,1],res_hyp[,input$methodehyp[i]]))
    }
    names(tabauc) <- input$methodehyp
    tabauc
  })
  
  output$matconf <- renderTable({
    tabconf <- as.data.frame(monerreur(res_hyp[,input$methodehyp[1]],res_hyp[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodehyp)) break
      tabconf <- cbind(tabconf,as.data.frame(monerreur(res_hyp[,input$methodehyp[i]],res_hyp[,1]))[,3])
    }
    names(tabconf)[3:length(names(tabconf))] <- input$methodehyp
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:length(names(tabconf))] <- tabconf[4,3:length(names(tabconf))]/(tabconf[4,3:length(names(tabconf))]+tabconf[3,3:length(names(tabconf))])
    tabconf[6,3:length(names(tabconf))] <- tabconf[1,3:length(names(tabconf))]/(tabconf[1,3:length(names(tabconf))]+tabconf[2,3:length(names(tabconf))])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision <- renderTable({
    tabprecision <- data.frame(precision(res_hyp[,input$methodehyp[1]],res_hyp[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodehyp)) break
      tabprecision <- cbind(tabprecision,precision(res_hyp[,input$methodehyp[i]],res_hyp[,1]))
    }
    names(tabprecision) <- input$methodehyp
    tabprecision
  })
  
  ####
  # Les metriques pour Cholesterol
  ####
  
  output$choixmethode_chol <- renderPlot({
    plot(roc(res_chol[,1],res_chol[,input$methodechol[1]]),col="black",main="Courbes ROC",
         print.thres = "best", print.thres.best.method = "closest.topleft")
    i=1
    traitchol <- c(input$methodechol[1])
    seuil <- c(round(coords(roc(res_chol[,1],res_chol[,input$methodechol[1]]), "best", best.method = "closest.topleft")[1],3))
    repeat{
      i=i+1
      if(i>length(input$methodechol)) break
      lines(roc(res_chol[,1],res_chol[,input$methodechol[i]]), col= i,
            print.thres = "best", print.thres.best.method = "closest.topleft")
      traitchol <- c(traitchol,input$methodechol[i])
      seuil <- c(seuil,round(coords(roc(res_chol[,1],res_chol[,input$methodechol[i]]), "best", best.method = "closest.topleft")[1],3))
      legend("bottomright",legend = paste(traitchol,seuil,sep = "_"), col=1:i, lty = 1)
    }
  })
  
  output$valAUC_chol <- renderTable({
    tabauc <- data.frame(nom1=auc(res_chol[,1],res_chol[,input$methodechol[1]]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodechol)) break
      tabauc <- cbind(tabauc,auc(res_chol[,1],res_chol[,input$methodechol[i]]))
    }
    names(tabauc) <- input$methodechol
    tabauc
  })
  
  output$matconf_chol <- renderTable({
    tabconf <- as.data.frame(monerreurcho(res_chol[,input$methodechol[1]],res_chol[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodechol)) break
      tabconf <- cbind(tabconf,as.data.frame(monerreurcho(res_chol[,input$methodechol[i]],res_chol[,1]))[,3])
    }
    names(tabconf)[3:length(names(tabconf))] <- input$methodechol
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:length(names(tabconf))] <- tabconf[4,3:length(names(tabconf))]/(tabconf[4,3:length(names(tabconf))]+tabconf[3,3:length(names(tabconf))])
    tabconf[6,3:length(names(tabconf))] <- tabconf[1,3:length(names(tabconf))]/(tabconf[1,3:length(names(tabconf))]+tabconf[2,3:length(names(tabconf))])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision_chol <- renderTable({
    tabprecision <- data.frame(precision(res_chol[,input$methodechol[1]],res_chol[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodechol)) break
      tabprecision <- cbind(tabprecision,precision(res_chol[,input$methodechol[i]],res_chol[,1]))
    }
    names(tabprecision) <- input$methodechol
    tabprecision
  })
  
  ####
  # Les metriques pour le diabete
  ####
  output$choixmethode_dia <- renderPlot({
    plot(roc(res_dia[,1],res_dia[,input$methodedia[1]]),col="black",main="Courbes ROC",
         print.thres = "best", print.thres.best.method = "closest.topleft")
    i=1
    traitdia <- c(input$methodedia[1])
    seuil <- c(round(coords(roc(res_dia[,1],res_dia[,input$methodedia[1]]), "best", best.method = "closest.topleft")[1],3))
    repeat{
      i=i+1
      if(i>length(input$methodedia)) break
      lines(roc(res_dia[,1],res_dia[,input$methodedia[i]]), col= i,
            print.thres = "best", print.thres.best.method = "closest.topleft")
      traitdia <- c(traitdia,input$methodedia[i])
      seuil <- c(seuil,round(coords(roc(res_dia[,1],res_dia[,input$methodedia[i]]), "best", best.method = "closest.topleft")[1],3))
      legend("bottomright",legend = paste(traitdia,seuil,sep = "_"), col=1:i, lty = 1)
    }
  })
  
  output$valAUC_dia <- renderTable({
    tabauc <- data.frame(nom1=auc(res_dia[,1],res_dia[,input$methodedia[1]]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodedia)) break
      tabauc <- cbind(tabauc,auc(res_dia[,1],res_dia[,input$methodedia[i]]))
    }
    names(tabauc) <- input$methodedia
    tabauc
  })
  
  output$matconf_dia <- renderTable({
    tabconf <- as.data.frame(monerreurdia(res_dia[,input$methodedia[1]],res_dia[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodedia)) break
      tabconf <- cbind(tabconf,as.data.frame(monerreurdia(res_dia[,input$methodedia[i]],res_dia[,1]))[,3])
    }
    names(tabconf)[3:length(names(tabconf))] <- input$methodedia
    names(tabconf)[1] <- "seuil"
    tabconf[5,3:length(names(tabconf))] <- tabconf[4,3:length(names(tabconf))]/(tabconf[4,3:length(names(tabconf))]+tabconf[3,3:length(names(tabconf))])
    tabconf[6,3:length(names(tabconf))] <- tabconf[1,3:length(names(tabconf))]/(tabconf[1,3:length(names(tabconf))]+tabconf[2,3:length(names(tabconf))])
    tabconf$seuil <- as.character(tabconf$seuil)
    tabconf$seuil[5] <- "Sensibilité"
    tabconf$seuil[6] <- "Spécificité"
    tabconf
  })
  
  output$matprecision_dia <- renderTable({
    tabprecision <- data.frame(precision(res_dia[,input$methodedia[1]],res_dia[,1]))
    i=1
    repeat{
      i=i+1
      if(i>length(input$methodedia)) break
      tabprecision <- cbind(tabprecision,precision(res_dia[,input$methodedia[i]],res_dia[,1]))
    }
    names(tabprecision) <- input$methodedia
    tabprecision
  })

  #tables de coefficients de la logistique
  output$coefdiatab <- renderTable({
    
    coefdia
  })
  output$coefchotab <- renderTable({
    
    coefcho
  })
  output$coefhyptab <- renderTable({
    
    coefhyp
  })
  
  
  output$Mdiagraph <- renderPlot({corrplot(Mdiacor)})
  
  #=================================
  #Graphiques pour l'onglet CONTEXTE
  #=================================
  
  #DIABETE
  #-------
  output$tabstatdia<-renderTable({
    xdia<-donDia_transco[,input$vardiax]
    if (class(xdia) %in% c("numeric","integer")) {
      tabstat<-data.frame(matrix(NA,nrow = 6,ncol=2))
      colnames(tabstat)<-c("Type","Valeur")
      tabstat[,1]<-c("Type","Min","1er quartile","2ème quartile","3ème quartile","Max")
      tabstat[1,2]<-class(xdia)
      tabstat[2,2]<-quantile(xdia)[1]
      tabstat[3,2]<-quantile(xdia)[2]
      tabstat[4,2]<-quantile(xdia)[3]
      tabstat[5,2]<-quantile(xdia)[4]
      tabstat[6,2]<-quantile(xdia)[5]
      tabstat} else {
        tabstat<-data.frame(table(xdia))
        colnames(tabstat)<-c("Modalité","Effectif")
        tabstat
      }
  })
  
  output$graph1dia<-renderPlotly({
    x1dia<-donDia_transco[,input$vardiax]
    x2dia<-donDia_transco[donDia_transco[,12]=="1",input$vardiax]
    
    if (class(x1dia) %in% c("numeric","integer")) {
      
      ggplot(donDia_transco, aes(x=x1dia, color=donDia_transco[,12])) +
        geom_histogram(fill="white", alpha=0.5, position="identity")+scale_color_manual(values=c("green","red"))+
        xlab(input$vardiax)
    } else {
      
      g<-ggplot(data=donDia_transco, aes(x=x1dia,fill=donDia_transco[,12]))+geom_bar()+scale_fill_manual(values = c("green","red"))+
        xlab(input$vardiax)
      ggplotly(g) }
    
  })
  
  output$graph2dia<-renderPlotly({
    x1dia<-donDia_transco[,input$vardiaxy[1]]
    x2dia<-donDia_transco[,input$vardiaxy[2]]
    
    if (input$idgraphtype==1) {
      g<-qplot(x=x1dia,y=x2dia,data=donDia_transco,color=donDia_transco[,12],geom="point")+scale_color_manual(values=c("green","red"))+
        xlab(input$vardiaxy[[1]])+ylab(input$vardiaxy[[2]])
      ggplotly(g)
    } else {
      if (input$idgraphtype==2) {
        
        g<-ggplot(data=donDia_transco,aes(x=x1dia,y=x2dia))+geom_boxplot()+
          xlab(input$vardiaxy[[1]])+ylab(input$vardiaxy[[2]])
        ggplotly(g) } else {
          
          if (input$idgraphtype==3) {
            g<-ggplot(data=donDia_transco,aes(x=x1dia,y=x2dia))+geom_bin2d()+scale_fill_gradient(low="yellow",high="red")+
              xlab(input$vardiaxy[[1]])+ylab(input$vardiaxy[[2]])
            ggplotly(g)}}}
    
  })
  
  #HYPERTENSION
  #------------
  output$tabstathyp<-renderTable({
    xhyp<-donHyp[,input$varhypx]
    if (class(xhyp) %in% c("numeric","integer")) {
      tabstat<-data.frame(matrix(NA,nrow = 6,ncol=2))
      colnames(tabstat)<-c("Type","Valeur")
      tabstat[,1]<-c("Type","Min","1er quartile","2ème quartile","3ème quartile","Max")
      tabstat[1,2]<-class(xhyp)
      tabstat[2,2]<-quantile(xhyp)[1]
      tabstat[3,2]<-quantile(xhyp)[2]
      tabstat[4,2]<-quantile(xhyp)[3]
      tabstat[5,2]<-quantile(xhyp)[4]
      tabstat[6,2]<-quantile(xhyp)[5]
      tabstat} else {
        tabstat<-data.frame(table(xhyp))
        colnames(tabstat)<-c("Modalité","Effectif")
        tabstat
      }
  })
  
  output$graph1hyp<-renderPlotly({
    x1hyp<-donHyp[,input$varhypx]
    x2hyp<-donHyp[donHyp$Y=="1",input$varhypx]
    if (class(x1hyp) %in% c("numeric","integer")) {
      
      ggplot(donHyp, aes(x=x1hyp, color=donHyp$Y)) +
        geom_histogram(fill="white", alpha=0.5, position="identity")+scale_color_manual(values=c("green","red"))+
        xlab(input$varhypx)
    } else {
      g<-ggplot(data=donHyp, aes(x=x1hyp,fill=donHyp$Y))+geom_bar()+scale_fill_manual(values = c("green","red"))+
        xlab(input$varhypx)
      ggplotly(g) }
    
  })
  
  output$graph2hyp<-renderPlotly({
    x1hyp<-donHyp[,input$varhypxy[1]]
    x2hyp<-donHyp[,input$varhypxy[2]]
    
    if (input$idgraphtypehyp==1) {
      g<-qplot(x=x1hyp,y=x2hyp,data=donHyp,color=donHyp$Y,geom="point")+scale_color_manual(values=c("green","red"))+
        xlab(input$varhypxy[[1]])+ylab(input$varhypxy[[2]])
      ggplotly(g)
    } else {
      if (input$idgraphtypehyp==2) {
        
        g<-ggplot(data=donHyp,aes(x=x1hyp,y=x2hyp))+geom_boxplot()+
          xlab(input$varhypxy[[1]])+ylab(input$varhypxy[[2]])
        ggplotly(g) } else {
          
          if (input$idgraphtypehyp==3) {
            g<-ggplot(data=donHyp,aes(x=x1hyp,y=x2hyp))+geom_bin2d()+scale_fill_gradient(low="yellow",high="red")+
              xlab(input$varhypxy[[1]])+ylab(input$varhypxy[[2]])
            ggplotly(g)}}}
    
  })
  
  #CHOLESTEROL
  #-----------
  output$tabstatcho<-renderTable({
    xcho<-donChol[,input$varchox]
    if (class(xcho) %in% c("numeric","integer")) {
      tabstat<-data.frame(matrix(NA,nrow = 6,ncol=2))
      colnames(tabstat)<-c("Type","Valeur")
      tabstat[,1]<-c("Type","Min","1er quartile","2ème quartile","3ème quartile","Max")
      tabstat[1,2]<-class(xcho)
      tabstat[2,2]<-quantile(xcho)[1]
      tabstat[3,2]<-quantile(xcho)[2]
      tabstat[4,2]<-quantile(xcho)[3]
      tabstat[5,2]<-quantile(xcho)[4]
      tabstat[6,2]<-quantile(xcho)[5]
      tabstat} else {
        tabstat<-data.frame(table(xcho))
        colnames(tabstat)<-c("Modalité","Effectif")
        tabstat
      }
  })
  
  output$graph1cho<-renderPlotly({
    x1cho<-donChol[,input$varchox]
    x2cho<-donChol[donChol$Y==1,input$varchox]
    if (class(x1cho) %in% c("numeric","integer")) {
      
      ggplot(donChol, aes(x=x1cho, color=donChol$Y)) +
        geom_histogram(fill="white", alpha=0.5, position="identity")+scale_color_manual(values=c("green","red"))+
        xlab(input$varchox)
    } else {
      g<-ggplot(data=donChol, aes(x=x1cho,fill=donChol$Y))+geom_bar()+scale_fill_manual(values = c("green","red"))+
        xlab(input$varchox)
      ggplotly(g) }
    
  })
  
  output$graph2cho<-renderPlotly({
    x1cho<-donChol[,input$varchoxy[1]]
    x2cho<-donChol[,input$varchoxy[2]]
    
    if (input$idgraphtypecho==1) {
      g<-qplot(x=x1cho,y=x2cho,data=donChol,color=donChol$Y,geom="point")+scale_color_manual(values=c("green","red"))+
        xlab(input$varchoxy[[1]])+ylab(input$varchoxy[[2]])
      ggplotly(g)
    } else {
      if (input$idgraphtypecho==2) {
        
        g<-ggplot(data=donChol,aes(x=x1cho,y=x2cho))+geom_boxplot()+
          xlab(input$varchoxy[[1]])+ylab(input$varchoxy[[2]])
        ggplotly(g) } else {
          
          if (input$idgraphtypecho==3) {
            g<-ggplot(data=donChol,aes(x=x1cho,y=x2cho))+geom_bin2d()+scale_fill_gradient(low="yellow",high="red")+
              xlab(input$varchoxy[[1]])+ylab(input$varchoxy[[2]])
            ggplotly(g)}}}
    
    
  })
  
})