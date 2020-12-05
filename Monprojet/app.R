# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(devtools)
library(ggplot2) 

#Sample data 
#dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5)) 
#                  , lines = rep(c("a", "b"), each = 100)) 
#Plot. 
#ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) 

#Sample data 
#dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5)) 
#                  , lines = rep(c("a", "b"), each = 100)) 
# #Plot. 
# ggplot(consos2, aes(x = conso_moyenne_residentiel_mwh_, fill = annee)) + geom_density(alpha = 0.2)
# consos2=consos %>% select(conso_moyenne_residentiel_mwh_,annee)
# consos2$annee<-as.factor(consos2$annee)
# class(consos2$conso_moyenne_residentiel_mwh_)
#plot(density(consos$conso_moyenne_residentiel_mwh_))


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')
consos_quali <- consos%>%select(annee,nom_departement,nom_region)
consos_quanti <- consos%>%select(-annee,-nom_departement,-nom_region,-code_departement,-code_region)
##Consos mailles régionales pour l onglet regions



# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
    'Analyses des consommations electriques',
    
    tabPanel('Mon département',
             id = 'departements',
             
             
             
             sidebarLayout(
                 sidebarPanel(
                     
                     # Choix du département 
                     #choix parmi toutes les possibilités 
                     selectInput("dep",
                                 "Choisissez votre departement:",
                                 choices = levels(consos$nom_departement),
                                 selected = 'Doubs')
                 ),
             
             mainPanel(
                 ##affichage du nom du departement
                 h3(textOutput('nom_dep')),
                 

                 
                 
             )
             
    )
    
))



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$nom_dep <- renderText({
        ##TODO: modifier pour afficher le nom du departement!!!! ok
        input$dep
    })
    

    
    filtre <- reactive({
        ##TODO: rajouter aussi un filtre sur les annees
        consos %>% 
            filter(nom_departement == input$dep)
    })
    
    
   
    output$ma_table <- renderTable({
        out <-  filtre() %>%
            select(annee,  conso_totale_residentiel_mwh_,
                   conso_totale_professionnel_mwh_,
                   conso_totale_agriculture_mwh_,
                   conso_totale_tertiaire_mwh_,
                   conso_totale_autres_mwh_)
        print(out)
        out
    } )
    
    
   
}



# Run the application 
shinyApp(ui = ui, server = server)


