library(shiny)
library(dplyr)

# Preparation des données -------------------------------------------------
library(readr)
bilan_electrique_transpose <- read_delim("C:/Users/Camille/Documents/Projet_shiny/projet/bilan-electrique-transpose.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

# renommer les variables
colnames(bilan_electrique_transpose) <- c("jour","categorie_client","puissance")
plot(bilan_electrique_transpose$puissance~bilan_electrique_transpose$jour)
bilan <- bilan_electrique_transpose[order(bilan_electrique_transpose$jour),]


# ui ----------------------------------------------------------------------

ui <- navbarPage(
  'Analyses des consommations electriques',
  
  tabPanel('Mon département',
           id = 'departements',
           sidebarLayout(
             sidebarPanel(
               
               # Choix du consommateur 
               #choix parmi toutes les possibilités 
               selectInput("segment",
                           "Choisissez votre consomateur:",
                           choices = levels(bilan$categorie_client),
                           selected = 'Entreprises')
               )
             ,
               mainPanel(
                 ##affichage du nom du segment
                 h3(textOutput('nom_segment')),
                # tableOutput('ma_table'),
                 plotOutput('courbe_realise')

               )
  
  )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$nom_segment <- renderText({
    input$segment
  })
  
  filtre <- reactive({
    bilan %>% 
      filter(categorie_client == input$segment)
  })
}

# output$ma_table <- renderTable({
#   out <-  filtre() %>%
#     select(jour,categorie_client,puissance)
#   print(out)
#   out
# } )


output$courbe_realise <- renderPlot({
  
df <- filtre() %>%
  select(jour, puissance)%>%
  tidyr::pivot_longer(-c("jour"))


fig= ggplot(df) +
  aes(y  = value, x = annee, color = name)+
  geom_line()

fig

})

# Run the application 
shinyApp(ui = ui, server = server)