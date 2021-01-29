library(shiny)
library(tidyverse)
library(formattable)

## app.R ##
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)

## app.R ##

seuils <- function(pop) {
  seuils <- data.frame(niveau = c("niveau 1","niveau 2", "niveau 3"),
                       budget = c(pop*2, pop, pop*0.5),
                       horaires_ouvertures = c(12,8,8),
                       surface_mini = c(100,50,25),
                       surface = c(pop*0.07, pop*0.04, pop*0.04),
                       salaries = c( ifelse( pop / 2000 < 1, 1, floor(pop / 2000)),1, 0),
                       agents_b = c(floor(pop/5000), 0, 0))
  return(seuils)
}



names <- (c("Niveau ministère",
            "Budget d'acquisition (en €)",
            "Amplitude horaire hebdomadaire",
            "Surface minimum",
            "Surface totale",
            "Nombre de salariés",
            "Nombre d'agents de catégorie B"))

#df critere : var_name = "bibli"

evaluation <- function(criteres, seuils) {
  
  # récupère df critères
  c <- criteres
  
  s <- seuils %>% pivot_longer(cols = c(2:7), names_to = "critere", values_to = "values") %>% 
    pivot_wider(names_from = niveau, values_from = values) %>% 
    setNames(c("critere","n1","n2","n3")) #reformate seuils
  
  result <- cbind(s,c) %>% 
    mutate(score = case_when(bibli < n2 ~ 3,
                             bibli >= n2 & bibli < n1 ~ 2,
                             TRUE ~ 1)) %>% # mutate colonne résultats
    dplyr::select(-c("n1","n2","n3")) # dplyr::select(-trois niveaux seuils)    
  
  return(result)
}



server <- function(input, output) {
  
  observeEvent(input$do, {
    criteres <- matrix(list(input$budget,input$horaires_ouverture,input$surface,
                            input$surface,input$salaries,input$agents_b), ncol = 1, byrow = TRUE) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% setNames("bibli")
    
    output$table <- renderTable(setNames(seuils(input$pop), names))
    output$score_table <- renderTable(evaluation(criteres, seuils(input$pop)))
  })
}

ui <- fluidPage(
  titlePanel("Kit data bibliothèque 1 : diagnostic établissement"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("pop", "Population de votre commune ou agglomération",value = 5000),
      p("Une fois renseignés les champs ci-dessous, cliquez sur le bouton ",
        em("évaluer"),
        " pour savoir quel niveau votre établissement atteint pour chaque critère de l'Observatoitre de la lecture publique et le niveau global auquel il peut prétendre."),
      textInput("nom", "", placeholder = 'nom de votre établissement'),
      numericInput("budget","Budget annuel d'acquisition (en €)",  value = 10000),
      sliderInput("horaires_ouverture", "Amplitude horaire hebdomadaire", min = 4, max = 35, value = 12),
      numericInput("surface","Surface (en m²)",  value = 80),
      sliderInput("salaries", "Nombre de salariés", min = 0, max = 100, value = 2),
      sliderInput("agents_b", "Nombre d'agents de catégorie B:", min = 0, max = 50, value = 0),
      actionButton("do", "Evaluer")
      ),
    
    mainPanel(
      h3("Seuils par niveau d'établissement"),
      tableOutput('table'),
      br(),
      h3("Carte de résultat"),
      tableOutput('score_table'),
      
    )))

shinyApp(ui = ui, server = server)

# usage du bouton do et du dataframe userdata : https://community.rstudio.com/t/r-shiny-help-merging-user-input-data-with-another-dataframe-at-a-click-of-actionbutton/51385
  # renderFormattable : https://stackoverflow.com/a/34813547/1649726