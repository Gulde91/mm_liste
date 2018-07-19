library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

rm(list=ls())

# to do
# ændre til ugeligt i stedet for dagligt
# kun hvis næste liste hvis buttom trykkes
# ændre navne

dato_interval <- 7

if (!exists('medlemmer')) {
  medlemmer <- c('Alexander',
                 'Erik',
                 'Jenny',
                 #'Hege',
                 'Rasmus',
                 'Kristian'
  )
}

morgen_orig <- data.frame(Person = medlemmer)
morgen_orig$Person <- as.character(morgen_orig$Person)

if (!exists('morgen')) {
  morgen <- data.frame(Person = medlemmer)
  morgen$Person <- as.character(morgen$Person)
  startdato <- as.Date("2018-07-18")
  morgen$Dato <- seq(startdato, startdato + (nrow(morgen) - 1) * dato_interval, by = dato_interval)
}

if(!exists('morgen2')) {
  morgen2 <- data.frame(Person = medlemmer)
  morgen2$Person <- as.character(morgen2$Person)
  startdato <- morgen$Dato[nrow(morgen)] + dato_interval
  morgen2$Dato <- seq(startdato, startdato + (nrow(morgen2) - 1) * dato_interval, by = dato_interval)
}

akt_dato <- Sys.Date()

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(
                      title = 'DATAANALYSE'
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Morgenmadsliste', tabName = 'morgen', icon = icon('coffe')) # birthday-cake
                      )
                      
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'morgen',
                                h2('Morgenmad'),
                                hr(),
                                fluidRow(
                                  column(7,
                                         box(title = 'Morgenmadsliste', solidHeader = T, status = 'primary',
                                             DT::dataTableOutput('morgen'),
                                             width = 400
                                         ),
                                         actionButton('skip', 'Skip', icon = icon('forward')),
                                         actionButton('byt', 'Byt', icon = icon('handshake-o')),
                                         actionButton('til_afm', 'Tilmeld/afmeld', icon = icon('exchange')),
                                         actionButton('dato', 'dato+1')
                                  ),
                                  column(5,
                                         box(title = 'Næste liste', solidHeader = T, status = 'primary',
                                             DT::dataTableOutput('morgen2'),
                                             width = 400
                                         )
                                  )
                                )
                        )
                      )
                    )
)

server <- shinyServer(function(input, output, session) {
  
  ### OUTPUT TABEL ----------------------------------
  output$morgen <- DT::renderDataTable({
    input$ok_skip
    input$ok_byt
    input$ok_til_afm
    
    index <- which(morgen$Dato %in% seq(akt_dato, akt_dato + dato_interval - 1, by = 'day'))
    #index <<- which(morgen$Dato == as.Date(akt_dato))
    
    if(length(index) == 0) {
      
      # gemmer den gamle liste
      morgen_gl <<- morgen
      
      # morgen2 bliver nu morgen
      morgen <<- morgen2 
      
      index <- which(morgen$Dato %in% seq(akt_dato, akt_dato + dato_interval - 1, by = dato_interval))
      
      # konstruerer næste liste, morgen2
      startdato2 <- morgen$Dato[nrow(morgen)] + dato_interval
      morgen2 <<- morgen_orig 
      morgen2$Dato <<- seq(startdato2, startdato2 + (nrow(morgen2) - 1) * dato_interval, by = dato_interval)
      
    }
    
    morgen$Dato <- format(morgen$Dato, format = '%A %d. %B')
    morgen2$Dato <- format(morgen2$Dato, format = '%A %d. %B')
    
    morgen_output <<- rbind(morgen, morgen2)
    
    DT::datatable(morgen_output[(index-1):(index+5),], 
                  options = list(paging = F, 
                                 searching = F, 
                                 bInfo = F), 
                  rownames= FALSE) %>% formatStyle(
                    'Dato',
                    target = 'row',
                    backgroundColor = styleEqual(morgen_output$Dato[index], c('lightblue'))
                  )
    #https://rstudio.github.io/DT/010-style.html
  })
  
  
  ### OUTPUT TABEL2 ----------------------------------
  output$morgen2 <- DT::renderDataTable({
    input$ok_skip
    input$ok_til_afm
    input$ok_byt
    
    morgen2$Dato <- format(morgen2$Dato, format = '%A %d. %B')
    
    DT::datatable(morgen2, 
                  options = list(paging = F, 
                                 searching = F, 
                                 bInfo = F), 
                  rownames = FALSE)
  })
  
  
})


shinyApp(ui=ui, server = server)

