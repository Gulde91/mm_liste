library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

#rm(list=ls())

# to do
# ændre til ugeligt i stedet for dagligt
# kun hvis næste liste hvis buttom trykkes
# ændre navne

# tester om git virker

if (!exists('medlemmer')) {
  medlemmer <- c('Alexander',
                 'Erik',
                 'Jenny',
                 'Rasmus',
                 'Hege',
                 'Rasmus',
                 'Kristian'
  )
}

morgen_orig <- data.frame(Person = medlemmer)
morgen_orig$Person <- as.character(morgen_orig$Person)

if (!exists('morgen')) {
  morgen <- data.frame(Person = medlemmer)
  morgen$Person <- as.character(morgen$Person)
  startdato <- as.Date("2018-07-19")
  #  morgen$Dato <- seq(startdato, startdato+((nrow(morgen)-1)*7), by = 'week')
  morgen$Dato <- seq(startdato, startdato+(nrow(morgen)-1), by = 'day')
}

if(!exists('morgen2')) {
  morgen2 <- data.frame(Person = medlemmer)
  morgen2$Person <- as.character(morgen2$Person)
  startdato <- morgen$Dato[nrow(morgen)] + 1 # + 6
  #  morgen2$Dato <<- seq(startdato, startdato+((nrow(morgen2)-1)*7), by = 'week')
  morgen2$Dato <- seq(startdato, startdato+(nrow(morgen2)-1), by = 'day')
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
  
})


shinyApp(ui=ui, server = server)

