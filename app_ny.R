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
                 'Rasmus',
                 'Kristian'
  )
}

morgen_orig <- data.frame(Person = medlemmer)
morgen_orig$Person <- as.character(morgen_orig$Person)

if (!exists('morgen')) {
  morgen <- data.frame(Person = medlemmer)
  morgen$Person <- as.character(morgen$Person)
  startdato <- as.Date("2018-07-06")
  morgen$Dato <- seq(startdato, startdato + (nrow(morgen) - 1) * dato_interval, by = dato_interval)
}

if(!exists('morgen2')) {
  morgen2 <- data.frame(Person = medlemmer)
  morgen2$Person <- as.character(morgen2$Person)
  startdato <- morgen$Dato[nrow(morgen)] + dato_interval
  morgen2$Dato <- seq(startdato, startdato + (nrow(morgen2) - 1) * dato_interval, by = dato_interval)
}

akt_dato <- Sys.Date()

server <- shinyServer(function(input, output, session) {
  
  ### TILMELD/AFMELD -----------------------------------------------
  # create tilføj_afmeld_model
  til_afm_model <- function(failed = FALSE) {
    modalDialog(easyClose = T,
                fade = T,
                title = 'Tilmed eller afmeld',
                
                selectInput(inputId = 'til_afm_per',
                            label = 'Vælg handling',
                            choices = list('Tilmeld',
                                           'Afmeld')
                ),
                
                conditionalPanel(
                  condition = "input.til_afm_per == 'Afmeld'",
                  selectInput(inputId = "afmeld_pers", 
                              label = 'Vælg person',
                              choices = medlemmer),
                  checkboxInput(inputId = "afmeld_nu", 
                                label = "Afmeld også fra nuværende liste", 
                                value = FALSE),
                  checkboxInput(inputId = "afmeld_midl", 
                                label = "Afmeld kun fra nuværende liste", 
                                value = FALSE)
                ),
                
                conditionalPanel(
                  condition = "input.til_afm_per == 'Tilmeld'",
                  textInput(inputId = "Tilmeld_pers", 
                            label = 'Tilmeld person',
                            value = ''),
                  checkboxInput(inputId = "tilmeld_nu", 
                                label = "Tilmeld nuværende liste", 
                                value = FALSE)
                ),
                
                if (failed) { 
                  if (input$til_afm_per == 'Tilmeld') {
                    div(tags$b(paste("Hov!", input$Tilmeld_pers, "findes jo allerede!"), style = "color: blue;"))
                  } 
                  else if (input$til_afm_per == 'Afmeld' & input$afmeld_midl == F & input$afmeld_nu == T) {
                    div(tags$b(paste("Hov!", input$afmeld_pers, "har enten haft kage med i denne omgang, skal 
                                     have kage med næste gang, eller findes ikke på den nuværende liste.", 
                                     input$afmeld_pers, "er afmeldt fra næste 'runde'."),
                               style = "color: blue;"))
                  }
                  else {
                    div(tags$b(paste("Hov! Der skete en fejl"),
                               style = "color: blue;"))
                  }
                },
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("ok_til_afm", "OK")
                )
                
                    )
                }
  
  # Show til_afm_model when button is clicked.
  observeEvent(input$til_afm, {
    showModal(til_afm_model())
  })
  
  
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


shinyApp(ui=ui, server = server)

