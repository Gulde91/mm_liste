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
  startdato <- as.Date("2018-07-20")
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
  
  # oberve tilmeld/afmeld event
  observeEvent(input$ok_til_afm, { 
    
    if (input$til_afm_per == 'Afmeld') {
      
      if(input$afmeld_nu == T & input$afmeld_midl == T) {
        showModal(til_afm_model(failed = TRUE))
      }
      
      else if (input$afmeld_midl == F) {

        morgen_orig <<- subset(morgen_orig, Person != input$afmeld_pers)

        idx_afmeld <<- which(medlemmer %in% input$afmeld_pers)
        medlemmer <<- medlemmer[-idx_afmeld]

        #afm_pers_m2_index <<- which(morgen2$Person == input$afmeld_pers) # kan denne linje slettes?
        morgen2 <<- subset(morgen2, Person != input$afmeld_pers)
        morgen2$Dato <<- seq(morgen$Dato[nrow(morgen)] + dato_interval, 
                             morgen$Dato[nrow(morgen)] + nrow(morgen2) * dato_interval,
                             by = dato_interval)

        if(input$afmeld_nu == T) {
          if (input$afmeld_pers %in% morgen$Person) {
            #afm_akt_index <<- which(morgen$Dato == as.Date(akt_dato)) # sys.Date() kode til rettes til uge-basis
            afm_akt_index <<- which(morgen$Dato %in% seq(akt_dato, akt_dato + dato_interval - 1,  by = 1))

            if (sum(morgen$Person == input$afmeld_pers) == 1) {
              if (morgen$Person[afm_akt_index] == input$afmeld_pers) {
                showModal(til_afm_model(failed = TRUE))
              }
              else if (which(morgen$Person == input$afmeld_pers) < afm_akt_index) {
                showModal(til_afm_model(failed = TRUE))
              }

              else {
                afm_pers_index <- which(morgen$Person == input$afmeld_pers)

                morgen <<- subset(morgen, Person != input$afmeld_pers)

                if (afm_pers_index != nrow(morgen) + 1) {
                  morgen$Dato[(afm_pers_index):nrow(morgen)] <<- morgen$Dato[(afm_pers_index):nrow(morgen)] - dato_interval
                }

                morgen2$Dato <<- morgen2$Dato - dato_interval

                removeModal()
              }
            }
            else if (sum(morgen$Person == input$afmeld_pers) == 2) {
              afm_pers_index <<- which(morgen$Person == input$afmeld_pers)

              if (morgen$Person[afm_akt_index] == input$afmeld_pers) {
                showModal(til_afm_model(failed = TRUE))
              }
              else if (afm_pers_index[1] < afm_akt_index & afm_pers_index[2] < afm_akt_index) {
                showModal(til_afm_model(failed = TRUE))
              }
              else if (afm_pers_index[1] < afm_akt_index & afm_pers_index[2] > afm_akt_index) {

                afm_pers_index_2  <- which(morgen$Person == input$afmeld_pers)[2]
                morgen <<- morgen[-afm_pers_index_2,]

                if (afm_pers_index_2 != nrow(morgen) + 1) {
                  morgen$Dato[(afm_pers_index_2):nrow(morgen)] <<- morgen$Dato[(afm_pers_index_2):nrow(morgen)] - dato_interval
                }

                morgen2$Dato <<- morgen2$Dato - dato_interval
                removeModal()
              }
              else if (afm_pers_index[1] > afm_akt_index & afm_pers_index[2] > afm_akt_index) {

                morgen <<- morgen[-afm_pers_index,]
                morgen$Dato <<- seq(morgen$Dato[1], morgen$Dato[1] + nrow(morgen) - dato_interval, by = 1)

                morgen2$Dato <<- morgen2$Dato - 2 * dato_interval
                removeModal()
              }
            }
            else {
              showModal(til_afm_model(failed = TRUE))
            }

          }
          else {
            showModal(til_afm_model(failed = TRUE))
          }

        } else {
          removeModal()
        }
       } 
      # else if (input$afmeld_midl == T) {
      #   if (input$afmeld_pers %in% morgen$Person) {
      #     afmeld_midl_index <- which(morgen$Dato == as.Date(akt_dato)) # sys.Date() kode til rettes til uge-basis
      #     
      #     if (morgen$Person[afmeld_midl_index] == input$afmeld_pers) {
      #       showModal(til_afm_model(failed = TRUE))
      #     }
      #     else if (which(morgen$Person == input$afmeld_pers) < afmeld_midl_index) {
      #       showModal(til_afm_model(failed = TRUE))
      #     }
      #     else {
      #       afmeld_person_midl_index <- which(morgen$Person == input$afmeld_pers)
      #       
      #       morgen <<- subset(morgen, Person != input$afmeld_pers)
      #       
      #       if (afmeld_person_midl_index != nrow(morgen)+1) {
      #         morgen$Dato[(afmeld_person_midl_index):nrow(morgen)] <<- morgen$Dato[(afmeld_person_midl_index):nrow(morgen)] - 1  
      #       }
      #       
      #       morgen2$Dato <<- morgen2$Dato - 1
      #       
      #       removeModal()
      #     }
      #   }
      #   else {
      #     showModal(til_afm_model(failed = TRUE))
      #   }
      #   
      #   
      # }
    }
    
    # if (input$til_afm_per == 'Tilmeld') {
    #   if (!(tolower(trimws(input$Tilmeld_pers)) %in% tolower(trimws(morgen_orig$Person)))) {
    #     
    #     morgen_orig <<- rbind(morgen_orig, input$Tilmeld_pers) %>% arrange(Person)
    #     medlemmer <<- c(medlemmer, input$Tilmeld_pers) %>% sort()
    #     
    #     tmp_m2_idx <- c(morgen2$Person, input$Tilmeld_pers) %>% sort()
    #     tmp_m2_idx <- which(tmp_m2_idx %in% input$Tilmeld_pers)
    #     
    #     if (tmp_m2_idx == 1) {
    #       morgen2 <<- rbind(data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx]),
    #                         morgen2)
    #       morgen2$Dato[2:nrow(morgen2)] <<- morgen2$Dato[2:nrow(morgen2)] + 1
    #     }
    #     else if (tmp_m2_idx == nrow(morgen2) + 1) {
    #       morgen2 <<- rbind(morgen2,
    #                         data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx-1] + 1))
    #     } 
    #     else {
    #       morgen2 <<- rbind(morgen2[1:(tmp_m2_idx-1),],
    #                         data.frame(Person = input$Tilmeld_pers, Dato = morgen2$Dato[tmp_m2_idx]),
    #                         morgen2[tmp_m2_idx:nrow(morgen2),])
    #       morgen2$Dato[(tmp_m2_idx+1):nrow(morgen2)] <<- morgen2$Dato[(tmp_m2_idx+1):nrow(morgen2)] + 1
    #     }
    #     
    #     if(input$tilmeld_nu == T) {
    #       if(!(input$Tilmeld_pers %in% morgen$Person)) {
    #         #which(morgen$Dato %in% seq(dags_dato-7, as.Date(akt_dato), by = 'day'))
    #         
    #         dato_input <- format(morgen$Dato[nrow(morgen)] + 1, format = '%Y-%m-%d') # + 6
    #         
    #         morgen <<- rbind(morgen, c(Person = input$Tilmeld_pers, Dato = dato_input)) 
    #         morgen2$Dato <<- morgen2$Dato + 1
    #         
    #         removeModal()
    #         
    #       } else {
    #         showModal(til_afm_model(failed = TRUE))
    #       }
    #     }
    #     if(input$tilmeld_nu == F) {
    #       removeModal()
    #     }  
    #   }
    #   else {
    #     showModal(til_afm_model(failed = TRUE))
    #   }
    # }
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

