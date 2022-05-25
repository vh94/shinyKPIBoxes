#' @name KPI_infobox_UI
#' @title UI function of module to render SORMAS-Stats Specific KPI valueBoxes
#' @param id Namespace id
#' @return A list of rendered HTML div’s of valueBoxes for SORMAS-Stats
KPI_infobox_UI <- function(id) {
  ns <- NS(id)
  wellPanel(style = "background: white",
            uiOutput(ns("boxlist_UI"))
  )
}

#' @name KPI_infobox_Server
#' @title Server function of module to render SORMAS-Stats Specific KPI valueBoxes
#' @param data reactive data you want to display (casedata,eventdata or contactdata)
#' @param panel set to which type of KPI's you want to render ("contact","case","event"), defines ifelse logic
#' @return A list of shiny.tag's containing valueBoxes for SORMAS-Stats
#'
KPI_infobox_Server <-
  function(id,data,panel) {
    stopifnot(is.reactive(data))
    moduleServer(id,
        function(input, output, session) {
            options(digits=2) # round to tow digits
                output$boxlist_UI <-
                  renderUI({
                    if (panel=="contact") {
                        icon  = "handshake"
                        color = "light-blue"
                        idData<-reactive({
                           data()$caze_id |> as.factor() |>  summary() |>  as.table() |>  data.frame()
                        })
                        ### build KPI list
                        KPI <- as.list(
                          c(
                            "All contacts" = nrow(data()),
                            table(data()$contactclassification),
                            table(data()$contactstatus),
                            "Min cont-per-case" = min(idData()$Freq),
                            "Med cont-per-case" = median(idData()$Freq),
                            "Mean cont-per-case" = round(mean(idData()$Freq)),
                            "Max cont-per-case" = max(idData()$Freq)
                           )
                         )
                         rendervalueBoxfromList(KPI,icon,color)
                     }
                    else if(panel=="event") {
                       icon  = "cog"
                       color = "green"
                       ### build KPI list
                       KPI <- as.list(
                         c(
                           "Total Events" = nrow(data()),
                           "Event Participants"=sum(data()$eventPart_sum),
                           "Resulting cases"=sum(data()$resulting_case_sum),
                           table(data()$eventstatus),
                           table(data()$eventmanagementstatus)
                         )
                       )
                       rendervalueBoxfromList(KPI,icon,color)
                     }
                    else if(panel=="case") {
                       icon  = "procedures"
                       color = "red"
                       ### build KPI lists:
                       KPI_class <- as.list(c("All cases" = nrow(data()),table(data()$caseclassification)))
                       KPI_quar <- as.list(c(table(data()[!is.na(data()$quarantine), ]$quarantine),
                           "Missing" = nrow(data()[is.na(data()$quarantine), ])))
                       KPI_sex <- as.list(c( table(data()[!is.na(data()$sex),]$sex),
                                    "Missing sex" = nrow(data()[is.na(data()$sex),] )))
                       KPI_age <- as.list(c(
                         "Min age" = min(data()$age[!is.na(data()$age) ]),
                         "Median age" = median(data()$age[!is.na(data()$age)]),
                         "Mean age" = round(mean(data()$age[!is.na(data()$age) ])),
                         "Max age" = max(data()$age[!is.na(data()$age) ]),
                         "Missing age" = nrow(data()$age[is.na(data()$age) ])
                       ))
                       KPI_outcome<-as.list(c(table(data()$outcome)))
                       KPI_occupation<-as.list(c(
                         table(data()[!is.na(data()$occupationtype), ]$occupationtype),
                         "Missing" = nrow(data()[!is.na(data()$occupationtype), ])
                       ))
                       ### render KPI lists and list them with headers:
                       list(
                          h3("case classification"), rendervalueBoxfromList(KPI_class,icon,color),
                          h3("case quarantine"), rendervalueBoxfromList(KPI_quar,icon,color),
                          h3("case gender"), rendervalueBoxfromList(KPI_sex,icon,color),
                          h3("case age"), rendervalueBoxfromList(KPI_age,icon,color),
                          h3("case outcome"), rendervalueBoxfromList(KPI_outcome,icon,color),
                          h3("case occupation"), rendervalueBoxfromList(KPI_occupation,icon,color)
                             )

                     }
                   })
                 }
        )
  }


