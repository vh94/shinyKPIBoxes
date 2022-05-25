#' @name Testapp
#' @title Testapp to show functionality
#' @importFrom datasets mtcars
#'
#'
#'
ui <- dashboardPage(
  dashboardHeader(title = "KPI-Boxes-modules"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    h2("KPI using the module"),
    KPI_infobox_UI("casedata"),
    h2("cars"),
    wellPanel(style = "background: white",
              uiOutput("boxlist_UI"))
  )
)

server <- function(input, output) {
  KPIlist <- as.list(c("Cylinders" = table(mtcars$cyl)))
  output$boxlist_UI <- renderUI({
    rendervalueBoxfromList(KPIlist, "car", "blue")
  })


  cdata <- reactive({
    read.csv("./casedata.csv")
  })
  KPI_infobox_Server("casedata", data = cdata, panel = "case")

}

shinyApp(ui, server)
