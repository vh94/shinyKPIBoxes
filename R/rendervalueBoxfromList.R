#' @name rendervalueBoxfromList
#' @title  render a list of shiny columns containing valueBoxes
#'
#' @importFrom shiny column
#' @import stringi
#' @importFrom shinydashboard valueBox
#' @importFrom purrr map
#'
#'
#' @param list List of named lists containing the values for the boxes
#' @param icon An <i> (icon) HTML tag.
#' @param color A color for the box. Valid colors are listed in validColors. help/library/shinydashboard/help/validColors
#' @return A list of shiny.tag's containing valueBoxes
#'
#'

rendervalueBoxfromList <- function(list,icon,color){
  # append cleaned list names to list as item
  mapply(
    append,list,
    gsub("_"," ",stringi::stri_trans_totitle(names(list),opts_brkiter = stri_opts_brkiter(type = "sentence"))),
    SIMPLIFY = FALSE
  ) |>
    # map over column & valueBox
    map( ~column(2, valueBox( .[[1]],.[[2]], icon(icon), color,width = NULL))) |> fluidRow()
}
