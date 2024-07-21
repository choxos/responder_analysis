#' Launch Responder Analysis App
#'
#' This function launches the Responder Analysis Shiny application.
#'
#'
#' @import shiny
#' @import shinydashboard
#' @import DT
#' @export
launch_responder_analysis <- function() {
        app_dir <- system.file("shiny-apps", "ResponderAnalysisApp", package = "ResponderAnalysisApp")
        if (app_dir == "") {
                stop("Could not find example directory. Try re-installing `ResponderAnalysisApp`.", call. = FALSE)
        }
        
        shiny::runApp(app_dir, display.mode = "normal")
}