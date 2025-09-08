#' Run the Shiny App
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shinypedidosecalc", package = "pedidosecalc")
  shiny::runApp(app_dir, display.mode = "normal")
}
