#' Play YouTube moview at RStudioServer
#'
#' @param url url that you want show Toutube movie
#' @param width width of movie
#' @param height height of movie
#' @param background background color
#' @param viewer set viewer. See ?htmltools::html_print .
#'
#' @examples
#' tubeplay() # play default set movie
#' tubeplay("https://www.youtube.com/watch?v=0E00Zuayv9Q") # play "PPAP"
#' @export
tubeplay <- function(url = "https://www.youtube.com/watch?v=iOFZKwv_LfA", width = "560", height = "315",
                     background = "white", viewer = getOption("viewer", utils::browseURL)) {
  # make video_id
  video_id <- gsub("^.*\\?v=", "", url)
  # make_ui
  ui <- miniUI::miniPage(
    tags$iframe(width = width, height = height,
                src = paste("https://www.youtube.com/embed", video_id, sep = "/"),
                frameborder="0")
  )
  html_print(ui)
}

