#' Get the dumplog of a NetHack 3.6.1 run
#' @param xlog A data frame containing cleaned xlog data
#' @return A data frame with the dumplog column added to the input data frame
#' @importFrom dplyr mutate
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2 possibly
#' @importFrom readr read_lines
#' @export
#' @examples
#' \dontrun{
#' get_dumplog(xlog)
#' }
get_dumplog <- function(xlog){
    dplyr::mutate(xlog, dumplog = purrr::map2(.x = name,
                                              .y = starttime,
                                              ~purrr::possibly(readr::read_lines, otherwise = NA)(
                                                  paste0("https://s3.amazonaws.com/altorg/dumplog/",
                                                         .x, "/", .y, ".nh361.txt")
                                              )))
}

