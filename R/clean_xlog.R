points <- deathdnum <- deathlev <- maxlvl <- hp <- maxhp <- deaths <- deathdate <- birthdate <- NULL
uid <- conduct <- turns <- achieve <- realtime <- starttime <- flags <- death <- name <- NULL
#' Clean extended log (xlog) files from NetHack 3.6.1.
#'
#' Clean extended log (xlog) files from NetHack 3.6.1. Adds column names, removes unneeded columns and
#' converts variables to their correct format.
#' @param xlog An xlog file
#' @return A data frame
#' @importFrom dplyr mutate select filter
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_df
#' @importFrom stringr str_remove
#' @export
#' @examples
#' \dontrun{
#' clean_xlog(xlog)
#' }
clean_xlog <- function(xlog){

    colnames(xlog) <- c("version", "points", "deathdnum", "deathlev", "maxlvl", "hp", "maxhp",
                        "deaths", "deathdate", "birthdate", "uid", "role", "race", "gender", "align",
                        "name", "death", "conduct", "turns", "achieve", "realtime",  "starttime",
                        "endtime",  "gender0", "align0", "flags")

    xlog %>%
        purrr::map_df(~stringr::str_remove(., ".*=")) %>%
        dplyr::mutate(points = as.numeric(points),
               deathdnum = as.numeric(deathdnum),
               deathlev = as.numeric(deathlev),
               maxlvl = as.numeric(maxlvl),
               hp = as.numeric(hp),
               maxhp = as.numeric(maxhp),
               deaths = as.numeric(deaths),
               deathdate = as.Date(deathdate, "%Y%m%d"),
               birthdate = as.Date(birthdate, "%Y%m%d"),
               uid = as.numeric(uid),
               conduct = as.numeric(conduct),
               turns = as.numeric(turns),
               achieve = as.numeric(achieve),
               realtime = as.numeric(realtime),
               starttime = as.numeric(starttime),
               flags = as.numeric(flags)) %>%
        dplyr::filter(death != "escaped", turns > 30) %>%
        dplyr::select(-version, -uid, -conduct, -achieve, -flags)
}
