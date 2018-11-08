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
#' xlog <- read_delim("~/path/to/nethack361_xlog.csv",
#' "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
#' clean_xlog(xlog)
#' }
clean_xlog <- function(xlog){

    nowhilecol <- xlog %>%
        dplyr::filter(!str_detect(X18, "while"))

    whilecol <- xlog %>%
        dplyr::filter(str_detect(X18, "while"))

    colnames(nowhilecol) <- c("version", "points", "deathdnum", "deathlev", "maxlvl", "hp", "maxhp",
                        "deaths", "deathdate", "birthdate", "uid", "role", "race", "gender", "align",
                        "name", "death", "conduct", "turns", "achieve", "realtime",  "starttime",
                        "endtime",  "gender0", "align0", "flags")


    colnames(whilecol) <- c("version", "points", "deathdnum", "deathlev", "maxlvl", "hp", "maxhp",
                        "deaths", "deathdate", "birthdate", "uid", "role", "race", "gender", "align",
                        "name", "death", "killed_while", "conduct", "turns", "achieve", "realtime",  "starttime",
                        "endtime",  "gender0", "align0")

    cleaning <- . %>%
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
               endtime = as.numeric(endtime)) %>%
        dplyr::filter(death != "escaped", turns > 30) %>%
        dplyr::select(-version, -uid, -conduct, -achieve)

    nowhilecol <- nowhilecol %>%
        cleaning %>%
        select(-flags)

    whilecol <- whilecol %>%
        cleaning

    dplyr::full_join(nowhilecol, whilecol)

}
