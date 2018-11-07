library(tidyverse)
X361_xlog <- read_table2("~/Documents/nethack_dumps/361_xlog.csv", col_names = FALSE)

clean_xlog <- function(xlog){
    # Column names
    colnames(xlog) <- c("version", "points", "deathdnum", "deathlev", "maxlvl", "hp", "maxhp",
                        "deaths", "deathdate", "birthdate", "uid", "role", "race", "gender", "align",
                        "name", "death", "conduct", "turns", "achieve", "realtime",  "starttime",
                        "endtime",  "gender0", "align0", "flags")

    # remove "blabla=" strings from each cell
    # and startscummer runs, by removing games that lasted less than 30 turns or that ended in escaped
    xlog %>%
        map_df(~str_remove(., ".*=")) %>%
        mutate(points = as.numeric(points),
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
        filter(death != "escaped", turns > 30) %>%
        select(-version, -uid, -conduct, -achieve, -flags)
}
nethack <- clean_xlog(X361_xlog)

url <- "https://s3.amazonaws.com/altorg/dumplog/{name}/{starttime}.nh361.txt"

nethack <- nethack %>%
 mutate(dumplog = map2(.x = name,
                       .y = starttime,
                       ~possibly(read_lines, otherwise = NA)(paste0("https://s3.amazonaws.com/altorg/dumplog/",
                                                                    .x, "/", .y, ".nh361.txt"))))
# nethack <- nethack %>%
#  filter(!is.na(dumplog))
#
# saveRDS(nethack, "~/Documents/nethack361.rds")
#
# usethis::use_data(nethack)
