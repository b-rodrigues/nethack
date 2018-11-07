library("tidyverse")
library("rvest")


scrape_one_day <- function(link){

    convert_to_seconds <- function(time_string){
        time_numeric <- time_string %>%
            str_split(":", simplify = TRUE) %>%
            as.numeric

        time_in_seconds <- time_numeric * c(3600, 60, 1)

        if(is.na(time_in_seconds)){
            time_in_seconds <- 61
        } else {
            time_in_seconds <- sum(time_in_seconds)
        }
        return(time_in_seconds)
    }


    Sys.sleep(1)

    date <- str_extract(link, "\\d{8}")

    read_lines_slow <- function(...){
        Sys.sleep(1)
        read_lines(...)
    }

    page <- read_html(link)

    # Get links
    dumplogs <- page %>%
        html_nodes(xpath = '//*[(@id = "perday")]//td') %>%
        html_children() %>%
        html_attr("href") %>%
        keep(str_detect(., "dumplog"))

    # Get table
    table <- page %>%
        html_node(xpath = '//*[(@id = "perday")]') %>%
        html_table(fill = TRUE)

    #print(table)
    if(is_empty(dumplogs)){
        print("dumplogs empty")
        dumplogs <- rep(NA, nrow(table))
    } else {
        dumplogs <- dumplogs
    }

    final <- table %>%
        janitor::clean_names() %>%
        mutate(dumplog_links = dumplogs)

    print(paste0("cleaning data of date ", date))

    clean_final <- final %>%
        select(-x) %>%
        rename(role = x_2,
               race = x_3,
               gender = x_4,
               alignment = x_5) %>%
        mutate(time_in_seconds = map(time, convert_to_seconds)) %>%
        filter(!(death %in% c("quit", "escaped")), time_in_seconds > 60) %>%
        mutate(dumplog = map(dumplog_links, ~possibly(read_lines_slow, otherwise = NA)(.))) %>%
        mutate(time_in_seconds = ifelse(time_in_seconds == 61, NA, time_in_seconds))

    saveRDS(clean_final, paste0("datasets/data_", date, ".rds"))

}
#
# link <- "https://alt.org/nethack/gamesday.php?date="
#
# dates <- seq(as.Date("2018/01/01"), as.Date("2018/01/10"), by = "day") %>%
#     str_remove_all("-")
#
# links <- paste0(link, dates)
#
# map(links, ~possibly(scrape_one_day, otherwise = NULL)(.))
