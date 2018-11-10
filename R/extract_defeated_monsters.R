#' Extract information about defeated monsters from an xlogfile
#' @param xlog A raw xlogfile
#' @return A data frame with information on vanquished, genocided and extincted monsters
#' @importFrom dplyr mutate select filter bind_rows full_join
#' @importFrom tidyr separate
#' @importFrom tibble as_tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom purrr map2 possibly is_empty modify_if simplify discard
#' @importFrom readr read_lines
#' @importFrom stringr str_which str_replace_all str_replace str_trim str_detect str_to_lower str_extract_all str_extract
#' @export
#' @examples
#' \dontrun{
#' get_dumplog(xlog)
#' }
extract_defeated_monsters <- function(dumplog){

    if(any(str_detect(dumplog, "No creatures were vanquished."))){
        return(NA)
    } else {

        start <- dumplog %>% # <- dectect the start of the list
            str_which("Vanquished creatures")

        end <- dumplog %>% # <- detect the end of the list
            str_which("\\d+ creatures vanquished.")

        if(is_empty(end)){ # This deals with the situation of only one vanquished creature
            end <- start + 2
        }

        list_creatures <- dumplog[(start + 1):(end - 1)] %>% # <- extract the list
            str_replace_all("\\s+an? ", "1 ") %>% # <- replace a or an by 1
            str_trim() # <- trim white space

        # The following function first extracts the digit in the string (123 times)
        # and replaces the 1 with this digit
        # This means that: "1 the Wizard of Yendor (4 times)" becomes "4 the Wizard of Yendor (4 times)"
        str_extract_replace <- function(string){
            times <- str_extract(string, "\\d+(?=\\stimes)")
            str_replace(string, "1", times)
        }

        result <- list_creatures %>%
            # If a string starts with a letter, add a 1
            # This means that: "Baalzebub" becomes "1 Baalzebub"
            modify_if(str_detect(., "^[:alpha:]"), ~paste("1", .)) %>%
            # If the string "(twice)" is detected, replace "1" (that was added the line before) with "2"
            modify_if(str_detect(., "(twice)"), ~str_replace(., "1", "2")) %>%
            # Same for "(thrice)"
            modify_if(str_detect(., "(thrice)"), ~str_replace(., "1", "3")) %>%
            # Exctract the digit in "digit times" and replace the "1" with digit
            modify_if(str_detect(., "(\\d+ times)"), str_extract_replace) %>%
            # Replace "(times)" or "(twice)" etc with ""
            str_replace_all("\\(.*\\)", "") %>%
            str_trim() %>%
            simplify() %>%
            # Convert the resulting list to a tibble. This tibble has one column:
            # value
            # 1 Baalzebub
            # 2 dogs
            #...
            as_tibble() %>%
            # Use tidyr::separate to separate the "value" column into two columns. The extra pieces get merged
            # So for example "1 Vlad the Impaler" becomes "1" "Vlad the Impaler" instead of "1" "Vlad" which
            # would be the case without "extra = "merge""
            separate(value, into = c("value", "monster"), extra = "merge") %>%
            mutate(value = as.numeric(value)) %>%
            mutate(monster = str_to_lower(monster))

        # This function singularizes names:
        singularize_monsters <- function(nethack_data){
            nethack_data %>%
                mutate(monster = str_replace_all(monster, "mummies", "mummy"),
                       monster = str_replace_all(monster, "jellies", "jelly"),
                       monster = str_replace_all(monster, "vortices", "vortex"),
                       monster = str_replace_all(monster, "elves", "elf"),
                       monster = str_replace_all(monster, "wolves", "wolf"),
                       monster = str_replace_all(monster, "dwarves", "dwarf"),
                       monster = str_replace_all(monster, "liches", "lich"),
                       monster = str_replace_all(monster, "baluchiteria", "baluchiterium"),
                       monster = str_replace_all(monster, "homunculi", "homonculus"),
                       monster = str_replace_all(monster, "mumakil", "mumak"),
                       monster = str_replace_all(monster, "sasquatches", "sasquatch"),
                       monster = str_replace_all(monster, "watchmen", "watchman"),
                       monster = str_replace_all(monster, "zruties", "zruty"),
                       monster = str_replace_all(monster, "xes$", "x"),
                       monster = str_replace_all(monster, "s$", ""))
        }

        result <- singularize_monsters(result)
    }
    # If a player did not genocide or extinct any species, return the result:
    if(any(str_detect(dumplog, "No species were genocided or became extinct."))){
        result <- result %>%
            mutate(status = NA_character_)
        return(result)
    } else {

        # If the player genocided or extincted species, add this info:
        start <- dumplog %>% # <- dectect the start of the list
            str_which("Genocided or extinct species:") # <- sometimes this does not appear in the xlogfile

        end <- dumplog %>% # <- detect the end of the list
            str_which("Voluntary challenges")

       if(is_empty(start)){# This deals with the situation start does not exist
           start <- end - 2
       }

        list_creatures <- dumplog[(start + 1):(end - 1)] %>% # <- extract the list
            str_trim() # <- trim white space

        extinct_species <- list_creatures %>%
            str_extract_all("[:alpha:]+\\s(?=\\(extinct\\))", simplify = T) %>%
            str_trim %>%
            discard(`==`(., ""))

        extinct_species_df <- tibble(monster = extinct_species, status = "extinct")

        genocided_species_index <- list_creatures %>%
            str_detect(pattern = "extinct|species") %>%
            `!`

        genocided_species <- list_creatures[genocided_species_index]

        genocided_species_df <- tibble(monster = genocided_species, status = "genocided")

        genocided_or_extinct_df <- singularize_monsters(bind_rows(extinct_species_df, genocided_species_df))

        result <- full_join(result, genocided_or_extinct_df, by = "monster") %>%
            filter(monster != "") # <- this is to remove lines that were added by mistake, for example if start was empty

        return(result)
    }
}
