#' NetHack runs data.
#'
#' Data on NetHack runs
#'
#' @format A data frame with 6000 rows and 23 variables extracted from xlogfiles of NetHack runs
#' on the public server \url{https://alt.org/nethack/}. The data goes from April to November 2018.
#' The original xlogfiles were kindly provided Pasi Kallinen on November 2018.
#' \describe{
#' \item{points}{The points the player achieved on that run}
#' \item{deathdnum}{The dungeon number the player died on:
#'
#' 0: The Dungeons of Doom
#'
#' 1: Gehennom
#'
#' 2: The Gnomish Mines
#'
#' 3: The Quest
#'
#' 4: Sokoban
#'
#' 5: Fort Ludios
#'
#' 6: Vlad's Tower
#'
#' 7: Elemental Planes
#' }
#' \item{deathlev}{The dungeon level the player died on}
#' \item{maxlvl}{The deepest dungeon level reached}
#' \item{hp}{The health points the player had when the game finished}
#' \item{maxhp}{The maximum health points the played had when the game finished}
#' \item{deaths}{The number of times the player died during the game}
#' \item{deathdate}{The day the game ended}
#' \item{birthdate}{The day the game started}
#' \item{role}{The role of the player}
#' \item{race}{The race the player}
#' \item{gender}{The gender the player when the game ended}
#' \item{align}{The alignement the player when the game ended}
#' \item{name}{The name of the player}
#' \item{death}{The reason the game ended}
#' \item{killed_while}{The player was killed while...}
#' \item{turns}{The number of turns taken until the game ended}
#' \item{realtime}{The length of the game in seconds}
#' \item{starttime}{Unix time of the start of the game}
#' \item{endtime}{Unix time of the end of the game}
#' \item{gender0}{The starting gender of the player}
#' \item{align0}{The starting alignment of the player}
#' \item{dumplog}{The dumplog of the run}
#' }
#' @source{ Dump from \url{https://alt.org/nethack/gamesday.php} provided by Pasi Kallinen on November 5th 2018}
"nethack"
