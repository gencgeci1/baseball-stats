#' Calculate the batting average of a player.
#'
#' @param h The number of hits of a player.
#' @param abs The number of official at bats of player.
#' @return The ratio of hits a player has given a certain number of opportunuities to get a hit (batting average).
#' @examples
#' ba(25, 100)
#' @export
ba = function(h, abs) {
  avg = h/abs
  return(avg)
}

#' Calculate the slugging percentage of a player.
#'
#' @param s The number of singles hit by a player.
#' @param d The number of doubles hit by a player.
#' @param t The number of triples hit by a player.
#' @param hr The number of homeruns bit by a player.
#' @param abs The number of official at bats for a player.
#' @return The total number of bases hit by a player over a set number of at bats.
#' @examples
#' slg(52, 25, 2, 8. 136)
#' @export
slg = function(s, d, t, hr, abs) {
  slgp = (s + (d * 2) + (t * 3) + (hr * 4)) / abs
  return(slgp)
}

#' Calculate the on-base percentage of a player.
#'
#' @param h The number of hits by player.
#' @param bb The number of walks let up to a player.
#' @param hpb The number of times a player was hit by a pitch.
#' @param abs The number of official at bats for a player.
#' @return The ratio of the number of times a batter reaches base safely over the total number of at bats.
#' @examples
#' obp(86, 18, 3, 350)
#' @export
obp = function(h, bb, hbp, abs) {
  obperc = (h + bb + hbp) / (abs + bb + hbp)
  return(obperc)
}

#' Calculate the number of runs created by player.
#'
#' @param h The number of hits by a player.
#' @param bb The number of walks let up to a player.
#' @param tb The total bases obtanied by a player. (Weighted hits for singles, doubles, triples, and homeruns)
#' @param abs The official number of at bats for a player.
#' @return Number of runs a player contributes to the team given number of hits, walks, total bases and at bats.
#' @examples
#' rc(145, 58, 315, 526)
#' @export
rc = function(h, bb, tb, abs) {
  created = ((h + bb) * tb) / (abs + bb)
  return(created)
}
