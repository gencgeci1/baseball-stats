#' Calculate the earned run average for a pitcher.
#'
#' @param er The number of total earned runs let up by the pitcher.
#' @param inn The total number of innings the pitcher has pitched.
#' @return Average amount of earned runs let up by a pitcher per 9 innings pitched.
#' @examples
#' era(18, 197)
#' @export
era = function(er, inn) {
  ravg = (er * 9) / inn
  return(ravg)
}

#' Calculate the average number of hits and walks let up by a pitcher per inning.
#'
#' @param bb The number of walks let up by a pitcher.
#' @param h The number of hits let up by a pitcher.
#' @param inn The total number of innings the pitcher pitched.
#' @return A pitcher's WHIP for a specified number of walks, hits, and innings.
#' @examples
#' whip(84, 341, 458)
#' @export
whip = function(bb, h, inn) {
  whip = (bb + h) / inn
  return(whip)
}

#' Calculate the percentage of times a pitcher wins the game he/she is pitching in.
#'
#' @param w The total number of wins by a pitcher
#' @param l The total number of losses by a pitcher.
#' @return The ratio of wins by a pitcher to the total number of appearences.
#' @examples
#' pitch_win(84, 13)
#' @export
pitch_win = function(w, l) {
  win_p = w/(w+l)
  return(win_p)
}

#' Calculate the ratio of successful defensive plays to the total number of opportunities a player has to make a play.
#'
#' @param pos The number of putouts by a player.
#' @param ass The number of assists by a player.
#' @param err The number of errors committed by a player.
#' @return The ratio of plays made to the opportunites to make a play given a specified number of putouts, assists, errors.
#' @examples
#' field_avg(210, 56, 11)
#' @export
field_avg = function(pos, ass, err){
  fa = (pos + ass) / (pos + ass + err)
  return(fa)
}
