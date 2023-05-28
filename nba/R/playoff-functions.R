#' Play all rounds for an individual conference
#'
#' @param qualified a character vector of team slugs of conference qualifiers
#' @param nba_stats data.frame of nba stats data
#'
#' @return a list of length 1 containg the name slug of the round winner. Match logs
#' for each round are also compiled and assigned to attribute `match_logs` of the output.
play_conference <- function(qualified, nba_stats) {
  round_1 <- play_round(
    qualified,
    nba_stats
  )
  semis <- play_round(
    round_1,
    nba_stats
  )
  finals <- play_round(
    semis,
    nba_stats
  )
  return(finals)
}
#' Play a round of matches. Should either be used for matches of teams in a single
#' conference or for the play-offs final.
#'
#' @param teams list of character strings of team slugs of all teams competing in round.
#' @param nba_stats data.frame of nba stats data
#' @param round_name Round name. If `NULL` (default), round name is auto-detected
#' by number of competing teams. For play-off final should be `"Play off Finals"`.
#' @param seed seed to set for lapply function.
#'
#' @return returns a list of the round winners of each match. Match logs are also
#' compiled and appended to the input's logs as attribute `match_logs` of the output
play_round <- function(teams, nba_stats, round_name = NULL, seed = TRUE) {

  # Detect round name from number of teams
  if (is.null(round_name)) {
    round_id <- as.character(length(teams))

    round_name <- switch(round_id,
      "8" = "Conference Round 1",
      "4" = "Conference Semi finals",
      "2" = "Conference finals"
    )
  }
  if (round_name == "Play off Finals") {
    conf <- NA
    conf_msg <- ""
  } else {
    conf <- unique(nba_stats[nba_stats$slug_team %in% unlist(teams), ]$id_conference)
    conf_msg <- paste0("(conf ", conf, ") ")
  }
  # Signal start of round
  cli::cli_h2("{round_name} {conf_msg}started!")

  # Create list of round pair match ups
  round_pairs <- draw_match_pairs(unlist(teams))

  # Use lapply to play each match
  set.seed(seed, kind = "L'Ecuyer-CMRG")
  round_winners <- lapply(
    X = round_pairs,
    FUN = play_match,
    nba_stats,
    round_name
  )

  # Compile match logs and append them to input match logs attribute
  attr(round_winners, "match_logs") <- rbind(
    attr(teams, "match_logs"),
    compile_match_logs(round_winners)
  )

  # Signal round completion and return results
  cli::cli_h2("{round_name} {conf_msg}COMPLETE!")

  return(round_winners)
}


#' Play a single match
#'
#' @param matchup a character vector of length 2 containing the name slugs of teams
#' competing.
#' @param nba_stats data.frame of nba stats data
#' @param round_name Character string. Round name.
#'
#' @return a list of length 1 containg the name slug of the round winner. Match logs are also
#' appended as attribute `match_logs`.
play_match <- function(matchup, nba_stats, round_name) {
  if (round_name == "Play off Finals") {
    conf <- NA
    conf_msg <- ""
  } else {
    conf <- unique(nba_stats[nba_stats$slug_team %in% matchup, ]$id_conference)
    conf_msg <- paste0("(conf ", conf, ") ")
  }


  # Create list of probabilities for sampling game length
  probs <- list(
    c(0.9452, 0.0481, 0.0057, 6e-04, 4e-04),
    c(0.7007, 0.1452, 0.0902, 0.0413, 0.0225)
  )
  # Assign higher probabilities for longer games to conference 2
  if (length(conf) > 1) {
    prob <- probs[[1]]
  } else {
    prob <- probs[[conf]]
  }

  pid <- Sys.getpid()
  node <- system2("hostname", stdout = TRUE)

  # play game
  cli::cli_h3("Playing {round_name} {conf_msg}game: {.var {matchup[1]}} VS {.var {matchup[2]}}")
  cli::cli_alert_info("Game location: {.val {pid}} ({node})")

  # Sample game length
  game_length <- sample(c(2.40, 2.65, 2.90, 3.15, 3.40), 1,
    prob = prob
  )
  # Send system to sleep to simulate playing match
  Sys.sleep(game_length)

  # SAMPLE WINNER from probability of winning stats
  # subset nba_stats to only team matchup data
  match_df <- nba_stats[nba_stats$slug_team %in% matchup, ]
  # sample winner
  winner <- sample(match_df$slug_team, 1, prob = match_df$prop_win)


  # print messages
  cli::cli_alert_info("{matchup[1]} VS {matchup[2]} match complete in {game_length * 50} minutes")
  cli::cli_alert_success("Winner: {winner}")

  # Compile match information into match logs data.frame and append as attribute.
  match_logs <- data.frame(
    winner = winner,
    team_1 = matchup[1],
    team_2 = matchup[2],
    pid = pid,
    node = node,
    game_length = game_length * 50,
    date = Sys.time(),
    conf = conf,
    round_name = round_name
  )
  attr(winner, "match_logs") <- match_logs

  return(winner)
}
#' Play Conference qualifiers
#'
#' @param teams a character vector of team slugs of teams competing in qualifiers
#'
#' @return a character vector of team slugs of qualified teams.
play_qualifiers <- function(teams) {
  conf <- unique(teams$id_conference)
  # play season
  cli::cli_h2("Playing qualifiers for conference {.val {conf}} on {.var {Sys.getpid()}}")
  Sys.sleep(5)
  # sample qualifiers
  qualified <- sample(teams$slug_team, 8, prob = teams$prop_win)

  cli::cli_alert_success("Conference {.val {conf}} qualifying round complete")

  return(qualified)
}
#' Draw pair matches.
#'
#' @param teams a character vector of team slugs of teams competing in round
#'
#' @return a list half the size of `teams` containing match pairs.
draw_match_pairs <- function(teams) {
  n_teams <- length(teams)
  matches <- sample(rep(1:(n_teams / 2), each = 2), size = n_teams)
  match_pairs <- split(teams, matches)
  return(match_pairs)
}
# Compile and order match_logs from lists of results
compile_match_logs <- function(x) {
  logs_list <- lapply(x, function(x) {
    attr(x, "match_logs")
  })
  logs <- do.call(rbind, logs_list)
  logs[order(logs$date), ]
}
