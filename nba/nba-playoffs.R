# Source function ----
source(here::here("nba", "R", "playoff-functions.R"))

# Load data ----
nba_stats <- readr::read_csv(
  here::here(
    "nba", "data",
    "nba_stats_summaries_19-21.csv"
  )
)


# Play qualifiers ----
tictoc::tic(msg = "Total Play-offs Duration")

cli::cli_h1("Qualifiers have begun!")
split_confs <- split(nba_stats,
  f = nba_stats$id_conference
)
set.seed(5, kind = "L'Ecuyer-CMRG")

qualified_confs <- lapply(
  split_confs,
  play_qualifiers
)

cli::cli_h2("ALL Qualifying matches COMPLETE!")

# Play Conference Rounds ----
cli::cli_h1("Conference Rounds have begun!")

conf_winners <- lapply(
  X = qualified_confs,
  FUN = play_conference,
  nba_stats
)

attr(conf_winners, "match_logs") <- compile_match_logs(conf_winners)
cli::cli_h2("ALL Conference matches COMPLETE!")

# Play Play-offs FINAL ----
cli::cli_h1("Overall Playoff final has begun!")

playoff_winner <- play_round(conf_winners,
  nba_stats,
  round_name = "Play off Finals",
  seed = 7
)

# Announce Winner!
winner_stats <- nba_stats[nba_stats$slug_team == playoff_winner, ]
cli::cli_h1("Playoffs complete!")
cli::cli_alert_success("Winner: {.field {winner_stats$name_team}} ({winner_stats$slug_team})")
tictoc::toc()

# Write match logs to csv ----
fs::dir_create(here::here("nba", "outputs"))
write.csv(attr(playoff_winner, "match_logs"),
  file = here::here("nba", "outputs", "playoff_results.csv"),
  row.names = FALSE
)
