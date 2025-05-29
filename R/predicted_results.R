predicted_results <- function(df){

  df <- df %>%
    mutate(team_name = str_to_title(gsub("_", " ", team_name)),
           opponent = str_to_title(gsub("_", " ", opponent))) %>%
    select(Round = round,
           `Home Team` = team_name,
           `Away Team` = opponent,
           `Goals Home` = gf,
           `Goals Away` = ga,
           `Real result` = result,
           `P (Home win)` = home_win,
           `P (Draw)` = draw,
           `P (Home lost)` = home_lost,
           `E(Goals Home)` = lambda_home,
           `E(Goals Away)` = lambda_away)

  df <- df[sample(1:nrow(df),10),]

  return(df %>%
    flextable() %>%
    align(align = "center", part = "all") %>%
    fix_border_issues() %>%
    fontsize(part = 'all', size = 8))
}
