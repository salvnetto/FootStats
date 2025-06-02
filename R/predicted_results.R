predicted_results <- function(df){
  
  # Selecting relevant columns from test_data
  df <- df %>%
    arrange(round) %>%
    select(comp,
           season,
           date,
           round,
           team_name,
           opponent,
           result,
           gf,
           ga,
           home_win:lambda_away) %>%
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
  
  df <- df[c(2,7,8,16,22,24,25,28,36,39),]
  
  return(df %>%
           flextable() %>%
           align(align = "center", part = "all") %>%
           fix_border_issues() %>%
           fontsize(part = 'all', size = 8))
}
