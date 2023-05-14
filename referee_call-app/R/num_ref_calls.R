num_ref_calls = function(data) {
  data %>% 
    mutate(home_count = 'HomeFouls' + 'HomeYellowCards' + 'HomeRedCards') %>% 
    mutate(away_count = 'AwayFouls' + 'AwayYellowCards' + 'AwayRedCards') %>% 
    select(home_count, away_count)
}