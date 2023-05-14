library(tidyverse)

url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv'
soccer = read_csv(url)
soccer = soccer %>% 
  select(Referee, Date:FTR, HF, HY, HR, AF, AY, AR) %>% 
  rename(HomeFouls = 'HF') %>%
  rename(AwayFouls = 'AF') %>%
  rename(HomeYellowCards = 'HY') %>% 
  rename(AwayYellowCards = 'AY') %>% 
  rename(HomeRedCards = 'HR') %>% 
  rename(AwayRedCards = 'AR') %>% 
  rename(HomeGoals = 'FTHG') %>% 
  rename(AwayGoals = 'FTAG') %>% 
  rename(Result = 'FTR')
  
  
write_csv(x = soccer, file = "data/soccer.csv")

ref_call_order = c(
  "HomeFouls",
  "HomeYellowCards",
  "HomeRedCards",
  "AwayFouls",
  "AwayYellowCards",
  "AwayRedCards"
)


soccer %>% 
  filter(Referee == 'M Oliver') %>% 
  filter(HomeTeam == 'Man City') %>%
  filter(AwayTeam == 'Man United') %>% 
  pivot_longer(HomeFouls:AwayRedCards, names_to = "Referee_Call", values_to = "Count") %>%
  group_by("Referee Call") %>% 
  mutate(Referee_Call = factor(Referee_Call, levels = ref_call_order)) 
# %>% 
#   mutate(home_total = HomeFouls + HomeYellowCards + HomeRedCards) %>% 
#   mutate(away_total = AwayFouls + AwayYellowCards + AwayRedCards)
  # ggplot() +
  # aes(x = Referee_Call, y = Count, fill = Count) %>% 
  # geom_bar(stat = "identity") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  # scale_y_continuous(n.breaks = 10) +
  # theme_bw()

