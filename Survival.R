# Analysis of bills' survival past session cutoff gates

library(tidyverse)
library(ggthemes)
library(scales)

downloadData()

Bills %>%
  group_by(Chamber, BillNumber) %>%
  summarize_at(vars(starts_with('Survived')), max) %>%
  group_by(Chamber) %>%
  summarize_at(vars(starts_with('Survived')), mean) %>%
  gather(key='SurvivalType', value='SurvivalPct', -Chamber) %>%
  mutate(SurvivalType=case_when(
    SurvivalType=='SurvivedPolicyCutoff' ~ 'Policy Committee',
    SurvivalType=='SurvivedFiscalCutoff' ~ 'Fiscal Committee',
    SurvivalType=='SurvivedHouseOfOriginCutoff' ~ 'House of Origin'
  )) %>%
  mutate(SurvivalPctLabel=percent(SurvivalPct)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(SurvivalType, SurvivalPct), y=SurvivalPct), stat='identity') +
  geom_text(aes(x=reorder(SurvivalType, SurvivalPct), y=SurvivalPct, label=SurvivalPctLabel), hjust='right', nudge_y = -.02, color='white') +
  coord_flip() +
  scale_y_continuous(labels=percent) +
  facet_grid(cols=vars(Chamber)) +
  theme_hc() +
  labs(x=NULL, y=NULL, title='Rates of Bill Survival in the 2019 Session of the Washington Legislature',
       subtitle='Percent of bills clearing each cutoff, as of day 62 of the session (March 16)',
       caption=paste0('A bill counts as "surviving" if any version (substitute, engrossed, etc.) survives the cutoff\n',
                      'Legislative vote data from Washington State Legislature, available at https://data.world/scottcame/washington-legislature-2019'))

Bills %>%
  group_by(Chamber, PrimeSponsorID, BillNumber) %>%
  summarize_at(vars(starts_with('Survived')), max) %>%
  group_by(Chamber, PrimeSponsorID) %>%
  filter(n() > 10) %>%
  summarize_at(vars(starts_with('Survived')), mean) %>%
  gather(key='SurvivalType', value='SurvivalPct', -Chamber, -PrimeSponsorID) %>%
  mutate(SurvivalType=case_when(
    SurvivalType=='SurvivedPolicyCutoff' ~ 'Policy Committee',
    SurvivalType=='SurvivedFiscalCutoff' ~ 'Fiscal Committee',
    SurvivalType=='SurvivedHouseOfOriginCutoff' ~ 'House of Origin'
  )) %>%
  filter(SurvivalType=='House of Origin') %>%
  inner_join(Members %>% select(-Chamber), by=c('PrimeSponsorID'='ID')) %>%
  group_by(Chamber) %>%
  top_n(10, SurvivalPct) %>% ungroup() %>%
  mutate(MemberLabel=paste0(MemberLast, ' (', Party, '-', District, ')')) %>%
  mutate(SurvivalPctLabel=percent(SurvivalPct)) %>%
  ggplot() +
  geom_bar(aes(x=reorder(MemberLabel, SurvivalPct), y=SurvivalPct), stat='identity') +
  geom_text(aes(x=reorder(MemberLabel, SurvivalPct), y=SurvivalPct, label=SurvivalPctLabel), hjust='right', nudge_y = -.02, color='white') +
  coord_flip() +
  scale_y_continuous(labels=percent) +
  facet_grid(rows=vars(Chamber), scales='free') +
  theme_hc() +
  labs(x=NULL, y=NULL, title='Rates of Bill Survival in the 2019 Session of the Washington Legislature, By Prime Sponsor',
       subtitle='Percent of bills surviving the house of origin cutoff, ten highest sponsor survival rates shown',
       caption=paste0('Members only shown if they served as prime sponsor of at least ten bills\n',
                      'A bill counts as "surviving" if any version (substitute, engrossed, etc.) survives the cutoff\n',
                      'Bill status as of day 62 of the session (March 16)\n',
                      'Legislative vote data from Washington State Legislature, available at https://data.world/scottcame/washington-legislature-2019'))
  
RollCalls %>% tabulateVotes() %>%
  group_by(VoteDate, Chamber) %>%
  ggplot() + geom_bar(aes(x=VoteDate, fill=Chamber), position = 'dodge') +
  scale_fill_manual(values=c(House='#7fc97f', Senate='#beaed4')) +
  theme_hc() +
  labs(
    x=NULL, y='Roll Call Votes Taken', fill=NULL,
    title='Roll Call Votes per Day in the 2019 Session of the Washington Legislature',
    caption=paste0('Legislative vote data from Washington State Legislature, available at https://data.world/scottcame/washington-legislature-2019')
  )

