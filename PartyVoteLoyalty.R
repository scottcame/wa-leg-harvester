# Party loyalty in legislative votes

library(ggrepel)
library(ggthemes)
library(scales)
library(sf)

downloadData()

LoyaltyDf <- RollCalls %>%
  group_by(Chamber, MemberName, District, Position, Party) %>%
  summarize(Loyalty=mean(WithPartyMajority)) %>%
  group_by(Chamber) %>%
  arrange(Chamber, Loyalty) %>% ungroup()

ChamberLoyaltyDf <- RollCalls %>%
  group_by(Chamber, Party) %>%
  summarize(MeanChamberPartyLoyalty=mean(WithPartyMajority), MedianChamberPartyLoyalty=median(WithPartyMajority))

LoyaltyDf <- inner_join(LoyaltyDf, ChamberLoyaltyDf, by=c('Chamber', 'Party')) %>%
  mutate(MeanRelativeLoyalty=100*(Loyalty-MeanChamberPartyLoyalty), MedianRelativeLoyalty=100*(Loyalty-MedianChamberPartyLoyalty))

LoyaltyDf %>%
  top_n(5, -Loyalty) %>%
  ungroup() %>%
  mutate(Loyalty=scales::percent(Loyalty)) %>%
  filter(Chamber=='Senate') %>% select(-Chamber) %>%
  rename(`Votes with Party`=Loyalty, Member=MemberName) %>%
  kableExtra::kable(format='rst')

RollCalls %>% group_by(Chamber, Party) %>%
  summarize(Loyalty=mean(WithPartyMajority)) %>%
  mutate(Loyalty=scales::percent(Loyalty)) %>%
  rename(`Votes with Party`=Loyalty) %>%
  kableExtra::kable(format='rst')

LoyaltyDf %>%
  filter(Chamber=='House') %>%
  select(District, Position, Loyalty) %>%
  spread(Position, Loyalty, sep='Loyalty_') %>%
  inner_join(LoyaltyDf %>%
               filter(Chamber=='House') %>%
               select(District, Position, Party) %>%
               spread(Position, Party, sep='Party_') %>%
               mutate(Party=case_when(
                 PositionParty_1==PositionParty_2 ~ PositionParty_1,
                 TRUE ~ 'Split'
               )) %>% select(District, Party)) %>%
  filter(PositionLoyalty_1 < .98 | PositionLoyalty_2 < .98) %>%
  ggplot() + geom_label_repel(aes(x=PositionLoyalty_1, y=PositionLoyalty_2, label=District, fill=Party), min.segment.length = 10) + 
  scale_x_continuous(breaks=c(.7, .75, .8, .85, .9, .95, 1), limits=c(.7, 1.05), labels=percent) +
  scale_y_continuous(breaks=c(.7, .75, .8, .85, .9, .95, 1), limits=c(.7, 1.05), labels=percent) +
  scale_fill_manual(values=c('D'='CornflowerBlue', 'R'='Tomato', 'Split'='MediumPurple')) +
  coord_equal() +
  theme_minimal() +
  labs(x='Position 1: % of votes with party', y='Position 2: % of votes with party',
       title='Party Loyalty of Representatives from the Same District', subtitle='Washington House of Representatives, 2019 Session',
       caption='Note: Excludes Districts where both reps voted with party > 98 % of time')

waLegShp <- read_sf('/opt/data/Shapefiles/cb_2017_53_sldl_500k/cb_2017_53_sldl_500k.shp')

loyaltyChoropleth <- function(chamber, position, personLabel) {
  
  relativeLoyaltyLimit <- LoyaltyDf %>% filter(Chamber==chamber) %>% filter(is.na(position) | Position==position) %>% .$MeanRelativeLoyalty %>% range() %>% abs() %>% max()
  
  caption <- NULL
  
  if (position) {
    caption <- paste0('Map depicts party voting loyalty of representatives from Position ', position, '\n')
  }
  
  ggplot(waLegShp %>%
           inner_join(LoyaltyDf %>% filter(Chamber==chamber) %>% filter(is.na(position) | Position==position) %>% mutate(District=as.character(District)), by=c('NAME'='District'))) +
    geom_sf(aes(fill=Loyalty), color='grey70') +
    scale_fill_gradient(low='#f7fbff', high='#08519c', labels=percent, breaks=c(.7, .8, .9, 1), limits=c(.7, 1)) +
    theme_void() +
    theme(panel.grid.major=element_line(color="transparent"), legend.position = 'bottom', legend.title = element_text(size=10),
          legend.key.width = unit(50, 'points')) +
    labs(
      fill='% of votes with party majority:   ',
      title=paste0('Party Voting Loyalty in Washington State ', chamber, ' (2019 Session)'),
      subtitle=paste0('Representatives in Position #', position),
      caption=paste0(#caption,
                     'Legislative vote data from Washington State Legislature, available at https://data.world/scottcame/washington-legislature-2019'
                     )
    )
  
}

loyaltyChoropleth('House', 1, 'Representative')
loyaltyChoropleth('House', 2, 'Representative')
#loyaltyChoropleth('Senate', NA, 'Senator')

# look at loyalty compared to margin of victory in 2018 election

results2018 <- read_csv('~/git-repos/openelections/openelections-data-wa/2018/20181106__wa__general__precinct.csv', col_types=cols(.default=col_character())) %>%
  filter(!is.na(district)) %>%
  filter(grepl(x=office, pattern='^State')) %>%
  filter(candidate != 'Registered Voters') %>%
  filter(candidate != 'Write-in') %>%
  select(office, district, candidate, party, votes) %>%
  mutate(votes=as.integer(votes)) %>%
  group_by(office, district, candidate, party) %>% summarize(votes=sum(votes)) %>%
  filter(!grepl(x=office, pattern='Senat')) %>%
  mutate(position=gsub(x=office, pattern='.+Pos\\. ([12]).+', replacement='\\1')) %>%
  group_by(district, position) %>% mutate(totalVotes=sum(votes)) %>% filter(votes==max(votes)) %>%
  ungroup() %>%
  mutate(WinMargin=votes/totalVotes) %>% mutate(district=as.integer(district), position=as.integer(position)) %>%
  select(District=district, Position=position, WinMargin2018=WinMargin)

LoyaltyDf <- LoyaltyDf %>% inner_join(results2018, by=c('District', 'Position')) %>%
  mutate(PartyFull=case_when(Party=='R' ~ 'Republican', TRUE ~ 'Democrat'))

LoyaltyDf %>%
  ggplot() + geom_point(aes(x=Loyalty, y=WinMargin2018, color=PartyFull), size=2.5) +
  geom_label_repel(data=LoyaltyDf %>%
                     filter(Loyalty < .88 | (Loyalty < .9 & WinMargin2018 > .7) | grepl(x=MemberName, pattern='Apple')) %>%
                     mutate(label=paste0(MemberName, ' (', Party, '-', District, ')')),
                   mapping=aes(x=Loyalty, y=WinMargin2018, label=label), min.segment.length = .05, box.padding = .8, size=3) +
  scale_color_manual(values=c('Republican'='#ae0305', 'Democrat'='#003464')) +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) +
  theme_hc() +
  labs(
    title='Do close elections compel legislators to cross the aisle more often?',
    subtitle='Washington State House Members: Margin of victory in 2018 and party voting loyalty in the House during the 2019 session',
    x='% of votes with party majority', y='% of vote won in 2018 general election', color=NULL,
    caption=paste0('Election results data from Open Elections (@openelex)\n',
                   'Legislative vote data from Washington State Legislature, available at https://data.world/scottcame/washington-legislature-2019')
  )



