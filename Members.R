# Script to harvest a Member list from scraping the legislature website

library(xml2)
library(rvest)
library(tidyverse)
library(stringr)

senateMembers <- read_html('https://app.leg.wa.gov/Rosters/Members/Senate') %>% xml_find_first('//table[@id="membertable"]') %>% html_table()

memberPattern <- '(.+),(.+) \\((.+)\\)'

senateMembers <- senateMembers %>%
  mutate(MemberLast=gsub(x=Member, pattern=memberPattern, replacement='\\1'), MemberFirst=gsub(x=Member, pattern=memberPattern, replacement='\\2'), Party=gsub(x=Member, pattern=memberPattern, replacement='\\3')) %>%
  select(MemberFirst, MemberLast, Party, District) %>%
  mutate_if(is.character, trimws) %>%
  group_by(MemberLast) %>%
  mutate(MatchLast=case_when(n() > 1 ~ paste0(MemberLast, ', ', str_sub(MemberFirst, 1, 1), '.'), TRUE ~ MemberLast)) %>%
  # add in members who have resigned during the session (not currently on the website)
  bind_rows(tribble(
    ~MemberFirst, ~MemberLast, ~Party, ~District,
    "Kevin", "Ranker", "D", 40
  ))

houseMembers <- read_html('https://app.leg.wa.gov/Rosters/Members/House') %>% xml_find_first('//table[@id="membertable"]') %>% html_table()

houseMembers <- houseMembers %>%
  mutate(MemberLast=gsub(x=Member, pattern=memberPattern, replacement='\\1'), MemberFirst=gsub(x=Member, pattern=memberPattern, replacement='\\2'), Party=gsub(x=Member, pattern=memberPattern, replacement='\\3')) %>%
  select(MemberFirst, MemberLast, Party, District, Position)

# IDs harvested by manually picking the select options at https://app.leg.wa.gov/dlr/billsbysponsor/

senateMemberIdMap <- tribble(
  ~ID, ~Last,
  8238, "Bailey",
14083, "Becker",
15811, "Billig",
17289, "Braun",
17759, "Brown",
14325, "Carlyle",
17294, "Cleveland",
972, "Conway",
5156, "Darneille",
29087, "Das",
28022, "Dhingra",
3473, "Ericksen",
3474, "Fortunato",
15821, "Frockt",
10030, "Hasegawa",
2006, "Hawkins",
12072, "Hobbs",
17223, "Holy",
1577, "Honeyford",
5155, "Hunt",
1950, "Keiser",
13199, "King",
21520, "Kuderer",
13546, "Liias",
29548, "Lovelett",
8211, "McCoy",
17226, "Mullet",
29088, "Nguyen",
17217, "O'Ban",
322, "Padden",
27225, "Palumbo",
12002, "Pedersen",
29112, "Randall",
14074, "Ranker",
15814, "Rivers",
11998, "Rolfes",
27290, "Saldaña",
29089, "Salomon",
652, "Schoesler",
394, "Sheldon",
11952, "Short",
10249, "Takko",
12003, "Van De Wege",
28317, "Wagoner",
1232, "Walsh",
12084, "Warnick",
27211, "Wellman",
29090, "Wilson, C.",
20758, "Wilson, L.",
4006, "Zeiger"
)

houseMemberIdMap <- tribble(
  ~ID, ~Last,
10031, "Appleton",
24075, "Barkis",
17227, "Bergquist",
8317, "Blake",
29094, "Boehnke",
20760, "Caldier",
29092, "Callan",
29100, "Chambers",
3469, "Chandler",
26176, "Chapman",
1659, "Chopp",
1543, "Cody",
29097, "Corry",
29104, "Davis",
2143, "DeBolt",
20761, "Dent",
26175, "Doglio",
26174, "Dolan",
29098, "Dufault",
21490, "Dye",
11403, "Entenman",
27988, "Eslick",
17241, "Fey",
13198, "Fitzgibbon",
23902, "Frame",
29101, "Gildon",
29096, "Goehner",
11999, "Goodman",
29093, "Graham",
18264, "Gregerson",
20752, "Griffey",
16499, "Hansen",
15813, "Harris",
29099, "Hoff",
8215, "Hudgins",
27522, "Irwin",
26172, "Jenkin",
15817, "Jinkins",
20753, "Kilduff",
5154, "Kirby",
14207, "Klippert",
26168, "Kloba",
26173, "Kraft",
10041, "Kretz",
29102, "Leavitt",
29106, "Lekanoff",
3476, "Lovick",
17221, "MacEwen",
26178, "Macri",
14115, "Maycumber",
20741, "McCaslin",
18325, "Mead",
29103, "Morgan",
2144, "Morris",
20759, "Mosbrucker",
7635, "Orcutt",
9207, "Ormsby",
18546, "Ortiz-Self",
14205, "Orwall",
29095, "Paul",
26177, "Pellicciotti",
20755, "Peterson",
8286, "Pettigrew",
16596, "Pollet",
29091, "Ramos",
27182, "Reeves",
15706, "Riccelli",
18265, "Robinson",
20837, "Rude",
15736, "Ryu",
3483, "Santos",
13209, "Schmick",
9997, "Sells",
18057, "Senn",
14202, "Shea",
29108, "Shewmake",
27504, "Slatter",
13577, "Smith",
10039, "Springer",
15809, "Stanford",
10546, "Steele",
20756, "Stokesbary",
17279, "Stonier",
1218, "Sullivan",
29105, "Sutherland",
17243, "Tarleton",
29107, "Thai",
15816, "Tharinger",
27975, "Valdez",
20757, "Van Werven",
17218, "Vick",
26170, "Volz",
29109, "Walen",
27181, "Walsh",
15810, "Wilcox",
16462, "Wylie",
29318, "Ybarra",
18516, "Young"
)

Members <- senateMembers %>% left_join(senateMemberIdMap, by=c('MatchLast'='Last')) %>% select(-MatchLast) %>% mutate(Chamber='Senate') %>%
  bind_rows(houseMembers %>% left_join(houseMemberIdMap, by=c('MemberLast'='Last')) %>% mutate(Chamber='House'))

rm(senateMemberIdMap, houseMemberIdMap, senateMembers, houseMembers)
