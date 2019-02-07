# Script that harvests legislation status from Legislative Service Center web services

library(tidyverse)
library(xml2)
library(lubridate)
library(dwapi)

if (!exists('Members')) {
  source('Members.R')
}

biennium <- '2019-20'

LegislationPassedHouse <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationPassedHouse?biennium=', biennium))
LegislationPassedSenate <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationPassedSenate?biennium=', biennium))

extractPassedLegislationDf <- function(xml) {
  xml %>% as_list() %>% .[[1]] %>%
    map_dfr(function(leg) {
      BillNumber <- leg$BillNumber[[1]]
      legDetail <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislation?biennium=', biennium, '&billNumber=', BillNumber)) %>% as_list() %>% .[[1]] %>%
        keep(function(rec) { rec$Active[[1]]=='true' }) %>%
        map_dfr(function(leg) {
          tibble(LongDescription=leg$LongDescription[[1]], IntroducedDate=as_date(ymd_hms(leg$IntroducedDate[[1]])))
        })
      tibble(BillId=leg$BillId[[1]], BillNumber=BillNumber, OriginalAgency=leg$OriginalAgency[[1]], Type=leg$ShortLegislationType$LongLegislationType[[1]]) %>% bind_cols(legDetail)
    })
}

legislation <- LegislationPassedHouse %>% extractPassedLegislationDf() %>% mutate(PassedChamber='House') %>%
  bind_rows(LegislationPassedSenate %>% extractPassedLegislationDf() %>% mutate(PassedChamber='Senate'))

votes <- legislation$BillNumber %>%
  map_dfr(function(billNumber) {
    rollCall <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetRollCalls?biennium=', biennium, '&billNumber=', billNumber)) %>% as_list() %>% .[[1]]
    voteDate <- rollCall$RollCall$VoteDate[[1]] %>% ymd_hms() %>% as_date()
    chamber <- rollCall$RollCall$Agency[[1]]
    rollCall %>% .[['RollCall']] %>% .[['Votes']] %>%
      map_dfr(function(vote) {
        tibble(BillNumber=billNumber, Chamber=chamber, VoteDate=voteDate, MemberId=vote$MemberId[[1]], MemberName=vote$Name[[1]], Vote=vote$VOte[[1]])
      })
  }) %>% mutate(MemberId=as.integer(MemberId)) # %>% left_join(Members, by=c('Chamber', 'MemberId'='ID'))

rm(LegislationPassedHouse, LegislationPassedSenate)

LegislationIntroduced <- read_xml('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationIntroducedSince?sinceDate=2019-01-01')
LegislationIntroducedDf <- LegislationIntroduced %>% as_list() %>% .[[1]] %>%
  map_dfr(function(leg) {
    BillNumber <- leg$BillNumber[[1]]
    LongDescription <- leg$LongDescription
    if (!is.null(LongDescription) && length(LongDescription)==1) {
      LongDescription <- LongDescription[[1]]
    } else {
      LongDescription <- NA_character_
    }
    ShortDescription <- leg$ShortDescription
    if (!is.null(ShortDescription) && length(ShortDescription)==1) {
      ShortDescription <- ShortDescription[[1]]
    } else {
      ShortDescription <- NA_character_
    }
    CurrentStatus <- NA_character_
    CurrentStatusDescription <- NA_character_
    cs <- leg$CurrentStatus
    if (!is.null(cs)) {
      cs <- cs$Status
      if (!is.null(cs) && length(cs)==1) {
        CurrentStatus <- cs[[1]]
      }
      cs <- leg$CurrentStatus
      cs <- cs$HistoryLine
      if (!is.null(cs) && length(cs)==1) {
        CurrentStatusDescription <- cs[[1]]
      }
    }
    approps <- leg$Appropriations
    approps <- case_when(
      is.na(approps) ~ NA,
      approps=='true' ~ TRUE,
      TRUE ~ FALSE
    )
    tibble(BillId=leg$BillId[[1]], BillNumber=BillNumber, ShortDescription=ShortDescription, Chamber=leg$OriginalAgency[[1]], Appropriations=approps,
           LongDescription=LongDescription, StatusDate=as_date(ymd_hms(leg$CurrentStatus$ActionDate[[1]])), CurrentStatus=CurrentStatus, CurrentStatusDescription=CurrentStatusDescription)
  })

# upload to data.world

configure(Sys.getenv('DATA_WORLD_RW_API_KEY'))
upload_data_frame('scottcame/washington-legislature-2019', Members, 'Members.csv')
upload_data_frame('scottcame/washington-legislature-2019', LegislationIntroducedDf, 'LegislationIntroduced.csv')
upload_data_frame('scottcame/washington-legislature-2019', votes, 'RollCalls.csv')

#LegislativeStatusChanges <- read_xml('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislativeStatusChangesByDateRange?biennium=2019-20&beginDate=2019-01-01&endDate=2019-01-31')

# df <- LegislativeStatusChanges %>% as_list() %>% .[[1]] %>%
#   map_dfr(function(status) {
#     tibble(BillId=status$BillId[[1]], ActionDate=status$ActionDate[[1]], HistoryLine=status$HistoryLine[[1]], Status=status$Status[[1]])
#   }) %>% mutate_all(trimws) %>%
#   mutate(ActionDate=as_datetime(ActionDate))
