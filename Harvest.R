# Script that harvests legislation status from Legislative Service Center web services

library(tidyverse)
library(xml2)
library(lubridate)
library(dwapi)

if (!exists('Members')) {
  source('Members.R')
}

# for now, need to keep these in sync manually
biennium <- '2019-20'
startDate <- '2019-01-01'
endDate <- '2019-08-01'

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

billTypeMap <- c(
  'SI'='Initiative (Senate)',
  'HI'='Initiative (House)',
  'HB'='House Bill',
  'SHB'='Substitute House Bill',
  'HJM'='House Joint Memorial',
  'HJR'='House Joint Resolution',
  'HCR'='House Concurrent Resolution',
  'HR'='House Resolution',
  'SB'='Senate Bill',
  'ESSB'='Engrossed Substitute Senate Bill',
  'SSB'='Substitute Senate Bill',
  'ESB'='Engrossed Senate Bill',
  'SJM'='Senate Joint Memorial',
  'SJR'='Senate Joint Resolution',
  'SCR'='Senate Concurrent Resolution',
  'SR'='Senate Resolution',
  'ESR'='Engrossed Senate Resolution',
  'SGA'='Senate Gubernatorial Appointment'
)

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

LegislationIntroduced <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislationIntroducedSince?sinceDate=', startDate))
LegislationIntroducedL <- LegislationIntroduced %>% as_list() %>% .[[1]]
LegislationIntroducedDf <- map2_dfr(LegislationIntroducedL, seq_along(LegislationIntroducedL), function(leg, idx, len) {
  if (idx %% 100 == 0) {
    writeLines(paste0('Processing bill ', idx, ' of ', len))
  }
  BillNumber <- leg$BillNumber[[1]]
  BillId <- leg$BillId[[1]]
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
  PrimeSponsorID <- leg$PrimeSponsorID
  if (!is.null(PrimeSponsorID) && length(PrimeSponsorID)==1) {
    PrimeSponsorID <- PrimeSponsorID[[1]] %>% as.integer()
  } else {
    PrimeSponsorID <- NA_integer_
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
  companionBillId <- NA_character_
  companions <- leg$Companions
  if (length(companions)==1) {
    companion <- companions$Companion
    if (biennium==companion$Biennium[[1]]) {
      companionBillId <- companion$BillId[[1]]
    }
  }
  
  Chamber=leg$OriginalAgency[[1]]
  
  digestUrl <- paste0('http://lawfilesext.leg.wa.gov/biennium/2019-20/Htm/Digests/', Chamber, '/', BillNumber, '.DIG.htm')
  
  digest <- NA_character_
  tryCatch({
    digest <- read_html(digestUrl) %>% html_node('table') %>% html_table() %>% .$X1 %>% tail(-2) %>% paste0(collapse=' ')
  }, error=function(cond) {
    # ignore 404s and any other errors...we just won't get a digest
  })
  
  tibble(BillId=leg$BillId[[1]], BillNumber=BillNumber, ShortDescription=ShortDescription, Chamber=Chamber, PrimeSponsorID=PrimeSponsorID, Appropriations=approps,
         LongDescription=LongDescription, StatusDate=as_date(ymd_hms(leg$CurrentStatus$ActionDate[[1]])), CurrentStatus=CurrentStatus, CurrentStatusDescription=CurrentStatusDescription,
         CompanionBillId=companionBillId, DigestText=digest)
  
}, length(LegislationIntroducedL)) %>%
  mutate(BillType=billTypeMap[gsub(x=BillId, pattern='(.+) (.+)', replacement='\\1')])

LegislativeStatusChanges <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislativeStatusChangesByDateRange?biennium=', biennium,
  '&beginDate=', startDate, '&endDate=', endDate)) %>% as_list() %>% .[[1]]

LegislativeStatusChangesDf <- LegislativeStatusChanges %>%
  map_dfr(function(status) {
    tibble(BillId=status$BillId[[1]], ActionDate=status$ActionDate[[1]], HistoryLine=status$HistoryLine[[1]], Status=status$Status[[1]])
  }) %>% mutate_all(trimws) %>%
  mutate(ActionDate=as_datetime(ActionDate)) %>%
  mutate(BillType=billTypeMap[gsub(x=BillId, pattern='(.+) (.+)', replacement='\\1')])

# upload to data.world

configure(Sys.getenv('DATA_WORLD_RW_API_KEY'))
upload_data_frame('scottcame/washington-legislature-2019', Members, 'Members.csv')
upload_data_frame('scottcame/washington-legislature-2019', LegislationIntroducedDf, 'LegislationIntroduced.csv')
upload_data_frame('scottcame/washington-legislature-2019', votes, 'RollCalls.csv')
upload_data_frame('scottcame/washington-legislature-2019', LegislativeStatusChangesDf, 'LegislativeStatusChanges.csv')


