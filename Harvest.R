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
      ParentBillId <- leg$BillId[[1]]
      legDetail <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislation?biennium=', biennium, '&billNumber=', BillNumber)) %>% as_list() %>% .[[1]]
      legDetail <- legDetail %>%
        keep(function(rec) { rec$Active[[1]]=='true' }) %>%
        map_dfr(function(leg) {
          tibble(BillId=leg$BillId[[1]], LongDescription=leg$LongDescription[[1]], IntroducedDate=as_date(ymd_hms(leg$IntroducedDate[[1]])))
        }) %>% filter(BillId==ParentBillId) %>% select(-BillId)
      ret <- tibble(BillId=ParentBillId, BillNumber=BillNumber, OriginalAgency=leg$OriginalAgency[[1]], Type=leg$ShortLegislationType$LongLegislationType[[1]])
      if (nrow(legDetail) > 0) ret <- ret %>% bind_cols(legDetail)
      ret
    })
}

billTypeMap <- c(
  'SI'='Initiative (Senate)',
  'HI'='Initiative (House)',
  'HB'='House Bill',
  'SHB'='Substitute House Bill',
  'SHJM'='Substitute House Joint Memorial',
  'HJM'='House Joint Memorial',
  'HJR'='House Joint Resolution',
  'HCR'='House Concurrent Resolution',
  'HR'='House Resolution',
  'SB'='Senate Bill',
  'ESSB'='Engrossed Substitute Senate Bill',
  'ESHB'='Engrossed Substitute House Bill',
  'SSB'='Substitute Senate Bill',
  'ESB'='Engrossed Senate Bill',
  'EHB'='Engrossed House Bill',
  'SSJM'='Substitute Senate Joint Memorial',
  'SJM'='Senate Joint Memorial',
  'SJR'='Senate Joint Resolution',
  'SSJR'='Substitute Senate Joint Resolution',
  'SCR'='Senate Concurrent Resolution',
  'SR'='Senate Resolution',
  'ESR'='Engrossed Senate Resolution',
  'SGA'='Senate Gubernatorial Appointment'
)

# Assemble a single data frame of all passed bills

legislation <- LegislationPassedHouse %>% extractPassedLegislationDf() %>% mutate(PassedChamber='House') %>%
  bind_rows(LegislationPassedSenate %>% extractPassedLegislationDf() %>% mutate(PassedChamber='Senate'))

# go bill by bill and pull the roll call vote data, keeping only the last vote taken on each bill (might revise this in the future)

votes <- legislation$BillNumber %>%
  unique() %>%
  map_dfr(function(billNumber) {
    rollCalls <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetRollCalls?biennium=', biennium, '&billNumber=', billNumber)) %>% as_list()
    ret <- map2_dfr(rollCalls$ArrayOfRollCall, seq_along(rollCalls$ArrayOfRollCall), function(rollCall, idx) {
      voteDate <- rollCall$VoteDate[[1]] %>% ymd_hms() %>% as_date()
      chamber <- rollCall$Agency[[1]]
      ret <- rollCall %>% .[['Votes']] %>%
        map_dfr(function(vote) {
          tibble(BillNumber=billNumber, Chamber=chamber, VoteDate=voteDate, MemberId=vote$MemberId[[1]], MemberName=vote$Name[[1]], Vote=vote$VOte[[1]], idx=idx)
        })
      ret
    })
    if (nrow(ret) > 0) {
      ret <- ret %>% arrange(desc(VoteDate), idx) %>% group_by(Chamber, MemberId) %>% filter(row_number()==1) %>% ungroup() %>% select(-idx)
    }
    ret
  }) %>% mutate(MemberId=as.integer(MemberId))

rm(LegislationPassedHouse, LegislationPassedSenate)

# Now we pull data on *every* bill introduced, not just those that passed

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
  
  # grab the digest (short 2-4 paragraph description of the purpose/impact of the bill)
  
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
  mutate(BillType=billTypeMap[gsub(x=gsub(x=BillId, pattern='^([A-Z]*)[0-9]([A-Z]+)', replacement='\\1\\2'), pattern='(.+) (.+)', replacement='\\1')])

# pull all status changes of each bill (not sure this is that useful...)

LegislativeStatusChanges <- read_xml(paste0('http://wslwebservices.leg.wa.gov/LegislationService.asmx/GetLegislativeStatusChangesByDateRange?biennium=', biennium,
  '&beginDate=', startDate, '&endDate=', endDate)) %>% as_list() %>% .[[1]]

LegislativeStatusChangesDf <- LegislativeStatusChanges %>%
  map_dfr(function(status) {
    tibble(BillId=status$BillId[[1]], ActionDate=status$ActionDate[[1]], HistoryLine=status$HistoryLine[[1]], Status=status$Status[[1]])
  }) %>% mutate_all(trimws) %>%
  mutate(ActionDate=as_datetime(ActionDate)) %>%
  mutate(BillType=billTypeMap[gsub(x=BillId, pattern='(.+) (.+)', replacement='\\1')])

# Capture information about bills' status at each "cutoff" point (where bills die if not sufficiently far along in the legislative process).  The leg
# publishes these as PDFs at each of the three cutoff points during session.  Had to be careful to download the PDF before overwritten at the next cutoff.

# Downloaded from http://leg.wa.gov/LIC/Documents/BillsPassedCutoff.pdf
# Hand-parsed with tabulizer::extract_areas() into variable tables

tables <- readRDS('BillsPassedPolicyCutoff.rds')
policyCutoffBills <- tibble(BillId=tables %>% map(function(m) {tt <- as_tibble(m); c(tt$V1, tt$V4); }) %>% unlist()) %>% mutate(BillId=trimws(BillId)) %>% filter(BillId != '') %>%
  mutate(SurvivedPolicyCutoff=TRUE)

LegislationIntroducedDf <- left_join(LegislationIntroducedDf, policyCutoffBills, by='BillId')
LegislationIntroducedDf <- LegislationIntroducedDf %>% mutate(SurvivedPolicyCutoff=case_when(is.na(SurvivedPolicyCutoff) ~ FALSE, TRUE ~ TRUE))

tables <- readRDS('BillsPassedFiscalCutoff.rds')
fiscalCutoffBills <- tibble(BillId=tables %>% map(function(m) {
  tt <- as_tibble(m)
  secondCol <- character()
  if ('V4' %in% colnames(tt)) secondCol <- tt$V4
  c(tt$V1, secondCol)
}) %>% unlist()) %>%
  mutate(BillId=trimws(BillId)) %>% filter(BillId != '') %>%
  mutate(SurvivedFiscalCutoff=TRUE)

LegislationIntroducedDf <- left_join(LegislationIntroducedDf, fiscalCutoffBills, by='BillId')
LegislationIntroducedDf <- LegislationIntroducedDf %>% mutate(SurvivedFiscalCutoff=case_when(is.na(SurvivedFiscalCutoff) ~ FALSE, TRUE ~ TRUE))

tables <- readRDS('BillsPassedHouseOfOriginCutoff.rds')
hooCutoffBills <- tibble(BillId=tables %>% map(function(m) {
  tt <- as_tibble(m)
  secondCol <- character()
  if ('V4' %in% colnames(tt)) secondCol <- tt$V4
  c(tt$V1, secondCol)
}) %>% unlist()) %>%
  mutate(BillId=trimws(BillId)) %>% filter(BillId != '') %>%
  mutate(SurvivedHouseOfOriginCutoff=TRUE)

LegislationIntroducedDf <- left_join(LegislationIntroducedDf, hooCutoffBills, by='BillId')
LegislationIntroducedDf <- LegislationIntroducedDf %>% mutate(SurvivedHouseOfOriginCutoff=case_when(is.na(SurvivedHouseOfOriginCutoff) ~ FALSE, TRUE ~ TRUE))

# upload to data.world

configure(Sys.getenv('DATA_WORLD_RW_API_KEY'))
upload_data_frame(Members, 'scottcame', 'washington-legislature-2019', 'Members.csv')
upload_data_frame(LegislationIntroducedDf, 'scottcame', 'washington-legislature-2019', 'LegislationIntroduced.csv')
upload_data_frame(votes, 'scottcame', 'washington-legislature-2019', 'RollCalls.csv')
upload_data_frame(LegislativeStatusChangesDf, 'scottcame', 'washington-legislature-2019', 'LegislativeStatusChanges.csv')

# establish this function for use elsewhere

downloadData <- function() {
  configure(Sys.getenv('DATA_WORLD_RW_API_KEY'))
  Bills <<- download_file_as_data_frame('scottcame', 'washington-legislature-2019', 'LegislationIntroduced.csv') %>% as_tibble() %>%
    mutate_if(is.factor, as.character) %>%
    mutate_at(vars(ends_with('Date')), ymd)
  Members <<- download_file_as_data_frame('scottcame', 'washington-legislature-2019', 'Members.csv') %>% as_tibble() %>% mutate_if(is.factor, as.character)
  RollCalls <<- download_file_as_data_frame('scottcame', 'washington-legislature-2019', 'RollCalls.csv') %>% as_tibble() %>%
    mutate_if(is.factor, as.character) %>%
    mutate_at(vars(ends_with('Date')), ymd)
  StatusChanges <<- download_file_as_data_frame('scottcame', 'washington-legislature-2019', 'LegislativeStatusChanges.csv') %>% as_tibble() %>%
    mutate_if(is.factor, as.character) %>%
    mutate_at(vars(ends_with('Date')), ymd)
  RollCalls <<- RollCalls %>%
    inner_join(Members %>% ungroup() %>% select(ID, Party, District, Position), by=c('MemberId'='ID')) %>%
    inner_join(Bills %>% select(BillNumber, BillId, ShortDescription, CompanionBillId), by='BillNumber')
  RollCalls <<- RollCalls %>%
    inner_join(RollCalls %>%
                 tabulateVotes() %>%
                 mutate(
                   D_Maj=case_when(D_Yea > D_Nay ~ 'Yea', TRUE ~ 'Nay'),
                   R_Maj=case_when(R_Yea > R_Nay ~ 'Yea', TRUE ~ 'Nay'),
                 ) %>%
                 select(Chamber, BillId, D_Maj, R_Maj), by=c('Chamber', 'BillId')) %>%
    mutate(PartyMajorityVote=case_when(
      Party=='D' ~ D_Maj,
      TRUE ~ R_Maj
    )) %>%
    select(-D_Maj, -R_Maj) %>%
    mutate(WithPartyMajority=Vote==PartyMajorityVote)
  invisible()
}