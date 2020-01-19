library(sf)
library(tidyverse)
library(lubridate)
library(dwapi)

waLegShp <- read_sf('/opt/data/Shapefiles/cb_2017_53_sldl_500k/cb_2017_53_sldl_500k.shp')

makePartyVote <- function(Party, Vote, sepChar='-') case_when(Vote %in% c('Yea','Nay') ~ paste0(Party, sepChar, Vote), TRUE ~ Vote)

# function to make a cartogram for the specified bill

createCartogram <- function(voteDf, customBillLabel=NULL) {
  
  missingColumns <- setdiff(c('Vote', 'Party', 'District', 'Position', 'Chamber', 'VoteDate', 'BillId', 'BillNumber', 'ShortDescription'), colnames(voteDf))
  if (length(missingColumns)) {
    stop(paste0('Cannot produce cartogram due to missing columns in voteDf: ', paste0(missingColumns, collapse=', ')))
  }
  
  bns <- unique(voteDf$BillNumber)
  if (length(bns) > 1) {
    stop(paste0('Multiple bills detected in voteDf, we can only do a cartogram for one at a time: ', paste0(bns, collapse=', ')))
  }
  
  centroids <- suppressWarnings(st_centroid(waLegShp))
  
  coords <- st_coordinates(centroids) %>% as_tibble() %>% rename(Longitude=X, Latitude=Y) %>%
    bind_cols(centroids %>% select(NAME) %>% st_set_geometry(NULL))
  
  districtVertices <- suppressWarnings(st_cast(waLegShp, 'POINT')) %>% st_coordinates() %>% as_tibble() %>% rename(Longitude=X, Latitude=Y)
  top <- max(districtVertices$Latitude)
  bottom <- min(districtVertices$Latitude)
  
  N_STACKS <- 30
  
  stackBoundaries <- seq(min(coords$Longitude), max(coords$Longitude), by=(max(coords$Longitude) - min(coords$Longitude))/N_STACKS)
  
  stacks <- map2(head(stackBoundaries, -1), tail(stackBoundaries, -1), function(w, e, n, s) {
    st_polygon(list(matrix(c(w,s,w,n,e,n,e,s,w,s), ncol=2, byrow=TRUE)))
  }, top, bottom) %>% st_sfc(crs=st_crs(waLegShp))
  
  stacks <- tibble(Stack=1:N_STACKS) %>% st_set_geometry(stacks)
  
  centroids <- suppressMessages(suppressWarnings(st_intersection(centroids, stacks))) %>%
    inner_join(coords, by='NAME') %>%
    inner_join(
      suppressWarnings(st_centroid(stacks)) %>% st_coordinates() %>% as_tibble() %>% bind_cols(stacks %>% st_set_geometry(NULL)) %>% select(Stack, StackCenterLongitude=X),
      by='Stack'
    )
  
  centroids <- st_as_sf(centroids %>% st_set_geometry(NULL) %>% select(-Longitude), coords=c('StackCenterLongitude', 'Latitude'), crs=st_crs(waLegShp))
  
  spaceNumbers <- function(v, threshold) {
    
    if (any(is.na(v))) {
      stop('Cannot space number vector containing NAs')
    }
    
    if (is.unsorted(rev(v))) {
      stop('Numbers are not sorted descending')
    }
    
    spaceNumbers_ <- function(v, threshold, s) {
      ret <- v
      for (i in seq_along(head(ret,-1))) {
        d <- abs(ret[i] - ret[i+1])
        if (d < threshold) {
          ret[i] <- ret[i] + s*(threshold - d)
          ret <- c(spaceNumbers_(ret[1:i], threshold, s), ret[-(1:i)])
        }
      }
      ret
    }
    
    m <- median(v)
    topHalf <- which(v >= m)
    bottomHalf <- rev(which(v < m))
    
    spaceNumbers_(c(spaceNumbers_(v[topHalf], threshold, 1), rev(spaceNumbers_(v[bottomHalf], threshold, -1))), threshold, 1)
    
  }
  
  centroids <- st_coordinates(centroids) %>% as_tibble() %>% rename(Longitude=X, Latitude=Y) %>%
    bind_cols(centroids %>% st_set_geometry(NULL) %>% select(Stack, District=NAME)) %>%
    group_by(Stack) %>%
    arrange(desc(Latitude)) %>%
    mutate(Latitude=spaceNumbers(Latitude, .15)) %>% st_as_sf(coords=c('Longitude', 'Latitude'), crs=st_crs(waLegShp)) %>%
    ungroup()
  
  emptyStacks <- setdiff(seq(N_STACKS), centroids$Stack %>% unique())
  stack1Longs <- stacks %>% filter(row_number()==1) %>% st_coordinates() %>% .[,'X']
  stackWidth <- abs(max(stack1Longs) - min(stack1Longs))
  
  centroids2 <- centroids %>%
    st_coordinates() %>%
    as_tibble() %>%
    bind_cols(centroids %>% st_set_geometry(NULL) %>% select(Stack, District))
  
  for (es in emptyStacks) {
    centroids2 <- centroids2 %>%
      mutate(X=case_when(Stack < es ~ X + stackWidth, TRUE ~ X))
  }
  
  centroids <- centroids2 %>% st_as_sf(coords=c('X', 'Y'), crs=st_crs(waLegShp)) %>%
    st_coordinates() %>%
    as_tibble() %>%
    bind_cols(centroids2 %>% select(Stack, District)) %>%
    mutate(District=as.integer(District))
  
  centroidsG <- centroids %>%
    inner_join(voteDf, by='District') %>%
    mutate(PartyVote=makePartyVote(Party, Vote))
  
  southwestOutline <- tribble(
    ~X, ~Y,
    -120.95, 46.69,
    -118.96, 46.69,
    -118.96, 45.50,
    -119.80, 45.50,
    -119.80, 45.40,
    -120.05, 45.40,
    -120.18, 45.80,
    -120.90, 46.00,
    -120.95, 46.69
  )
  
  easternOutline <- tribble(
    ~X, ~Y,
    -118.94, 45.50,
    -118.94, 49.00,
    -117.10, 49.00,
    -117.10, 45.75,
    -118.00, 45.75,
    -118.94, 45.50,
  )
  
  southSoundOutline <- tribble(
    ~X, ~Y,
    -120.25, 46.71,
    -120.25, 47.49,
    -119.85, 47.49,
    -119.85, 47.185,
    -119.36, 47.185,
    -119.36, 47.35,
    -118.96, 47.35,
    -118.96, 46.71,
    -120.25, 46.71,
  )
  
  peninsulaOutline <- tribble(
    ~X, ~Y,
    -120.95, 46.71,
    -120.27, 46.71,
    -120.27, 47.51,
    -119.85, 47.51,
    -119.85, 48.00,
    -120.50, 48.15,
    -121.00, 48.40,
    -120.95, 46.71,
  )
  
  kingOutline <- tribble(
    ~X, ~Y,
    -119.83, 48.24,
    -119.61, 48.24,
    -119.61, 47.94,
    -118.96, 47.94,
    -118.96, 47.365,
    -119.37, 47.365,
    -119.37, 47.20,
    -119.83, 47.20,
    -119.83, 48.24,
  )
  
  northSoundOutline <- tribble(
    ~X, ~Y,
    -119.83, 48.255,
    -119.83, 48.50,
    -120.05, 48.50,
    -120.05, 49.00,
    -118.96, 49.00,
    -118.96, 47.955,
    -119.60, 47.955,
    -119.60, 48.255,
    -119.83, 48.255,
  )
  
  spokaneOutline <- tribble(
    ~X, ~Y,
    -117.19, 47.50,
    -117.62, 47.50,
    -117.62, 47.92,
    -117.19, 47.92,
    -117.19, 47.50,
  )
  
  regionLabels <- tribble(
    ~Label, ~X, ~Y, ~Just,
    'North Sound', -120.00, 48.90, 'left',
    'Spokane Area', -117.62, 48.00, 'left',
    'Eastern Washington', -118.90, 48.90, 'left',
    'Southwest Washington', -120.88, 46.10, 'left',
    'Olympic Peninsula', -120.92, 46.80, 'left',
    'South Sound', -119.00, 46.80, 'right',
    'King County', -119.00, 47.87, 'right',
  )
  
  shades <- c('D-Yea'='#003464', 'D-Nay'='#839fc2', 'R-Yea'='#ae0305', 'R-Nay'='#d98385', 'Excused'='grey90', 'Absent'='grey90')
  textColors <- c('D-Yea'='white', 'D-Nay'='black', 'R-Yea'='white', 'R-Nay'='black', 'Excused'='black', 'Absent'='black')
  
  result <- centroidsG %>%
    group_by(Vote) %>%
    summarize(n=n()) %>%
    bind_rows(tribble(
      ~Vote, ~n,
      'Yea', 0,
      'Nay', 0
    )) %>%
    group_by(Vote) %>%
    arrange(desc(n)) %>%
    filter(row_number()==1) %>%
    ungroup()
  
  tally <- map2_chr(result$Vote, result$n, function(vote, tally) {
    paste0(vote, ': ', tally)
  }) %>% paste0(collapse=', ')
  
  result <- result %>% filter(row_number()==1) %>% .$Vote
  result <- case_when(result=='Yea' ~ 'passed the ', TRUE ~ 'failed in the ')
  result <- paste0('Bill ', result)
  
  VoteDate <- centroidsG %>% filter(row_number()==1) %>% .$VoteDate %>% format('%B %e, %Y')
  
  firstRow <- voteDf %>% filter(row_number()==1)
  chamber <- firstRow$Chamber
  billId <- firstRow$BillId
  if (is.null(customBillLabel)) {
    customBillLabel <- firstRow$ShortDescription
  }
  
  result <- paste0(result, chamber, ' on ', VoteDate, '. ')

  tileHeight <- .11
  regionColor <- 'grey50'
  
  districtCaptionPart <- 'Legislative districts indicated by number'
  
  addSymbols <- function(ggp) {
    tileWidth <- tileHeight # won't be square because width of plot is greater than height
    ggp +
      geom_tile(data=centroidsG, aes(x=X, y=Y, fill=PartyVote), width=tileWidth, height=tileHeight) +
      geom_text(data=centroidsG %>% mutate(cc=textColors[PartyVote]), mapping=aes(x=X, y=Y, label=District, color=PartyVote), vjust='middle', size=3) +
      scale_color_manual(values=textColors, guide=FALSE)
  }
  
  if (chamber=='House') {
    addSymbols <- function(ggp) {
      tileWidth <- tileHeight/2
      ggp +
        geom_tile(data=centroidsG %>% filter(Position==1) %>% mutate(X=X-(tileWidth/2) + .03 - .002), aes(x=X, y=Y, fill=PartyVote), width=tileWidth, height=tileHeight) +
        geom_tile(data=centroidsG %>% filter(Position==2) %>% mutate(X=X+(tileWidth/2) + .03 + .002), aes(x=X, y=Y, fill=PartyVote), width=tileWidth, height=tileHeight) +
        geom_text(data=centroidsG %>% mutate(cc=textColors[PartyVote]) %>% mutate(X=X - tileWidth + .02), mapping=aes(x=X, y=Y, label=District), vjust='middle', hjust='right', size=3)
    }
    districtCaptionPart <- paste0(districtCaptionPart, '; positions 1 and 2 represented by left and right boxes, respectively')
  }
  
  ggp <- ggplot() + 
    geom_path(data=southwestOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=easternOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=southSoundOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=peninsulaOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=kingOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=northSoundOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_path(data=spokaneOutline, mapping=aes(x=X, y=Y), color=regionColor) +
    geom_text(data=regionLabels %>% filter(Just=='left'), mapping=aes(x=X, y=Y, label=Label), hjust='left', size=3, color=regionColor) +
    geom_text(data=regionLabels %>% filter(Just=='right'), mapping=aes(x=X, y=Y, label=Label), hjust='right', size=3, color=regionColor)
  
  ggp <- addSymbols(ggp)
  
  ggp + scale_fill_manual(values=shades, breaks=names(shades)) +
    theme_void() +
    theme(plot.margin=unit(c(6,6,6,6),"points")) +
    labs(
      title=paste0('Washington State ', chamber, ' Roll Call Vote on ', billId, ' (', customBillLabel, ')'),
      subtitle=paste0(result, tally),
      caption=paste0(
        'Source: WA Legislative Service Center web services, http://wslwebservices.leg.wa.gov/LegislationService.asmx\n',
        'Data available for download at: https://data.world/scottcame/washington-legislature-2019\n',
        districtCaptionPart
      ),
      fill=NULL)
  
}

tabulateVotes <- function(voteDf) {
  voteDf %>%
    mutate(PartyVote=makePartyVote(Party, Vote, '_')) %>%
    group_by(BillNumber, BillId, VoteDate, ShortDescription, Chamber, PartyVote, CompanionBillId) %>%
    summarize(Votes=n()) %>%
    spread(PartyVote, Votes, fill=0) %>%
    ungroup() %>%
    mutate(Nay=D_Nay+R_Nay, Yea=D_Yea+R_Yea, Margin=Yea-Nay, Passed=Margin>0) %>%
    mutate_if(is.double, as.integer) %>% mutate(VoteDate=as_date(VoteDate)) %>%
    arrange(desc(VoteDate))
}

downloadData('2020')

# Look at the votes
latestDate <- max(RollCalls$VoteDate)
writeLines(paste0('Latest Roll Call is ', latestDate))
RollCalls %>% tabulateVotes() %>%
  filter(VoteDate %in% (latestDate - 0:7)) %>%
  arrange(Margin) %>%
  View()

# Look at the detailed vote on one bill
RollCalls %>% filter(BillId=='ESSB 5526') %>% View()

# Make pretty map
RollCalls %>% filter(BillId=='EHB 1638' & Chamber=='House') %>%
  createCartogram()
  createCartogram('Increasing cap on sales tax for emergency communications')
