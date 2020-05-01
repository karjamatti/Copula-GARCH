#' --------------------------------------
#' A BAYESIAN COINTEGRATION TESTING PROCEDURE WITH CONDITIONAL PRIOR ODDS - DATA PROCESSING
#' --------------------------------------


# Load or install and load packages
'package' <- function(library, repo = getOption('repos')){ 
  if(!is.element(library, .packages(all.available = TRUE))) {install.packages(library)}
  library(library,character.only = TRUE)}

'package'('dplyr')
'package'('lubridate')
'package'('Rfast')
'package'('data.table')

#dir <- 'Z:/Desktop'
dir <- 'C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/CRSP_Data'
setwd(dir)
stopifnot(all.equal(getwd(), dir))

#'------------------------------------------------
#' DATA
#'------------------------------------------------

crsp <- fread('dailydata_.csv', header = TRUE, stringsAsFactors = FALSE, sep = ',') %>% as.data.frame()
head(crsp) # Check structure

crsp <- crsp %>% select(date, everything()) # Move date to first column
colnames(crsp) <- c('Date', 'PermNo', 'ShareCode', 'Exchange', 'Ticker', 'NAIC', 'Cusip', 'Price', 'Volume', 'Return', 'ShrOut') # Name columns
crsp$Date <- lubridate::ymd(crsp$Date) # Convert dates to date objects

'CUSIPcheck' <- function(x){
  
  if((x %>% nchar()) != 8) {stop("Provided CUSIP does not have length 8")}
  v <- unlist(strsplit(x, ""))
  if(any(v %in% LETTERS)) {
    v[which(v %in% LETTERS)] <- which(LETTERS %in% v[which(v %in% LETTERS)])+9
    v <- as.numeric(v)
  } else {v <- as.numeric(v)}
  out <- 0
  for(i in 1:8) {
    if(i %% 2 == 0){v[i] <- v[i]*2}
    out <- out + v[i] %/% 10 + v[i] %% 10
  }
  ((10 - (out %% 10)) %% 10) %>% return()
}

CheckCusips <- FALSE
clist <- crsp$Cusip %>% unique

if (CheckCusips){
  clist9 <- clist %>% length() %>% numeric()
  for (i in 1:length(clist9)) {clist9[i] <- CUSIPcheck(clist[i])}
  cusip9 <- paste0(clist, clist9)
  c9 <- data.frame(Cusip = cusip9)
  fwrite(c9, '9Cusip.csv')  
}

#'------------------------------------------------------
#' MATCH ATTRIBUTES FROM DATASTREAM FILES
#'------------------------------------------------------

dir <- 'C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Thomson_Data'
setwd(dir)
stopifnot(all.equal(getwd(), dir))

BVpS <- fread('BVperS.csv', skip = 1, header = TRUE, stringsAsFactors = FALSE, sep = ',') %>% as.data.frame()

ISIN <- colnames(BVpS)[2:ncol(BVpS)]

'ISIN to Cusip' <- function(ISIN){
  if((ISIN %>% nchar()) == 12) {
    substr(ISIN, 3, 10) %>% return()  
  } else {NA %>% return()}
}

C8.Key <- ISIN %>% length() %>% numeric()

for (i in 1:(ISIN %>% length())){
  C8.Key[i] <- 'ISIN to Cusip'(ISIN[i])  
}

for (i in 1:(C8.Key %>% length())){
  if (C8.Key[i] %>% is.na()){
    C8.Key[i] <- clist[i]  
  }
}

colnames(BVpS)[2:(BVpS %>% ncol())] <- C8.Key %>% as.list()
colnames(BVpS)[1] <- 'Date'
BVpS$Date <- lubridate::mdy(BVpS$Date) # Convert dates to date objects
day(BVpS$Date) <- 1 # First of month
month(BVpS$Date) <- month(BVpS$Date) + 1 # Assign to next month (since this is observed for the previous month)
day(BVpS$Date) <- days_in_month(BVpS$Date) # Convert back to end of month

B.melt <- melt(BVpS %>% as.data.table(), 'Date') %>% as.data.frame()
B.melt$Key <- paste(B.melt$Date, B.melt$variable, sep = '_') # Hash key for matching between data frames
B.melt %>% head()

#'------------------------------------------------------
#' PRICES: CRSP shows closing prices with minus sign if replaced by average bid-ask
#' We want strictly non-negative prices...
#' Also, we want to filter out penny stocks
#'------------------------------------------------------

'fix' <- function(x) {x %>% as.character() %>% as.numeric() %>% return()} # 'Fixes' factor-valued numeric data back to numeric
'errs' <- function(arg) {arg %>% 'fix'() %>% is.na() %>% which() %>% length() %>% return()} # Function that returns number of non-numeric elements (errors)
errs <- crsp$Price %>% 'errs'() + crsp$ShrOut %>% 'errs'() + crsp$Volume %>% 'errs'() # Check for non-numeric values

if (errs > 0){
  
  p <- crsp$Price  # Extract price column as vector for faster mutation
  price <- ifelse(p %>% 'fix'() %>% is.na() == FALSE, p %>% 'fix'() %>% abs(), 0) # Replace negative values with abs, NAs as 0
  crsp$Price <- price # Replace Price column in data frame
  
  s <- crsp$ShrOut # Extract Outstanding shares column as vector for faster mutation
  mktcap <- ifelse(s %>% 'fix'() %>% is.na == FALSE, s %>% 'fix'() * price, 0) # Compute market cap, assign 0 where unavailable
  crsp$MktCap <- mktcap # Add to data frame
  
  v <- crsp$Volume # Extract trading volume (units) column as vector for faster mutation
  liq <- ifelse(v %>% 'fix'() %>% is.na == FALSE, v %>% 'fix'() * price, 0) # Compute daily trading volume (dollar value)
  crsp$Liq <- liq # Add to data frame
  
} else {
  
  crsp %>% {mutate(., Price = Price %>% abs(), MktCap = ShrOut * Price, Liq = Volume * Price)}
  
}

#'-----------------------------------------------------
#' CLEANING UP: COMMON STOCKS IN NYSE, AMEX & NASDAQ ONLY, NO PENNY-STOCKS, ONLY VALID VALUES ETC...
#'-----------------------------------------------------

codes <- c('10','11') # CRSP Share codes for common stocks
crsp <- subset(crsp, ShareCode %in% codes) # Discard non common stocks
Ex <- c('1','2','3') # CRSP Exhange codes for NYSE (1), AMEX (2) and NASDAQ (3)
crsp <- subset(crsp, Exchange %in% Ex) # Discard securities outside these exchanges
crsp <- subset(crsp, Return != 'C') # Remove rows with missing return data
crsp$Return <- crsp$Return %>% 'fix'() # Convert to numeric
crsp <- subset(crsp, NAIC %>% is.na() == FALSE) # Remove rows with missing industry classifier
titles <- c('Date', 'Cusip', 'NAIC', 'MktCap', 'Liq', 'Price', 'Return') #Select the relevant columns
crsp <- crsp %>% select(titles)

#'-----------------------------------------------------
#' ADD THOMSON DATA AND CLEAN UP
#'-----------------------------------------------------

lastofmonth <- crsp$Date # Copy Date vector for manipulation
day(lastofmonth) <- days_in_month(lastofmonth) # Convert to true last day of month for simplicity
crsp$Month <- lastofmonth # Add to data frame
crsp$key <- paste(crsp$Month, crsp$Cusip, sep = '_') #Add hash key to crsp for matching

head(crsp)

crsp$BV <- B.melt$value[match(crsp$key, B.melt$Key)] # Add BV per Share
crsp2008 <- subset(crsp, Month %>% year() == 2008) # Save first year for later
crsp$numBV <- crsp$BV %>% as.character() %>% as.numeric()
crsp <- subset(crsp, BV %>% is.na() == FALSE) # Remove rows with missing Book Values
crsp <- crsp %>% as_tibble() %>% mutate(PB = numBV / Price) %>% as.data.frame() # Add book to price
crsp$numBV <- NULL
crsp2008$PB <- crsp2008 %>% nrow() %>% numeric() # Add column to match dimensions
crsp <- rbind(crsp2008, crsp) # Add back first year

#'-----------------------------------------------------
#' WINSORIZING
#'-----------------------------------------------------

crsp %>% pull(PB) %>% hist(breaks = 1000, ylim = c(0,1000)) # Check distribution
crsp <- crsp %>% subset(PB < 500)
crsp <- crsp %>% subset(PB > -100)
crsp <- crsp %>% subset(Price > 1) #Filter out penny stocks

#'-----------------------------------------------------
#' MONTHLY LIQUIDITY
#'-----------------------------------------------------

monthliq <- aggregate(crsp$Liq, by = list(Month = crsp$Month, Cusip = crsp$Cusip), FUN = sum) %>% as.data.frame() # Sum over trading volume (dollar value) for each stock in each month
monthcap <- aggregate(crsp$MktCap, by = list(Month = crsp$Month, Cusip = crsp$Cusip), FUN = mean) %>% as.data.frame() # Avg mkt cap
monthpb <- aggregate(crsp$PB, by = list(Month = crsp$Month, Cusip = crsp$Cusip), FUN = mean) %>% as.data.frame() # Avg BP

names(monthliq)[names(monthliq) == 'x'] <- 'Liquidity' # Rename column
names(monthcap)[names(monthcap) == 'x'] <- 'Cap' # Rename column
names(monthpb)[names(monthpb) == 'x'] <- 'PB' # Rename column
monthliq <- monthliq %>% {arrange(., Month, Cusip)} # Arrange by month
monthcap <- monthcap %>% {arrange(., Month, Cusip)} # Arrange by month
monthpb <- monthpb %>% {arrange(., Month, Cusip)} # Arrange by month
monthliq$Liquidity <- monthliq$Liquidity / monthcap$Cap

#'-----------------------------------------------------
#' TEMP WRITE CSV
#'-----------------------------------------------------

dir <- 'C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Temp_Data'
setwd(dir)
stopifnot(all.equal(getwd(), dir))

fwrite(crsp, 'crsp.csv') # Export processed data
fwrite(monthliq, 'monthliq.csv') # Export processed data
fwrite(monthcap, 'monthcap.csv') # Export processed data
fwrite(monthpb, 'monthpb.csv') # Export processed data

  
crsp <- fread('crsp.csv', sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
monthliq <- fread('monthliq.csv', sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
monthcap <- fread('monthcap.csv', sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
monthpb <- fread('monthpb.csv', sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  

crsp$Date <- lubridate::ymd(crsp$Date) # Convert dates to date objects
crsp$Month <- lubridate::ymd(crsp$Month)
monthliq$Month <- lubridate::ymd(monthliq$Month)
monthcap$Month <- lubridate::ymd(monthcap$Month)
monthpb$Month <- lubridate::ymd(monthpb$Month)

head(crsp) # Check structure
lastofmonth <- crsp$Month

#'-----------------------------------------------------
#' BREAKPOINTS
#'-----------------------------------------------------

months <- lastofmonth %>% unique() %>% sort() # Get list of month end dates
liq.breakpoints1 <- liq.breakpoints2 <- months %>% length() %>% numeric()
mc1 <- mc2 <- mc3 <- mc4 <- mc5 <- mc6 <- mc7 <- mc8 <- mc9 <- months %>% length() %>% numeric()
pb1 <- pb2 <- pb3 <- pb4 <- pb5 <- pb6 <- pb7 <- pb8 <- pb9 <- months %>% length() %>% numeric()

N1 <- 200
N2 <- N1*2
BP <- 10 # Number of breakpoints

for (i in 1:length(months)){
  
  liqu <- monthliq %>% {filter(., Month == months[i])} %>% {pull(., 'Liquidity')}
  liq.breakpoints1[i] <- liqu %>% {Rfast::nth(., N1, descending = TRUE)}
  liq.breakpoints2[i] <- liqu %>% {Rfast::nth(., N2, descending = TRUE)}

}

for (i in 1:length(months)){
  
  cap <- monthcap %>% {filter(., Month == months[i])} %>% {pull(., 'Cap')}
  N.mc <- ((cap %>% length()) / BP) %>% floor() # Number of stocks per decile
  mc1[i] <- cap %>% {Rfast::nth(., N.mc*1, descending = TRUE)}
  mc2[i] <- cap %>% {Rfast::nth(., N.mc*2, descending = TRUE)}
  mc3[i] <- cap %>% {Rfast::nth(., N.mc*3, descending = TRUE)}
  mc4[i] <- cap %>% {Rfast::nth(., N.mc*4, descending = TRUE)}
  mc5[i] <- cap %>% {Rfast::nth(., N.mc*5, descending = TRUE)}
  mc6[i] <- cap %>% {Rfast::nth(., N.mc*6, descending = TRUE)}
  mc7[i] <- cap %>% {Rfast::nth(., N.mc*7, descending = TRUE)}
  mc8[i] <- cap %>% {Rfast::nth(., N.mc*8, descending = TRUE)}
  mc9[i] <- cap %>% {Rfast::nth(., N.mc*9, descending = TRUE)}
  
}

for (i in 1:length(months)){
  
  pb <- monthpb %>% {filter(., Month == months[i])} %>% {pull(., 'PB')}
  N.pb <- ((pb %>% length()) / BP) %>% floor() # Number of stocks per decile
  pb1[i] <- pb %>% {Rfast::nth(., N.pb*1, descending = TRUE)}
  pb2[i] <- pb %>% {Rfast::nth(., N.pb*2, descending = TRUE)}
  pb3[i] <- pb %>% {Rfast::nth(., N.pb*3, descending = TRUE)}
  pb4[i] <- pb %>% {Rfast::nth(., N.pb*4, descending = TRUE)}
  pb5[i] <- pb %>% {Rfast::nth(., N.pb*5, descending = TRUE)}
  pb6[i] <- pb %>% {Rfast::nth(., N.pb*6, descending = TRUE)}
  pb7[i] <- pb %>% {Rfast::nth(., N.pb*7, descending = TRUE)}
  pb8[i] <- pb %>% {Rfast::nth(., N.pb*8, descending = TRUE)}
  pb9[i] <- pb %>% {Rfast::nth(., N.pb*9, descending = TRUE)}
  
}

liq.bp1 <- cbind.data.frame(c(months, 0), c(0, liq.breakpoints1)) # Make breakpoint from previous month correspond to each observation
liq.bp2 <- cbind.data.frame(c(months, 0), c(0, liq.breakpoints2)) # Make breakpoint from previous month correspond to each observation

mc.bp <- cbind.data.frame(c(months, 0),
                          c(0, mc1),
                          c(0, mc2),
                          c(0, mc3),
                          c(0, mc4),
                          c(0, mc5),
                          c(0, mc6),
                          c(0, mc7),
                          c(0, mc8),
                          c(0, mc9)) # Make breakpoint from previous month correspond to each observation

pb.bp <- cbind.data.frame(c(months, 0),
                          c(0, pb1),
                          c(0, pb2),
                          c(0, pb3),
                          c(0, pb4),
                          c(0, pb5),
                          c(0, pb6),
                          c(0, pb7),
                          c(0, pb8),
                          c(0, pb9)) # Make breakpoint from previous month correspond to each observation

names(liq.bp1)[names(liq.bp1) == 'c(0, liq.breakpoints1)'] <- 'Breakpoint'
names(liq.bp1)[names(liq.bp1) == 'c(months, 0)'] <- 'Month'
names(liq.bp2)[names(liq.bp2) == 'c(0, liq.breakpoints2)'] <- 'Breakpoint'
names(liq.bp2)[names(liq.bp2) == 'c(months, 0)'] <- 'Month'

names(mc.bp) <- c('Month', 'mc1', 'mc2', 'mc3', 'mc4', 'mc5', 'mc6', 'mc7', 'mc8', 'mc9')
names(pb.bp) <- c('Month', 'pb1', 'pb2', 'pb3', 'pb4', 'pb5', 'pb6', 'pb7', 'pb8', 'pb9')

crsp$bp1 <- liq.bp1$Breakpoint[match(crsp$Month, liq.bp1$Month)] #Add matching breakpoint for each month observation
crsp$bp2 <- liq.bp2$Breakpoint[match(crsp$Month, liq.bp2$Month)] #Add matching breakpoint for each month observation

crsp$mc1 <- mc.bp$mc1[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc2 <- mc.bp$mc2[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc3 <- mc.bp$mc3[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc4 <- mc.bp$mc4[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc5 <- mc.bp$mc5[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc6 <- mc.bp$mc6[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc7 <- mc.bp$mc7[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc8 <- mc.bp$mc8[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation
crsp$mc9 <- mc.bp$mc9[match(crsp$Month, mc.bp$Month)] #Add matching breakpoint for each month observation

crsp$pb1 <- pb.bp$pb1[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb2 <- pb.bp$pb2[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb3 <- pb.bp$pb3[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb4 <- pb.bp$pb4[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb5 <- pb.bp$pb5[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb6 <- pb.bp$pb6[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb7 <- pb.bp$pb7[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb8 <- pb.bp$pb8[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation
crsp$pb9 <- pb.bp$pb9[match(crsp$Month, pb.bp$Month)] #Add matching breakpoint for each month observation

# Determine mc decile
mc.dec1 <- ifelse(crsp$MktCap < crsp$mc1, 1, 0)
mc.dec2 <- ifelse(crsp$MktCap < crsp$mc2, 1, 0)
mc.dec3 <- ifelse(crsp$MktCap < crsp$mc3, 1, 0)
mc.dec4 <- ifelse(crsp$MktCap < crsp$mc4, 1, 0)
mc.dec5 <- ifelse(crsp$MktCap < crsp$mc5, 1, 0)
mc.dec6 <- ifelse(crsp$MktCap < crsp$mc6, 1, 0)
mc.dec7 <- ifelse(crsp$MktCap < crsp$mc7, 1, 0)
mc.dec8 <- ifelse(crsp$MktCap < crsp$mc8, 1, 0)
mc.dec9 <- ifelse(crsp$MktCap < crsp$mc9, 1, 0)
mc.dec <- 1 + mc.dec1 + mc.dec2 + mc.dec3 + mc.dec4 + mc.dec5 + mc.dec6 + mc.dec7 + mc.dec8 + mc.dec9

# Determine pb decile
pb.dec1 <- ifelse(crsp$PB < crsp$pb1, 1, 0)
pb.dec2 <- ifelse(crsp$PB < crsp$pb2, 1, 0)
pb.dec3 <- ifelse(crsp$PB < crsp$pb3, 1, 0)
pb.dec4 <- ifelse(crsp$PB < crsp$pb4, 1, 0)
pb.dec5 <- ifelse(crsp$PB < crsp$pb5, 1, 0)
pb.dec6 <- ifelse(crsp$PB < crsp$pb6, 1, 0)
pb.dec7 <- ifelse(crsp$PB < crsp$pb7, 1, 0)
pb.dec8 <- ifelse(crsp$PB < crsp$pb8, 1, 0)
pb.dec9 <- ifelse(crsp$PB < crsp$pb9, 1, 0)
pb.dec <- 1 + pb.dec1 + pb.dec2 + pb.dec3 + pb.dec4 + pb.dec5 + pb.dec6 + pb.dec7 + pb.dec8 + pb.dec9

crsp$MC.dec <- mc.dec
crsp$PB.dec <- pb.dec

day(monthliq$Month) <- 1 # Convert month ends to month beginnings for month manipulation
month(monthliq$Month) <- month(monthliq$Month) + 1 # Assign monthly liquidity to investment decision month (i.e. lag)
day(monthliq$Month) <- days_in_month(monthliq$Month) # Convert back to end of month
monthliq$Key <- paste(monthliq$Month, monthliq$Cusip, sep = '_') # Hash key for matching between data frames

crsp$ml <- monthliq$Liquidity[match(crsp$key, monthliq$Key)] # Add monthly liquidity to crsp

#'-----------------------------------------------------
#' RETURN DEPENDENT TRUE PRICE EVOLUTION WITH RE-INVESTMENTS
#'-----------------------------------------------------

crsp <- crsp %>% {arrange(., Cusip, Date)} # Arrange by month and date
TruePrice <- crsp %>% nrow() %>% numeric() # Initialize
cvec <- crsp %>% pull(Cusip) # pull for faster looping
pvec <- crsp %>% pull(Price) # pull for faster looping
rvec <- crsp %>% pull(Return) # pull for faster looping

TruePrice[1] <- crsp$Price[1]

for (i in 2:length(TruePrice)){
  if (cvec[i-1] == cvec[i]){TruePrice[i] <- TruePrice[i-1]*(1 + rvec[i])} else {TruePrice[i] <- pvec[i]}
}

crsp$TruePrice <- TruePrice # Add to data frame
crsp$LogTruePrice <- TruePrice %>% log() # Add to data frame

#'-----------------------------------------------------
#' EXPORT PROCESSED DATA
#'-----------------------------------------------------

#Get rid of unnecessary stuff
titles <- c('Date', 'Month', 'Cusip', 'NAIC', 'MC.dec', 'PB.dec', 'ml', 'bp1', 'bp2', 'TruePrice', 'LogTruePrice') #Select the relevant columns
crsp.out <- crsp %>% select(titles)
crsp.out <- subset(crsp.out, Month %>% year() != 2008)


dir <- 'C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Processed_Data'
setwd(dir)
stopifnot(all.equal(getwd(), dir))

write <- TRUE
if(write){fwrite(crsp.out, 'processed_data.csv')} # Export processed data
