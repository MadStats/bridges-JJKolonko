#Import Data Set into R
#Downloaded data set to computer and moved it to the working directory
#install.packages("tidyverse")
#library(tidyverse)
NinetySix <- read_csv("fluna_991996-20160914145707.txt") #bridges all 50 states, year: 1996


#Find WI bridges
tab <- table(NinetySix[,1]) #Wisconsin's state code is 55, can be found on page 1 here: https://www.fhwa.dot.gov/bridge/mtguide.pdf
numWiBdg <- as.numeric(tab[50]) #Wisconsin is the 50th listed territory (includes DC and PR)

#Make a file with bridge ID, year, fips codes, condition ratings, and few other variables

#bridge ID -> make from combining latitude and longitude values
lat <- vector(mode = "character", length = numWiBdg) #latitude
long <- vector(mode = "character", length = numWiBdg) #longitude
e = 1
r = 1
for (e in 1:nrow(NinetySix)) {
  if (NinetySix[e,1] == "55") {
    lat[r] <- NinetySix$LAT_016[e]
    long[r] <- NinetySix$LONG_017[e]
    r=r+1
  }
  e=e+1
}
bridgeID <- paste(lat, long, sep = "")


#Make the fips codes
stCode <- vector(mode = "character", length = numWiBdg) #State of Wisconsin in 1996 (NS->NinetySix)
cntyCode <- vector(mode = "character", length = numWiBdg) #Counties of Wisconsin in 1996
q = 1
w = 1
for (q in 1:nrow(NinetySix)) {
  if (NinetySix[q,1] == "55") {
    stCode[w] <- NinetySix$STATE_CODE_001[q]
    cntyCode[w] <- NinetySix$COUNTY_CODE_003[q]
    w=w+1
  }
  q=q+1
}
fips <- paste(stCode, cntyCode, sep = "")

#Conditions ratings, items 58-62
deck <- vector(mode = "character", length = numWiBdg) #deck condition
sprsrctsr <- vector(mode = "character", length = numWiBdg) #superstructure condition
sbsrctsr <- vector(mode = "character", length = numWiBdg) #substructure condition
chnnl <- vector(mode = "character", length = numWiBdg) #channel and channel protection
culverts <- vector(mode = "character", length = numWiBdg) #culverts condition
p = 1
u = 1
for (p in 1:nrow(NinetySix)) {
  if (NinetySix[p,1] == "55") {
    deck[u] <- NinetySix$DECK_COND_058[p]
    sprsrctsr[u] <- NinetySix$SUPERSTRUCTURE_COND_059[p]
    sbsrctsr[u] <- NinetySix$SUBSTRUCTURE_COND_060[p]
    chnnl[u] <- NinetySix$CHANNEL_COND_061[p]
    culverts[u] <- NinetySix$CULVERT_COND_062[p]
    u=u+1
  }
  p=p+1
}

#Other variables
tolls <- vector(mode = "character", length = numWiBdg) #type of toll road or non, item 20
yrblt <- vector(mode = "character", length = numWiBdg) #year built, item 27
historical <- vector(mode = "character", length = numWiBdg) #historical significance, item 37
adtt <- vector(mode = "character", length = numWiBdg) #average daily truck traffic, item 109
a = 1
s = 1
for (p in 1:nrow(NinetySix)) {
  if (NinetySix[a,1] == "55") {
    tolls[s] <- NinetySix$TOLL_020[a]
    yrblt[s] <- NinetySix$YEAR_BUILT_027[a]
    historical[s] <- NinetySix$HISTORY_037[a]
    adtt[s] <- NinetySix$PERCENT_ADT_TRUCK_109[a]
    s=s+1
  }
  a=a+1
}

srfcprtctnOne <- vector(mode = "character", length = numWiBdg) #surface protection, 3 parts, item 108
srfcprtctnTwo <- vector(mode = "character", length = numWiBdg) #surface protection, 3 parts, item 108
srfcprtctnThree <- vector(mode = "character", length = numWiBdg) #surface protection, 3 parts, item 108
d = 1
f = 1
for (d in 1:nrow(NinetySix)){
  if (NinetySix[d,1] == "55") {
    srfcprtctnOne[f] <- NinetySix$SURFACE_TYPE_108A[d]
    srfcprtctnTwo[f] <- NinetySix$MEMBRANE_TYPE_108B[d]
    srfcprtctnThree[f] <- NinetySix$DECK_PROTECTION_108C[d]
    f=f+1
  }
  d=d+1
}
srfcprtctn <- paste(srfcprtctnOne, srfcprtctnTwo, srfcprtctnThree, sep = "") #surface protetion in one variable, but digits independent

#Make the new data frame with desired factors
wiNS <- data.frame(bridgeID, fips, deck, sprsrctsr, sbsrctsr, chnnl, culverts, tolls, yrblt, historical, adtt, srfcprtctn) #Wisconsin Bridges 1996



