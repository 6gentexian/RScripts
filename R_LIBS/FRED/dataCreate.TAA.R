################################################################################
##  data creation
################################################################################
#     setwd(paste(dataPath, 'TXT', sep=''))
     source('D:\\Projects\\TAA\\CreateBBDataSeries.TAA.R')
     bbData <-CreateBBDataSeries.TAA(progPath)

     ###########################################################################
      
      bbData$Slope.30Y3M <- (bbData$USGG30YR - bbData$USGG3M)
      bbData$Slope.30Y2Y <- (bbData$USGG30YR - bbData$USGG2YR)
      bbData$Slope.10Y3M <- (bbData$USGG10YR - bbData$USGG3M)
      bbData$Slope.10Y2Y <- (bbData$USGG10YR - bbData$USGG2YR)
      bbData$Slope.2Y3M  <- (bbData$USGG2YR  - bbData$USGG3M)

varsToModify <-colnames(bbData)[c(grep("AUD", toupper(colnames(bbData)))
                                , grep("CAD", toupper(colnames(bbData)))
                                , grep("CHF", toupper(colnames(bbData)))
                                , grep("DXY", toupper(colnames(bbData)))
                                , grep("EUR", toupper(colnames(bbData)))
                                , grep("JPY", toupper(colnames(bbData)))
                                )]


for(mm in seq_along(varsToModify))
{
  bbData[, varsToModify[mm]] <- na.locf(bbData[, varsToModify[mm]], na.rm = FALSE)
}

##  last row is carried over from the last true historical obs
bbData <-bbData[-nrow(bbData), ]

     ###########################################################################
     source('D:\\Projects\\TAA\\CreateFedDataSeries.TAA.R')
     FedData <-CreateFedDataSeries.TAA()
     ###########################################################################
     

rm(mm, varsToModify)
################################################################################
################################################################################

     ###########################################################################
     source('D:\\Projects\\TAA\\dataCreate.TreasPriceDur.TAA.R')
     ###########################################################################

################################################################################
################################################################################


#################################################################################
##  merge and create quarterly and monthly datasets
dData <-cbind(bbData, tyData, tuData, durData, FedData$daily)

temp <-dData
temp <-temp[which(index(temp)>=StartingDate)]
varsToModify <-colnames(temp)[c(grep("OIL", toupper(colnames(temp)))
                                , grep("BAA", toupper(colnames(temp)))
                                , grep("USTCM", toupper(colnames(temp)))
                                )]


for(mm in seq_along(varsToModify))
{
  temp[, varsToModify[mm]] <- na.locf(temp[, varsToModify[mm]], na.rm = FALSE)
}
rm(mm)



colnames(temp)[grep("TY.Close", colnames(temp))] <-c("TY")
colnames(temp)[grep("TU.Close", colnames(temp))] <-c("TU")


temp.names <-colnames(temp)

print(last(temp[which(is.na(temp$USGG10YR)), 1:8], 4))
##  missing data is before the TY series starts or holidays, 9/11
#1993-11-11        NA        NA       NA       NA    NA
#2000-07-04       109       110      107      107    NA
#2001-09-12       123       126      119      117 107.1
#2005-12-26       145       145      144      140    NA

temp <-temp[which(!is.na(temp$USGG10YR)), ]
################################################################################
##  merge in weekly data and then lag each series three weeks - 15 bus days
temp <-cbind(temp, FedData$weekly)

wkNames <-unique(setdiff(colnames(temp), temp.names))
for(nn in seq_along(wkNames))
{
    temp[, wkNames[nn]] <-lag(temp[, wkNames[nn]], k=15)
}

temp.names <-colnames(temp)  
################################################################################
##  merge in monthly data and then lag each series two weeks
temp <-cbind(temp, FedData$monthly)

temp <-temp[which(index(temp)>=StartingDate)]


mNames <-unique(setdiff(colnames(temp), temp.names))

##  IP and CPIU lag 35 business days - 1.5 months
##  NAPM lag 25 business days - 1.1 months
for(nn in seq_along(mNames))
{
    if (is.element(mNames[nn], colnames(temp)[grep("CPI", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=35)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("IP", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=35)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("NAPM", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=25)
    }    

    if (is.element(mNames[nn], colnames(temp)[grep("UMICH", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=15)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("CFNAI", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=25)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("SALESORD", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=25)
    }
    
    if (is.element(mNames[nn], colnames(temp)[grep("NOMAN", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=30)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("INDEX", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=45)
    }
    if (is.element(mNames[nn], colnames(temp)[grep("INVTSALES", toupper(colnames(temp)))]) )
    {
      temp[, mNames[nn]] <-lag(temp[, mNames[nn]], k=35)
    }
}
rm(nn)





varsToModify <-colnames(temp)[c(grep("M2", toupper(colnames(temp)))
                                , grep("NAPM", toupper(colnames(temp)))
                                , grep("CPI", toupper(colnames(temp)))
                                
                                , grep("IP", toupper(colnames(temp)))
                                , grep("SLOPE", toupper(colnames(temp)))
                                , grep("USGG", toupper(colnames(temp)))

                                , grep("UMICH", toupper(colnames(temp)))
                                , grep("CFNAI", toupper(colnames(temp)))
                                , grep("SALESORD", toupper(colnames(temp)))
                                
                                , grep("NOALLMAN", toupper(colnames(temp)))
                                , grep("INDEX", toupper(colnames(temp)))
                                , grep("INVTSALES", toupper(colnames(temp)))
                                )]
                                
                                 
for(mm in seq_along(varsToModify)) 
{
  temp[, varsToModify[mm]] <- na.locf(temp[, varsToModify[mm]], na.rm = FALSE)
}
rm(mm)

  
##   TRANSFORM TO PROPER MONTHLY DATA

##  idea - on the day that the gummint releases IP data


##  check work and make sure that there is no more than ~ 34 days b/t obs
#cat(print(which(diff(index(mData)) > 33)), "\n")


#mData <-temp
#mData <-as.data.frame(mData)
#
#mData$day  <-weekdays(index(temp))
#mData$year <-years(index(temp))
#mData$month<-month(index(temp))
#
#mData$period<-c(0)
#
#for(yy in seq_along(unique(mData$year)))
#
#  for(mm in seq_along(unique(mData$month)))
#
#    sample <-mData[which(mData$year == yy & mData$month == mm),]


mData <-NULL



  for (col in 1:ncol(temp))   
  {
      sample <- to.monthly(temp[ ,col], indexAt='endof')
      colnames(sample) <-c("O", "H", "L", "Close")

      sample <-sample[,"Close"]
      colnames(sample)<-NULL

      mData <-cbind(mData, sample)
      rm(sample)
  }
  rm(col)

colnames(mData) <-colnames(temp)
mData <- as.xts(mData, as.Date(rownames(mData), format='%Y-%m-%d'))

mData <- mData[, -match(c("NAPM.BusAct"), colnames(mData))]
mData <-mData[which(!is.na(mData$IP)), ]

cat(print(which(diff(index(mData)) > 33)), "\n")



##   TRANSFORM TO QUARTER DATA
qData <-NULL

  for (col in 1:ncol(temp))   {
      sample <- to.quarterly(temp[ ,col], indexAt='endof')
      colnames(sample) <-c("O", "H", "L", "Close")

      sample <-sample[,"Close"]
      colnames(sample)<-NULL

      qData <-cbind(qData, sample)
      rm(sample)
  }
  rm(col)

colnames(qData) <-colnames(temp)
qData <- as.xts(qData, as.Date(rownames(qData), format='%Y-%m-%d'))


qData <- qData[, -match(c("NAPM.BusAct"), colnames(qData))]
qData <-qData[which(!is.na(qData$IP)), ]


cat(print(which(diff(index(qData)) > 94)), "\n")


rm(mNames, temp, temp.names, wkNames, varsToModify)
################################################################################

################################################################################

qData$TY.rets <- diff(log(qData$TY))
mData$TY.rets <- diff(log(mData$TY))

qData$TY.rets.F1 <- lag(qData$TY.rets, k=-1)
mData$TY.rets.F1 <- lag(mData$TY.rets, k=-1)

################################################################################

qData$TU.rets <- diff(log(qData$TU))
mData$TU.rets <- diff(log(mData$TU))

qData$TU.rets.F1 <- lag(qData$TU.rets, k=-1)
mData$TU.rets.F1 <- lag(mData$TU.rets, k=-1)

################################################################################


save(bbData, FedData, dData, mData, qData, tyData, tuData, durData, file='TAA.RData')


################################################################################
##                                 EOF                                        ##
################################################################################


