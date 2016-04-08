CreateBBDataSeries.TAA <- function(progPath, StartingDate='1990-01-01')
{
################################################################################
####                    TURN OFF ALL GRAPHICAL DEVICES                      ####
################################################################################
for (ii in 1:length(dev.list()))
{
       graphics.off()
}
################################################################################
rm(ii)

################################################################################
## call python to create USGG, FX data 
system(paste("python ", progPath, "CreateBBData.py", sep=""))
################################################################################
BBData = read.table('TAA_LAST.txt',fill = T)
BBData[which(BBData$V3 == "N/A"),3] = NA

nVars   <-unique(BBData$V1)
allData <-NULL


  for(nn in seq_along(nVars))
  {
    temp  <-NULL
    temp  <- BBData[which(BBData$V1 == nVars[nn]), ]

      temp$V2 <-as.character(temp$V2)
      temp$V3 <-as.numeric(temp$V3)

    xData <- xts(as.numeric(temp$V3), as.Date(as.character(temp[, 2]), format="%m/%d/%Y"))
    colnames(xData) <- nVars[nn]
    
    if (length(which(is.na(xData))) > 1)
    {
      cat("\n", "\n", "# obs missing for ", "\t", colnames(xData)
        , "\t", length(which(is.na(xData)))
        , "\n", "\n")
    }
    
    xData <- na.locf(xData, na.rm = FALSE)
    xData = xData[!duplicated(index(xData)), ]
    allData <-cbind(allData, xData)

    rm(xData, temp)
  }
  rm(nn)

allData <-allData[, order(colnames(allData))]
allData = allData[which(index(allData)>=StartingDate), ]

  print(head(allData))
  print(tail(allData))

rm(BBData, nVars, StartingDate)
################################################################################

#################################################################################
#BBOpen = read.table('TAA_OPEN.txt',fill = T)
#BBOpen[which(BBOpen$V3 == "N/A"),3] = NA
#
#BBHigh = read.table('TAA_HIGH.txt',fill = T)
#BBHigh[which(BBHigh$V3 == "N/A"),3] = NA
#
#BBLow = read.table('TAA_LOW.txt',fill = T)
#BBLow[which(BBLow$V3 == "N/A"),3] = NA
#
#BBYC = read.table('TAA_YESTCLOSE.txt',fill = T)
#BBYC[which(BBYC$V3 == "N/A"),3] = NA
#
#
#CurVars <-setdiff(unique(BBOpen$V1), unique(BBOpen$V1)[grep("USGG", unique(BBOpen$V1))])
#CurData <-NULL
#
#
#  for(nn in seq_along(CurVars))
#  {
#    temp  <-NULL
#    temp  <- BBYC[which(BBYC$V1 == CurVars[nn]), ]
#
#      temp$V2 <-as.character(temp$V2)
#      temp$V3 <-as.numeric(temp$V3)
#
#    xData <- xts(as.numeric(temp$V3), as.Date(as.character(temp[, 2]), format="%m/%d/%Y"))
#    colnames(xData) <- YldIndexVars[nn]
#    
#    if (length(which(is.na(xData))) > 1)
#    {
#      cat("\n", "\n", "# obs missing for ", "\t", colnames(xData)
#        , "\t", length(which(is.na(xData)))
#        , "\n", "\n")
#    }
#    
#    xData <- na.locf(xData, na.rm = FALSE)
#    xData = xData[!duplicated(index(xData)), ]
#    rateIndexData <-cbind(rateIndexData, xData)
#
#    rm(xData, temp)
#  }
#  rm(nn)
#
#rateIndexData <-rateIndexData[, c("USGG3M", "USGG2YR", "USGG10YR", "USGG30YR")]
#rateIndexData = rateIndexData[which(index(rateIndexData)>=StartingDate), ]
#
#  print(head(rateIndexData))
#  print(tail(rateIndexData))



write.csv(as.matrix(allData),"FundData.TAA.csv")

return(allData)

}

################################################################################
############                      EOF                           ################
################################################################################


































