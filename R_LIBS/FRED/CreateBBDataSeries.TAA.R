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


write.csv(as.matrix(allData),"FundData.TAA.csv")

return(allData)

}

################################################################################
############                      EOF                           ################
################################################################################


































