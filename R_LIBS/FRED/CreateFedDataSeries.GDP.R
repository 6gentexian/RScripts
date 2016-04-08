CreateFedDataSeries.GDP <- function(StartingDate='1990-01-01')
{
################################################################################
###########################            GDP           ###########################
################################################################################

require(fImport)
require(xts)

################################################################################
################################################################################
##  Disposable Personal Income 	            DSPI 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
##  Disposable Personal Income: Per capita: Chained (2005) Dollars 	A229RX0 	1959-01 	2011-08 	M 	Chn. 2005 $ 	SAAR 	 
##  Disposable Personal Income: Per capita: Current dollars 	      A229RC0 	1959-01 	2011-08 	M 	$ 	SAAR 	 
##
##  Real Personal Income 	                  RPI 	1959-01 	2011-08 	M 	Bil. of Chn. 2005 $ 	SAAR
##  Real Disposable Personal Income 	      DSPIC96 	1959-01 	2011-08 	M 	Bil. of Chn. 2005 $ 	SAAR 	 
##  Real personal income excluding current transfer receipts 	W875RX1 	1959-01 	2011-08 	M 	Bil. of Chn. 2005 $ 	SAAR
##
##  Real Personal Consumption Expenditures 	PCEC96 	1995-01 	2011-08 	M 	Bil. of Chn. 2005 $ 	SAAR 	 
##
##  Government 	                            B202RC1 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
##
##  Personal Consumption Expenditures 	    PCE 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
##  Personal current taxes 	                W055RC1 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
##  Personal current transfer payments 	    W211RC1 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
##  Personal Current Transfer Receipts 	    PCTR 	1959-01 	2011-08 	M 	Bil. of $ 	SAAR 	 
################################################################################
################################################################################
 
    realPersInc  <- fredImport("RPI")
    realPersInc  <- xts(realPersInc@data[,"RPI"], as.Date(as.character(time(realPersInc@data[,"RPI"]))))
    colnames(realPersInc) <- c("realPersInc"); Sys.sleep(1)
    
    realDispPersInc  <- fredImport("DSPIC96")
    realDispPersInc  <- xts(realDispPersInc@data[,"DSPIC96"], as.Date(as.character(time(realDispPersInc@data[,"DSPIC96"]))))
    colnames(realDispPersInc) <- c("realDispPersInc")

    realPersIncExTrans  <- fredImport("W875RX1")
    realPersIncExTrans  <- xts(realPersIncExTrans@data[,"W875RX1"], as.Date(as.character(time(realPersIncExTrans@data[,"W875RX1"]))))
    colnames(realPersIncExTrans) <- c("realPersIncExTrans")

    realConsume <- fredImport("PCEC96")
    realConsume  <- xts(realConsume@data[,"PCEC96"], as.Date(as.character(time(realConsume@data[,"PCEC96"]))))
    colnames(realConsume) <- c("realConsume")

################################################################################
################################################################################ 


        
allCredit <-cbind(realPersInc, realDispPersInc, realPersIncExTrans, realConsume)
 


rm(realPersInc, realDispPersInc, realPersIncExTrans, realConsume)



return(allCredit)

}

################################################################################
################################################################################

####################        EOF Business - Fiscal      #########################

################################################################################
################################################################################


