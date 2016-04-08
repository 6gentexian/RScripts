CreateFedDataSeries.TAA <- function(StartingDate='1990-01-01')
{

require(fImport)

################################################################################
################################################################################
##  M2 Money Stock 	          M2 	      1980-11-03 	2011-07-04 	W 	Bil. of $ 	SA
##  M2 Money Stock 	          WM2NS 	 1980-11-03 	2011-07-04 	W 	Bil. of $ 	NSA
##  Non-M1 Components of M2 	NONM1 	  1980-11-03 	2011-07-04 	W 	Bil. of $ 	SA
##  Non-M1 Components of M2 	WNONM1NS 	1980-11-03 	2011-07-04 	W 	Bil. of $ 	NSA
################################################################################
################################################################################
                                   # StartingDate
    M2  <- fredImport("M2")
    M2  <- xts(M2@data[,"M2"], as.Date(as.character(time(M2@data[,"M2"]))))
    colnames(M2) <- c("M2"); Sys.sleep(1)
    
    M2.NSA  <- fredImport("WM2NS")
    M2.NSA  <- xts(M2.NSA@data[,"WM2NS"], as.Date(as.character(time(M2.NSA@data[,"WM2NS"]))))
    colnames(M2.NSA) <- c("M2.NSA")
        
    M2NoM1  <- fredImport("NONM1")
    M2NoM1  <- xts(M2NoM1@data[,"NONM1"], as.Date(as.character(time(M2NoM1@data[,"NONM1"]))))
    colnames(M2NoM1) <- c("M2NoM1"); Sys.sleep(1)
    
    M2NoM1.NSA  <- fredImport("WNONM1NS")
    M2NoM1.NSA  <- xts(M2NoM1.NSA@data[,"WNONM1NS"], as.Date(as.character(time(M2NoM1.NSA@data[,"WNONM1NS"]))))
    colnames(M2NoM1.NSA) <- c("M2NoM1.NSA")


M2Data <-cbind(M2, M2.NSA, M2NoM1, M2NoM1.NSA)
rm(M2, M2.NSA, M2NoM1, M2NoM1.NSA)
################################################################################
################################################################################



################################################################################
################################################################################
##  Consumer Price Index for All Urban Consumers: All Items 	                  CPIAUCSL 	1947-01 	2011-06 	M 	Index 1982-84=100 	SA
##  Consumer Price Index for All Urban Consumers: All Items                     CPIAUCNS 	1913-01 	2011-06 	M 	Index 1982-84=100 	NSA

##  Consumer Price Index for All Urban Consumers: All Items Less Food & Energy 	CPILFESL 	1957-01 	2011-06 	M 	Index 1982-84=100 	SA
##  Consumer Price Index for All Urban Consumers: All Items Less Food & Energy 	CPILFENS 	1957-01 	2011-06 	M 	Index 1982-84=100 	NS

##  University of Michigan Inflation Expectation 	                              MICH 	1978-01 	2011-07 	M

    CPI  <- fredImport("CPIAUCSL")
    CPI  <- xts(CPI@data[,"CPIAUCSL"], as.Date(as.character(time(CPI@data[,"CPIAUCSL"]))))
    colnames(CPI) <-("CPI"); Sys.sleep(1)

    CPI.NSA  <- fredImport("CPIAUCNS")
    CPI.NSA  <- xts(CPI.NSA@data[,"CPIAUCNS"], as.Date(as.character(time(CPI.NSA@data[,"CPIAUCNS"]))))
    colnames(CPI.NSA) <- c("CPI.NSA")  

    CPIExFE  <- fredImport("CPILFESL")
    CPIExFE  <- xts(CPIExFE@data[,"CPILFESL"], as.Date(as.character(time(CPIExFE@data[,"CPILFESL"]))))
    colnames(CPIExFE) <- c("CPIExFE"); Sys.sleep(1)

    CPIExFE.NSA  <- fredImport("CPILFENS")
    CPIExFE.NSA  <- xts(CPIExFE.NSA@data[,"CPILFENS"], as.Date(as.character(time(CPIExFE.NSA@data[,"CPILFENS"]))))
    colnames(CPIExFE.NSA) <- c("CPIExFE.NSA")  

    UMICH.InflExp  <- fredImport("MICH")
    UMICH.InflExp  <- xts(UMICH.InflExp@data[,"MICH"], as.Date(as.character(time(UMICH.InflExp@data[,"MICH"]))))
    colnames(UMICH.InflExp) <- c("UMICH.InflExp"); Sys.sleep(1)

CPIData <-cbind(CPI, CPI.NSA, CPIExFE, CPIExFE.NSA, UMICH.InflExp)
rm(CPI, CPI.NSA, CPIExFE, CPIExFE.NSA, UMICH.InflExp)
################################################################################
################################################################################



################################################################################
################################################################################
##	ISM Manufacturing: PMI Composite Index 	         NAPM    1948-01 	2011-06 	M 	Index 	SA
##	ISM Manufacturing: New Orders Index              NAPMNOI 1948-01 	2011-06 	M 	Index 	SA
##	ISM Manufacturing: Prices Index                  NAPMPRI 1948-01 	2011-06 	M 	Index 	NSA
##	ISM Manufacturing: Production Index              NAPMPI  1948-01 	2011-06 	M 	Index 	SA
##	ISM Manufacturing: Inventories Index             NAPMII  1948-01 	2011-06 	M 	Index 	SA
################################################################################
################################################################################

    NAPM  <- fredImport("NAPM")
    NAPM  <- xts(NAPM@data[,"NAPM"], as.Date(as.character(time(NAPM@data[,"NAPM"]))))
    colnames(NAPM) <- c("NAPM"); Sys.sleep(1)
    
    NAPM.BusAct  <- fredImport("NMFBAI")
    NAPM.BusAct  <- xts(NAPM.BusAct@data[,"NMFBAI"], as.Date(as.character(time(NAPM.BusAct@data[,"NMFBAI"]))))
    colnames(NAPM.BusAct) <-c("NAPM.BusAct")
    
    NAPM.NOI  <- fredImport("NAPMNOI")
    NAPM.NOI  <- xts(NAPM.NOI@data[,"NAPMNOI"], as.Date(as.character(time(NAPM.NOI@data[,"NAPMNOI"]))))
    colnames(NAPM.NOI) <-c("NAPM.NOI"); Sys.sleep(1)
    
    NAPM.Prices  <- fredImport("NAPMPRI")
    NAPM.Prices  <- xts(NAPM.Prices@data[,"NAPMPRI"], as.Date(as.character(time(NAPM.Prices@data[,"NAPMPRI"]))))
    colnames(NAPM.Prices) <-c("NAPM.Prices")

    NAPM.Prodn  <- fredImport("NAPMPI")
    NAPM.Prodn  <- xts(NAPM.Prodn@data[,"NAPMPI"], as.Date(as.character(time(NAPM.Prodn@data[,"NAPMPI"]))))
    colnames(NAPM.Prodn) <-c("NAPM.Prodn"); Sys.sleep(1)

    NAPM.Invt  <- fredImport("NAPMII")
    NAPM.Invt  <- xts(NAPM.Invt@data[,"NAPMII"], as.Date(as.character(time(NAPM.Invt@data[,"NAPMII"]))))
    colnames(NAPM.Invt) <-c("NAPM.Invt")


NAPMData <-cbind(NAPM, NAPM.BusAct, NAPM.NOI, NAPM.Prices, NAPM.Prodn, NAPM.Invt)
rm(NAPM, NAPM.BusAct, NAPM.NOI, NAPM.Prices, NAPM.Prodn, NAPM.Invt)
################################################################################
################################################################################
##  Inventory to Sales Ratio: Total Business 	        ISRATIO 	1992-01 	2011-06 	M 	Ratio 	SA 	 

    invtSales  <- fredImport("ISRATIO")
    invtSales  <- xts(invtSales@data[,"ISRATIO"], as.Date(as.character(time(invtSales@data[,"ISRATIO"]))))
    colnames(invtSales) <-c("invtSales"); Sys.sleep(1)

################################################################################
################################################################################
##  Industrial Production Index 	                        INDPRO 	1919-01 	2011-06 	M 	Index 2007=100 	SA 	 
##  Industrial Production: Durable Manufacturing (NAICS) 	IPDMAN 	1972-01 	2011-06 	M 	Index 2007=100 	SA 	 
##  Industrial Production: Manufacturing (NAICS) 	        IPMAN 	1972-01 	2011-06 	M 	Index 2007=100 	SA 	 


    IP  <- fredImport("INDPRO")
    IP  <- xts(IP@data[,"INDPRO"], as.Date(as.character(time(IP@data[,"INDPRO"]))))
    colnames(IP) <-c("IP")
 
#    IP.DurMan  <- fredImport("IPDMAN")
#    IP.DurMan  <- xts(IP.DurMan@data[,"IPDMAN"], as.Date(as.character(time(IP.DurMan@data[,"IPDMAN"]))))
#    colnames(IP.DurMan) <-c("IP.DurMan")       

    IP.Man  <- fredImport("IPMAN")
    IP.Man  <- xts(IP.Man@data[,"IPMAN"], as.Date(as.character(time(IP.Man@data[,"IPMAN"]))))
    colnames(IP.Man) <-c("IP.Man"); Sys.sleep(1)
    
    
IPData <-cbind(IP, IP.Man)
rm(IP, IP.Man)
################################################################################
################################################################################
##  Crude Oil: West Texas Intermediate (WTI) - Cushing, Oklahoma 	DCOILWTICO 	1986-01-02 	2011-08-09 	D 	$ per Barrel 	NA 	 
##  Crude Oil: Brent - Europe 	                                  DCOILBRENTEU 	1987-05-20 	2011-08-09 	D 	

    OIL  <- fredImport("DCOILWTICO")
    OIL  <- xts(OIL@data[,"DCOILWTICO"], as.Date(as.character(time(OIL@data[,"DCOILWTICO"]))))
    colnames(OIL) <-c("OIL"); Sys.sleep(1)

################################################################################
################################################################################
##  Leading Index for the United States 	                    USSLIND 	1982-01 	2011-06 	M
##  Coincident Economic Activity Index for the United States 	USPHCI 	  1979-01 	2011-06 	M 	Jul 1992=100 	NA

    leadIndexUS  <- fredImport("USSLIND")
    leadIndexUS  <- xts(leadIndexUS@data[,"USSLIND"], as.Date(as.character(time(leadIndexUS@data[,"USSLIND"]))))
    colnames(leadIndexUS) <-c("leadIndexUS") 

    coinIndexUS  <- fredImport("USPHCI")
    coinIndexUS  <- xts(coinIndexUS@data[,"USPHCI"], as.Date(as.character(time(coinIndexUS@data[,"USPHCI"]))))
    colnames(coinIndexUS) <-c("coinIndexUS"); Sys.sleep(1)

################################################################################
################################################################################
	 
##  Chicago Fed National Activity Index 	                                    CFNAI 	  1967-03 	2011-06 	M 	Index 	NA 	 
##  Chicago Fed National Activity Index: Employment, Unemployment and Hours 	EUANDH 	1967-03 	2011-06 	M 	Index 	NA 	 
##  Chicago Fed National Activity Index: Personal Consumption and Housing 	  CANDH 	1967-03 	2011-06 	M 	Index 	NA 	 
##  Chicago Fed National Activity Index: Production and Income 	              PANDI 	  1967-03 	2011-06 	M 	Index 	NA 	 
##  Chicago Fed National Activity Index: Sales, Orders and Inventories 	      SOANDI 	  1967-03 	2011-06 	M 	Index 	NA 	 


    CFNAI  <- fredImport("CFNAI")
    CFNAI  <- xts(CFNAI@data[,"CFNAI"], as.Date(as.character(time(CFNAI@data[,"CFNAI"]))))
    colnames(CFNAI) <-c("CFNAI"); Sys.sleep(1)

    salesOrdInvt  <- fredImport("SOANDI")
    salesOrdInvt  <- xts(salesOrdInvt@data[,"SOANDI"], as.Date(as.character(time(salesOrdInvt@data[,"SOANDI"]))))
    colnames(salesOrdInvt) <-c("salesOrdInvt")


################################################################################
################################################################################


################################################################################
################################################################################
##  Value of Manufacturers' New Orders for All Manufacturing Industries 	      AMTMNO 	1992-02 	2011-06 	M 	Mil. of $ 	SA 	 
##  Value of Manufacturers' New Orders for All Manufacturing Industries 	      UMTMNO 	1992-02 	2011-06 	M 	Mil. of $ 	NSA 	 

    NOAllMan  <- fredImport("AMTMNO")
    NOAllMan  <- xts(NOAllMan@data[,"AMTMNO"], as.Date(as.character(time(NOAllMan@data[,"AMTMNO"]))))
    colnames(NOAllMan) <-c("NOAllMan"); Sys.sleep(1)

    NOAllMan.NSA  <- fredImport("UMTMNO")
    NOAllMan.NSA  <- xts(NOAllMan.NSA@data[,"UMTMNO"], as.Date(as.character(time(NOAllMan.NSA@data[,"UMTMNO"]))))
    colnames(NOAllMan.NSA) <-c("NOAllMan.NSA")

################################################################################
################################################################################
## Moody's Seasoned Baa Corporate Bond Yield 	DBAA 	1986-01-02 	2011-08-10 	D 	% 	NA 	


    BAA  <- fredImport("DBAA")
    BAA  <- xts(BAA@data[,"DBAA"], as.Date(as.character(time(BAA@data[,"DBAA"]))))
    colnames(BAA) <-c("BAA"); Sys.sleep(1)


################################################################################
################################################################################
## 3-Month Treasury Constant Maturity Rate 	DGS3MO 	1982-01-04 	2011-08-10 	D 	% 	NA 	 
## 2-Year Treasury Constant Maturity Rate 	DGS2 	1976-06-01 	2011-08-10 	D 	% 	NA 	 
## 10-Year Treasury Constant Maturity Rate 	DGS10 	1962-01-02 	2011-08-10 	D 	% 	NA 	 


    USTCM.2YR  <- fredImport("DGS2")
    USTCM.2YR  <- xts(USTCM.2YR@data[,"DGS2"], as.Date(as.character(time(USTCM.2YR@data[,"DGS2"]))))
    colnames(USTCM.2YR) <-c("USTCM.2YR"); Sys.sleep(1)

    USTCM.3M  <- fredImport("DGS3MO")
    USTCM.3M  <- xts(USTCM.3M@data[,"DGS3MO"], as.Date(as.character(time(USTCM.3M@data[,"DGS3MO"]))))
    colnames(USTCM.3M) <-c("USTCM.3M")

    USTCM.10YR  <- fredImport("DGS10")
    USTCM.10YR   <- xts(USTCM.10YR @data[,"DGS10"], as.Date(as.character(time(USTCM.10YR @data[,"DGS10"]))))
    colnames(USTCM.10YR ) <-c("USTCM.10YR"); Sys.sleep(1)


################################################################################
################################################################################

##  consolidate data

################################################################################
################################################################################

monthlyData <- cbind(CPIData, NAPMData, IPData
                    , invtSales, leadIndexUS, coinIndexUS
                    , CFNAI, salesOrdInvt
                    , NOAllMan, NOAllMan.NSA)
                   

weeklyData  <- cbind(M2Data)


dailyData   <- cbind(OIL, BAA, USTCM.3M, USTCM.2YR, USTCM.10YR)


rm(CPIData, NAPMData, IPData, invtSales)
rm(leadIndexUS, coinIndexUS, CFNAI, salesOrdInvt)
rm(NOAllMan, NOAllMan.NSA, M2Data)
rm(OIL, BAA, USTCM.3M, USTCM.2YR, USTCM.10YR)



output <-list(monthly=monthlyData
            , weekly=weeklyData
            , daily=dailyData
            )


return(output)

}
