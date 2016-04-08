CreateFedDataSeries.BusFiscal <- function(StartingDate='1990-01-01')
{
################################################################################
######################        Business - Fiscal      ###########################
################################################################################

require(fImport)
require(xts)

################################################################################
################################################################################
##	Total Consumer Credit Outstanding 	TOTALSL 	1943-01 	2011-07 	M 	Bil. of $ 	SA 	 
##	Total Consumer Credit Outstanding 	TOTALNS 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################
 
#    consCredit  <- fredImport("TOTALSL")
#    consCredit  <- xts(consCredit@data[,"TOTALSL"], as.Date(as.character(time(consCredit@data[,"TOTALSL"]))))
#    colnames(consCredit) <- c("consCredit"); Sys.sleep(1)
#    
#    consCredit.NSA  <- fredImport("TOTALNS")
#    consCredit.NSA  <- xts(consCredit.NSA@data[,"TOTALNS"], as.Date(as.character(time(consCredit.NSA@data[,"TOTALNS"]))))
#    colnames(consCredit.NSA) <- c("consCredit.NSA")

################################################################################

##	Bank Credit of All Commercial Banks 	TOTBKCR 	1973-01-03 	2011-09-14 	W 	Bil. of $ 	SA 	 

################################################################################
##	Total Revolving Credit Outstanding 	REVOLSL 	1968-01 	2011-07 	M 	Bil. of $ 	SA 	 
##	Total Revolving Credit Outstanding 	REVOLNS 	1968-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################
        
#    revCredit  <- fredImport("REVOLSL")
#    revCredit  <- xts(revCredit@data[,"REVOLSL"], as.Date(as.character(time(revCredit@data[,"REVOLSL"]))))
#    colnames(revCredit) <- c("revCredit"); Sys.sleep(1)
#    
#    revCredit.NSA  <- fredImport("REVOLNS")
#    revCredit.NSA  <- xts(revCredit.NSA@data[,"REVOLNS"], as.Date(as.character(time(revCredit.NSA@data[,"REVOLNS"]))))
#    colnames(revCredit.NSA) <- c("revCredit.NSA")

################################################################################
##	Total Nonrevolving Credit Outstanding 	NONREVSL 	1943-01 	2011-07 	M 	Bil. of $ 	SA 	 
##	Total Nonrevolving Credit Outstanding 	NONREVNS 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################

#    nonRevCredit  <- fredImport("NONREVSL")
#    nonRevCredit  <- xts(nonRevCredit@data[,"NONREVSL"], as.Date(as.character(time(nonRevCredit@data[,"NONREVSL"]))))
#    colnames(nonRevCredit) <- c("nonRevCredit"); Sys.sleep(1)
#    
#    nonRevCredit.NSA  <- fredImport("NONREVNS")
#    nonRevCredit.NSA  <- xts(nonRevCredit.NSA@data[,"NONREVNS"], as.Date(as.character(time(nonRevCredit.NSA@data[,"NONREVNS"]))))
#    colnames(nonRevCredit.NSA) <- c("nonRevCredit.NSA")

################################################################################
##	Total Consumer Loans Owned by Federal Government and Sallie Mae 	TOTALGOV 	1977-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Total Consumer Loans Owned by Commercial Banks         TOTALCB  	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Total Consumer Loans Owned by Credit Unions            TOTALTCU 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Total Consumer Loans Owned by Savings Institutions     TOTALSAV 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Total Consumer Loans Owned by Finance Companies        TOTALFC  	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Total Consumer Loans Owned by Nonfinancial Businesses  TOTALNFC 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################

#    govtCredit  <- fredImport("TOTALGOV")
#    govtCredit  <- xts(govtCredit@data[,"TOTALGOV"], as.Date(as.character(time(govtCredit@data[,"TOTALGOV"]))))
#    colnames(govtCredit) <- c("govtCredit"); Sys.sleep(1)

################################################################################
##	Nonrevolving Consumer Loans the Federal Government and Sallie Mae 	NREVNGOV 	1977-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Nonrevolving Consumer Loans owned by Commercial Banks        NREVNCB 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Nonrevolving Consumer Loans owned by Finance Companies       NREVNFC 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Nonrevolving Consumer Loans owned by Credit Unions           NREVNCU 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Nonrevolving Consumer Loans owned by Nonfinancial Businesses NREVNNFC 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Nonrevolving Consumer Loans owned by Savings Institutions    NREVNSAV 	1943-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################
    
#    nonRevGovtCredit  <- fredImport("NREVNGOV")
#    nonRevGovtCredit  <- xts(nonRevGovtCredit@data[,"NREVNGOV"], as.Date(as.character(time(nonRevGovtCredit@data[,"NREVNGOV"]))))
#    colnames(nonRevGovtCredit) <- c("nonRevGovtCredit")

################################################################################
##	Securitized Total Consumer Loans 	       TOTALSEC 	1945-12 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Securitized Consumer Revolving Credit    REVOLNSEC 	1989-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Securitized Consumer Nonrevolving Credit NREVNSEC 	1945-12 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################

################################################################################
##	Consumer Revolving Credit Owned by Commercial Banks      REVOLNCB 	1968-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Consumer Revolving Credit Owned by Finance Companies     REVOLNFC 	1984-12 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Consumer Revolving Credit Owned by Savings Institutions  REVOLNSAV 	1981-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Consumer Revolving Credit Owned by Credit Unions         REVOLNCU 	1984-01 	2011-07 	M 	Bil. of $ 	NSA 	 
##	Consumer Revolving Credit Owned by Nonfinancial Businesses REVOLNNFC 	1970-01 	2011-07 	M 	Bil. of $ 	NSA 	 
################################################################################
################################################################################


################################################################################
##	Real Retail and Food Services Sales 	          RRSFS 	1992-01 	2011-08 	M 	Mil. of $ 	SA 	 
##	Retail Sales: Total (Excluding Food Services) 	RSXFS 	1992-01 	2011-09 	M 	Mil. of $ 	SA 	 
##	Retail and Food Services Sales 	                RSAFS 	1992-01 	2011-09 	M 	Mil. of $ 	SA 	 
##	Retail Sales and Food Services Excluding Motor Vehicles and Parts Dealers 	RSFSXMV 	1992-01 	2011-09 	M 	Mil. of $ 	SA 	 
##	Retail and Food Services Sales 	                RSAFSNA 	1992-01 	2011-09 	M 	Mil. of $ 	NSA 	 
##	E-Commerce Retail Sales 	ECOMSA 	1999:Q4 	2011:Q2 	Q 	Mil. of $ 	SA 	 
##	E-Commerce Retail Sales 	ECOMNSA 	1999:Q4 	2011:Q2 	Q 	Mil. of $ 	NSA 	
################################################################################

    realRetail  <- fredImport("RRSFS")
    realRetail  <- xts(realRetail@data[,"RRSFS"], as.Date(as.character(time(realRetail@data[,"RRSFS"]))))
    colnames(realRetail) <- c("realRetail")


        
#allCredit <-cbind(consCredit, consCredit.NSA, revCredit, revCredit.NSA)
#allCredit <-cbind(allCredit, nonRevCredit, nonRevCredit.NSA)
#allCredit <-cbind(allCredit, govtCredit,  nonRevGovtCredit)

allCredit <-cbind(realRetail)
 


#rm(consCredit, consCredit.NSA, revCredit, revCredit.NSA)
#rm(nonRevCredit, nonRevCredit.NSA, govtCredit,  nonRevGovtCredit)
rm(realRetail)


return(allCredit)

}

################################################################################
################################################################################

####################        EOF Business - Fiscal      #########################

################################################################################
################################################################################


