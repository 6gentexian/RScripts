CreateFedDataSeries.Employ <- function(StartingDate='1990-01-01')
{
################################################################################
######################             Employment             ######################
################################################################################

require(fImport)
require(xts)

################################################################################
################################################################################
##  Chicago Fed National Activity Index: Employment, Unemployment and Hours 	EUANDH 	1967-03 	2011-08 	M 	Index 	NA
################################################################################

    cfnaiEmpl  <- fredImport("EUANDH")
    cfnaiEmpl  <- xts(cfnaiEmpl@data[,"EUANDH"], as.Date(as.character(time(cfnaiEmpl@data[,"EUANDH"]))))
    colnames(cfnaiEmpl) <- c("cfnaiEmpl"); Sys.sleep(1)

################################################################################

################################################################################
## 	ISM Non-manufacturing: Employment Index NMFEI 	1997-07 	2011-09 	M 	Index 	SA
##  ISM Manufacturing: Employment Index     NAPMEI 	1948-01 	2011-09 	M 	Index 	SA
################################################################################

    ismNonManEmpl  <- fredImport("NMFEI")
    ismNonManEmpl  <- xts(ismNonManEmpl@data[,"NMFEI"], as.Date(as.character(time(ismNonManEmpl@data[,"NMFEI"]))))
    colnames(ismNonManEmpl) <- c("ismNonManEmpl"); Sys.sleep(1)

    ismManEmpl  <- fredImport("NAPMEI")
    ismManEmpl  <- xts(ismManEmpl@data[,"NAPMEI"], as.Date(as.character(time(ismManEmpl@data[,"NAPMEI"]))))
    colnames(ismManEmpl) <- c("ismManEmpl")

################################################################################
##  Civilian Unemployment Rate 	UNRATE 	1948-01 	2011-09 	M 	% 	SA
##  Civilian Unemployment Rate 	UNRATENSA 	1948-01 	2011-09 	M 	% 	NSA
##
##
##  Special Unemployment Rate: Unemployed and Discouraged Workers 	U4RATE 	1994-01 	2011-09 	M 	% 	SA
##  Special Unemployment Rate: Unemployed and Discouraged Workers 	U4RATENSA 	1994-01 	2011-09 	M 	% 	NSA
##
##
##  Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons 	U6RATE 	1994-01 	2011-09 	M 	% 	SA
##  Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons 	U6RATENSA 	1994-01 	2011-09 	M 	% 	NSA
################################################################################

    u3  <- fredImport("UNRATE")
    u3  <- xts(u3@data[,"UNRATE"], as.Date(as.character(time(u3@data[,"UNRATE"]))))
    colnames(u3) <- c("u3"); Sys.sleep(1)

    u3.NSA  <- fredImport("UNRATENSA")
    u3.NSA  <- xts(u3.NSA@data[,"UNRATENSA"], as.Date(as.character(time(u3.NSA@data[,"UNRATENSA"]))))
    colnames(u3.NSA) <- c("u3.NSA"); Sys.sleep(1)


    u4  <- fredImport("U4RATE")
    u4  <- xts(u4@data[,"U4RATE"], as.Date(as.character(time(u4@data[,"U4RATE"]))))
    colnames(u4) <- c("u4"); Sys.sleep(1)

    u4.NSA  <- fredImport("U4RATENSA")
    u4.NSA  <- xts(u4.NSA@data[,"U4RATENSA"], as.Date(as.character(time(u4.NSA@data[,"U4RATENSA"]))))
    colnames(u4.NSA) <- c("u4.NSA"); Sys.sleep(1)
    
    
    u6  <- fredImport("U6RATE")
    u6  <- xts(u6@data[,"U6RATE"], as.Date(as.character(time(u6@data[,"U6RATE"]))))
    colnames(u6) <- c("u6"); Sys.sleep(1)

    u6.NSA  <- fredImport("U6RATENSA")
    u6.NSA  <- xts(u6.NSA@data[,"U6RATENSA"], as.Date(as.character(time(u6.NSA@data[,"U6RATENSA"]))))
    colnames(u6.NSA) <- c("u6.NSA"); Sys.sleep(1)
    
    
################################################################################
##  Civilian Labor Force 	      CLF16OV 	1948-01 	2011-09 	M 	Thous. of Persons 	SA
##  Civilian Labor Force Level 	LNU01000000 	1948-01 	2011-09 	M 	Thous. of Persons 	NSA
##
##
##  Civilian Participation Rate 	            CIVPART 	1948-01 	2011-09 	M 	% 	SA
##  Civilian Labor Force Participation Rate 	LNU01300000 	1948-01 	2011-09 	M 	% 	NSA
##
##
##  Civilian Employment 	      CE16OV 	1948-01 	2011-09 	M 	Thous. of Persons 	SA
##  Employment Level 	          LNU02000000 	1948-01 	2011-09 	M 	Thous. of Persons 	NSA
##
##
##  Unemployed 	                UNEMPLOY 	1948-01 	2011-09 	M 	Thous. of Persons 	SA
##  Unemployment Level 	        LNU03000000 	1948-01 	2011-09 	M 	Thous. of Persons 	NSA
##
##
##  Civilian Employment-Population Ratio 	EMRATIO 	1948-01 	2011-09 	M 	% 	SA
##
#### same as 'employed'
###All Employees: Total nonfarm 	PAYEMS 	1939-01 	2011-09 	M 	Thous. of Persons 	SA
###All Employees: Total nonfarm 	PAYNSA 	1939-01 	2011-09 	M 	Thous. of Persons 	NSA
###Total Population: All Ages including Armed Forces Overseas ## not correct
####
##
##
####  this is used as the 'population' variable
##  Civilian Noninstitutional Population 	CNP16OV 	1948-01 	2011-09 	M 	Thous. of Persons 	NSA
################################################################################

    civLabor  <- fredImport("CLF16OV")
    civLabor  <- xts(civLabor@data[,"CLF16OV"], as.Date(as.character(time(civLabor@data[,"CLF16OV"]))))
    colnames(civLabor) <- c("civLabor"); Sys.sleep(1)

    employ  <- fredImport("CE16OV")
    employ  <- xts(employ@data[,"CE16OV"], as.Date(as.character(time(employ@data[,"CE16OV"]))))
    colnames(employ) <- c("employ")

    unEmploy  <- fredImport("UNEMPLOY")
    unEmploy  <- xts(unEmploy@data[,"UNEMPLOY"], as.Date(as.character(time(unEmploy@data[,"UNEMPLOY"]))))
    colnames(unEmploy) <- c("unEmploy")


    empIndex  <-cbind(cfnaiEmpl, ismNonManEmpl, ismManEmpl)
    empRates  <-cbind(u3, u3.NSA, u4, u4.NSA, u6, u6.NSA)
    empLevels <-cbind(civLabor, employ, unEmploy)
    
output <-cbind(empIndex, empRates, empLevels)


rm(cfnaiEmpl, ismNonManEmpl, ismManEmpl, civLabor, employ, unEmploy)
rm(u3, u3.NSA, u4, u4.NSA, u6, u6.NSA)
rm(empIndex, empRates, empLevels)



return(output)

}

################################################################################
################################################################################

####################          EOF Employment           #########################

################################################################################
################################################################################


