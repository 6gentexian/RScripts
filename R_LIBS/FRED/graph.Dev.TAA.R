if (WINDOWS)  source('D:\\Projects\\TAA\\dasboard.Graphs.TAA.R')
if (!WINDOWS)  source('/home/edward/HIMCO/TAA/dasboard.Graphs.TAA.R')

################################################################################
################################################################################
################################################################################

require(sandwich)
require(lmtest)
require(xts)
require(PerformanceAnalytics)
require(DAAG)
require(lars)

if (WINDOWS)
{
  setwd('D:\\Projects\\TAA')
  outPath  <- 'D:\\Projects\\TAA\\OUTPUT'
  progPath <- 'D:\\Projects\\TAA\\PROGRAMS\\PYTHON\\'
}
if (!WINDOWS)  setwd('/home/edward/HIMCO/TAA')

StartingDate='1990-01-01'

load('TAA.RData')

################################################################################
################################################################################

cat(print(which(diff(index(qData)) > 93)), "\n")
cat(print(which(diff(index(qData)) > 94)), "\n")
coredata(qData$TY.rets.F1[nrow(qData)]) <- coredata(qData$TY.rets[nrow(qData)])

################################################################################
################################################################################


##  transform M2
#qData$M2.rets         <-diff(log(qData$M2))
qData$M2.NSA.rets     <-diff(log(qData$M2.NSA))
#qData$M2.M2NoM1.rets     <-diff(log(qData$M2NoM1))
#qData$M2.M2NoM1.NSA.rets <-diff(log(qData$M2NoM1.NSA))

#qData$M2.diffrM26          <-qData$M2.rets - lag(qData$M2.rets, 6)
qData$M2.diffrM26.NSA      <-qData$M2.NSA.rets - lag(qData$M2.NSA.rets, 6)
#qData$M2.diffrM26.NoM1     <-qData$M2.M2NoM1.rets - lag(qData$M2.M2NoM1.rets, 6)
#qData$M2.diffrM26.NoM1.NSA <-qData$M2.M2NoM1.NSA.rets - lag(qData$M2.M2NoM1.NSA.rets, 6)

##  IP
#qData$IP.rets <-diff(log(qData$IP))


##  DXY
#qData$DXY.rAUDJPY <- diff(log(qData$AUD/qData$JPY))
#qData$DXY.rets<- diff(log(qData$DXY))
#qData$DXY.EUR.rets<- diff(log(qData$EUR))
#qData$DXY.vDXYAY  <- (qData$DXY.rets - qData$DXY.rAUDJPY)^2


##  CPI
qData$cpi.rets        <-diff(log(qData$cpi))
#qData$cpi.NSA.rets    <-diff(log(qData$cpi.NSA))
#qData$cpi.cpiExFE.rets    <-diff(log(qData$cpiExFE))
#qData$cpi.cpiExFE.NSA.rets<-diff(log(qData$cpiExFE.NSA))
#
#qData$cpi.NAPM.Prices.rets<-diff(log(qData$NAPM.Prices))


## ISM
#qData$NAPM.rets      <-diff(log(qData$NAPM))
#qData$NAPM.NOI.rets  <-diff(log(qData$NAPM.NOI))
qData$NAPM.Prodn.rets<-diff(log(qData$NAPM.Prodn))
#qData$NAPM.NOInvt    <-qData$NAPM.NOI/qData$NAPM.Invt



################################################################################
################################################################################

testVars <- list(c("Slope.2Y3M"
              , "M2.diffrM26.NSA"
              , "NAPM.Prodn.rets"
              , "cpi.rets"
              )
              )


################################################################################
################################################################################


 freq <-'Q'
 q.windows <- c(30, 40, nrow(qData))
 m.windows <- c(30, 60, 90, nrow(mData))
 if( freq=='Q') windows=q.windows
 if( freq=='M') windows=m.windows


rm(m.windows, q.windows)
#   ***   must make sure that we're not at the perfect end of the quarter  *** #
#   ***   must make sure that we're not at the perfect end of the quarter  *** #
#   ***   must make sure that we're not at the perfect end of the quarter  *** #
#   ***   must make sure that we're not at the perfect end of the quarter  *** #

################################################################################
################################################################################

testVarGroup <- c("ALL")
#testVars <-list()
##testVars <- list(c("Slope.2Y3M")
##              , c("Slope.10Y3M"), c("Slope.10Y2Y")
##              , c("Slope.30Y3M"), c("Slope.30Y2Y")
##              )
#indepVars <-list()
##testVars <- list(c("Slope.2Y3M")
##              , c("Slope.10Y3M"), c("Slope.10Y2Y")
##              , c("Slope.30Y3M"), c("Slope.30Y2Y")
##     indepVar         )
#indepVars <- list(c("Slope.2Y3M")              )
#
#if (lagLength !=0)
#{
#  for(mm in 1:lagLength)
#  {
#    qData <-cbind(qData, lag(qData[, indepVars[[1]]], mm))
#
#    colnames(qData)[ncol(qData)] <- paste(eval(parse(text=paste("indepVars[[1]]", sep='')))
#                                          , ".L", mm, sep='')
#    indepVars[[mm+1]] <-   paste(eval(parse(text=paste("indepVars[[1]]", sep='')))
#              , ".L", mm, sep='')
#  }
#}

testEQ <-list()
testEQ <- list(c("ALL")
#                , c("cpi.NSA")
#                , c("cpi.NAPM")
#                , c("DXY")
#                , c("M2.M2NoM1.NSA")
                )


#testVars <- list(c("NAPM.NOI.rets"
#              , "cpi.NSA.rets"
##              , "cpi.NAPM.Prices.rets"
#              , "DXY.rets"
#              , "M2.M2NoM1.NSA.rets"
#              ))

#rm(indepVars, mm)
################################################################################
################################################################################


regData <-qData[which(index(qData) >= first(index(na.omit(qData$TY.rets.F1)))), ]
################################################################################
depVar <-c("TY.rets.F1"
          )
################################################################################
indepVar <-list()

indepVar <- testVars[[1]]
################################################################################

regData <-regData[, match(c(depVar, unlist(indepVar)), colnames(regData))]

if (length(testVars)==1)
{
  colnames(regData)[2:ncol(regData)] <-paste(testVarGroup, '.', testVars[[1]], sep='')
  indepVar  <-paste(testVarGroup, '.', testVars[[1]], sep='')
}
################################################################################

################################################################################
#hasIntercept <-TRUE

eq <-paste(depVar, " ~ ", indepVar[1], sep='')

if (length(indepVar) > 1)
{

  for(pp in 2:length(indepVar))
  {
    eq <-paste(eq, " + ", indepVar[pp], sep='')
  }
  rm(pp)
  eq <-formula(eq)
}

################################################################################
lagLength <-0



 regType <-c('OLS')

     ###########################################################################
     width <-windows[2]+1
     ###########################################################################
     minWindow <-30

     TT     <- nrow(regData)
     from  <- c(rep.int(1, width), seq(2, (TT-width+1)))
     to    <-1:TT

     all.52elements  <-apply(cbind(from,to),1,function(x) seq(x[1],x[2]))
     good.52elements <-all.52elements[minWindow:length(all.52elements)]
     ###########################################################################
      fileName <-paste("TAA_", gsub(".rets", "", depVar)
                  , "_", freq, "_", paste(gsub(".rets", "", unlist(testVars)), sep='', collapse=''), "_"
                  , "Lags.", lagLength, "_", regType[1], "_"
                  , (width-1), sep="")



################################################################################
################################################################################
indepVars <- toupper(c("Slope.2Y3M", "M2", "NAPM", "cpi"))

top.title <- function(x, backcolor="cornflowerblue", forecolor="darkblue", cex=3, ypos=0.4)
{
    plot(x=c(-1,1), y=c(0,1), xlim=c(-0.25,1.0), ylim=c(0,1), type="n", axes=FALSE)
    polygon(x=c(-2,-2,2,2), y=c(-2,2,2,-2), col=backcolor, border=NA)
    text(x=0,y=ypos, pos=4, cex= cex, labels=x, col=forecolor)
}


ltitle <- function(x, backcolor="#e8c9c1", forecolor="darkred", cex=2, ypos=0.4)
{
    mp <- barplot(seq_along(indepVars), plot=FALSE)    #xlim=c(-0.40,1),
    plot(x=c(min(floor(mp)), max(ceiling(mp))),y=c(0,1), ylim=c(0,1), type="n", axes=FALSE)
    polygon(x=c(min(floor(mp))-1, min(floor(mp))-1, max(ceiling(mp))+1, max(ceiling(mp))+1)
          , y=c(-2,2,2,-2),col=backcolor,border=NA)
    text(x=(min(floor(mp))-1 + max(ceiling(mp))+1)/2,y=ypos, cex=cex,labels=x,col=forecolor)

    return(mp)
}  


#    def.par <- par(no.readonly = TRUE)
#    on.exit(par(def.par))
#pdf(file ="TY_QU_Fundamental.pdf"
#    , paper = "USr", height = 8, width = 15)

    mylayout=layout(matrix(c(1,2,3,4,5,5,5,
                             1,6,6,6,6,7,8,
                             1,6,6,6,6,9,10,
                             1,6,6,6,6,11,12,
                             1,6,6,6,6,13,14
                             )
                    , ncol=5)
                    , widths=c(1, 3/4, 3/4, 3/4, 3/4)   #c(4/18,2/18,6/18,6/18)
                    , heights=c(lcm(1), lcm(1), 2/6, lcm(1), 2/6, lcm(1) ,2/6)
                    )
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    par(oma = rep(0.1,4))
#    layout.show(mylayout)

    assetName <- switch(gsub(".rets.F1", "", depVar)
            , 'TU' = "2 Year"
            , 'FV' = "5 Year"
            , 'TY' = "10 Year"
            , 'US' = "30 Year"
            , 'TY-TU' = "10-2 Year"
            )

    top.title(paste(assetName, ifelse(freq=='Q', ' Quarterly', ' Monthly')
            , " Fundamental Model", sep='')
            )


    mp <- ltitle("Factor Strength", cex=1.6, ypos=0.7)
    text(x=mp,y=0.2,cex=0.8,labels=indepVars,col='black')


    signalStrength.Graph.TAA()



    mp <-ltitle(paste(ifelse(freq=='Q', ' Qtrly', ' Monthly'), " Variable Levels"
                , sep=''),cex=1.5, ypos=0.7)

    varLevels <-format(as.numeric(last(regData[, 2:ncol(regData)])) *
                        c(1, 100, 100, 100), digits=2)
    varLevels <-c(varLevels[1], paste(varLevels[-1], " %", sep=''))
    
    text(x=mp,y=0.2,cex=0.8,labels=varLevels,col='black')
        
        
        
        
    variableLevels.Graph.TAA()



    plot.BBands(na.omit(dData$TY.Close), "TY", n=50, type=c("sma")
                , years=1, blue=TRUE
                , Ptarget, LimPrice)
                
                
                
##  signal strength section  ##
























#    indepVars <-c("Slope.2Y3M", "M2NoM1.NSA", "NAPM.Prodn", "NAPM.Prices")
#    reg <-qData[,  c("TY.rets.F1", indepVars)]
#
#      reg$M2NoM1.NSA.rets <-diff(log(reg$M2NoM1.NSA))
#      reg$NAPM.Prodn.rets <-diff(log(reg$NAPM.Prodn))
#      reg$NAPM.Prices.rets<-diff(log(reg$NAPM.Prices))
#
#
#    indepVars <-c("Slope.2Y3M", "M2NoM1.NSA.rets"
#              , "NAPM.Prodn.rets", "NAPM.Prices.rets")
#
#      reg <-reg[, c(depVar, indepVars)]
#      reg <-na.omit(reg)
#
#      window <-41
#      reg <- last(reg, window)
#      regData <- reg[1:(nrow(reg)-1), ]
#
#
#      eq <-paste(depVar, " ~ ", indepVars[1], sep='')
#
#      for(pp in 2:length(indepVars))
#      {
#        eq <-paste(eq, " + ", indepVars[pp], sep='')
#      }
#
#      eq <-formula(eq)
#      zreg <-summary(lm(eq,  data=regData))
#      Coeffs <- zreg$coefficients[, 1]
#
#
#
#
#      expn <- t(c(1, as.numeric(reg[nrow(reg), 2:ncol(reg)]))) %*% as.numeric(Coeffs)
#      defl.expn <-2
#      Expn <-(1 + expn)
#      Lim  <-(1 + -1*expn/defl.expn)
#
#    Ptarget  <-as.numeric(qData$TY[nrow(qData)-1]) * Expn
#    LimPrice <-as.numeric(qData$TY[nrow(qData)-1]) * Lim







dev.off()



































## need to extend the price series to include the forecasted values -- 66 days out
termDate = last(index(na.omit(dData$Close))) + 66


## chron package
#is.weekend(x)
#weekdays(x, abbreviate = TRUE)
#seq.dates("01/01/92", "12/31/92", by = "months")



################################################################################
################################################################################



    
    
#plotacpclust <- function(data,xax=1,yax=2,hcut,cor=TRUE,clustermethod="ave",
#                         colbacktitle="#e8c9c1",wcos=3,Rpowered=FALSE,...)

data <-swiss[,1:5]
xax=1; yax=3; hcut=48
cor=TRUE; clustermethod="ave"; colbacktitle="#e8c9c1"; wcos=3; Rpowered=FALSE


    require(ade4)
    pcr=princomp(data,cor=cor)
    datac=t((t(data)-pcr$center )/pcr$scale)
    hc=hclust(dist(data),method=clustermethod)
    if (missing(hcut)) hcut=quantile(hc$height,c(0.97))

    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))


    mylayout=layout(matrix(c(1,2,3,4,5,1,2,3,4,6,7,7,7,8,9,7,7,7,10,11)
                    , ncol=4)
                    , widths=c(4/18,2/18,6/18,6/18)
                    , heights=c(lcm(1),3/6,1/6,lcm(1),1/3)
                    )

    par(mar = c(0.1, 0.1, 0.1, 0.1))
    par(oma = rep(1,4))
    layout.show(mylayout)
    
    
    ltitle(paste("PCA ",dim(unclass(pcr$loadings))[2], "vars"),cex=1.6,ypos=0.7)
    text(x=0,y=0.2,pos=4,cex=1,labels=deparse(pcr$call),col="black")
    pcl=unclass(pcr$loadings)
    pclperc=100*(pcr$sdev)/sum(pcr$sdev)
    s.corcircle(pcl[,c(xax,yax)],1,2,sub=paste("(",xax,"-",yax,") ",
                                     round(sum(pclperc[c(xax,yax)]),0),"%",sep=""),
                possub="bottomright",csub=3,clabel=2)
    wsel=c(xax,yax)
    scatterutil.eigen(pcr$sdev,wsel=wsel,sub="")
    
    



### Code by Eric Lecoutre, Universite catholique de Louvain, Belgium
### Winner of the R Homepage graphics competition 2004

### Created using R 1.8.1, still works in 2.9.2

require(ade4)
## require(mva)   # was merged into stats
require(RColorBrewer)
require(pixmap)

ltitle <- function(x, backcolor="#e8c9c1", forecolor="darkred", cex=2, ypos=0.4)
{
    plot(x=c(-1,1),y=c(0,1),xlim=c(0,1),ylim=c(0,1),type="n",axes=FALSE)
    polygon(x=c(-2,-2,2,2),y=c(-2,2,2,-2),col=backcolor,border=NA)
    text(x=0,y=ypos,pos=4,cex=cex,labels=x,col=forecolor)
}

plotacpclust <- function(data,xax=1,yax=2,hcut,cor=TRUE,clustermethod="ave",
                         colbacktitle="#e8c9c1",wcos=3,Rpowered=FALSE,...)
{
    ## data: data.frame to analyze
    ## xax, yax: Factors to select for graphs

    ## Parameters for hclust
    ##   hcut
    ##   clustermethod

    require(ade4)

    pcr=princomp(data,cor=cor)

    datac=t((t(data)-pcr$center )/pcr$scale)

    hc=hclust(dist(data),method=clustermethod)
    if (missing(hcut)) hcut=quantile(hc$height,c(0.97))

    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))

    mylayout=layout(matrix(c(1,2,3,4,5,1,2,3,4,6,7,7,7,8,9,7,7,7,10,11),ncol=4),widths=c(4/18,2/18,6/18,6/18),heights=c(lcm(1),3/6,1/6,lcm(1),1/3))

    par(mar = c(0.1, 0.1, 0.1, 0.1))
    par(oma = rep(1,4))
    ltitle(paste("PCA ",dim(unclass(pcr$loadings))[2], "vars"),cex=1.6,ypos=0.7)
    text(x=0,y=0.2,pos=4,cex=1,labels=deparse(pcr$call),col="black")
    pcl=unclass(pcr$loadings)
    pclperc=100*(pcr$sdev)/sum(pcr$sdev)
    s.corcircle(pcl[,c(xax,yax)],1,2,sub=paste("(",xax,"-",yax,") ",
                                     round(sum(pclperc[c(xax,yax)]),0),"%",sep=""),
                possub="bottomright",csub=3,clabel=2)
    wsel=c(xax,yax)
    scatterutil.eigen(pcr$sdev,wsel=wsel,sub="")

    dend=hc
    dend$labels=rep("",length(dend$labels))
    dend=as.dendrogram(dend)

    ngrp=length(cut(dend,hcut)$lower)

    ltitle(paste("Clustering ",ngrp, "groups"),cex=1.6,ypos=0.4)

    par(mar = c(3, 0.3, 1, 0.5))

    ## Dendrogram
    attr(dend,"edgetext") = round(max(hc$height),1)
    plot(dend, edgePar = list(lty=1, col=c("black","darkgrey")), edge.root=FALSE,horiz=TRUE,axes=TRUE)

    abline(v=hcut,col="red")
    text(x=hcut,y=length(hc$height),labels=as.character(round(hcut,1)),col="red",pos=4)


    colorsnames= brewer.pal(ngrp,"Dark2")
    groupes=cutree(hc,h=hcut)
    ttab=table(groupes)

    ## Groups
    par(mar = c(0.3, 0.3, 1.6, 0.3))
    mp=barplot(as.vector(rev(ttab)),horiz=TRUE,space=0,col=rev(colorsnames),
               xlim=c(0,max(ttab)+10),axes=FALSE,main="Groups",axisnames=FALSE)
    text(rev(ttab),mp,as.character(rev(ttab)),col=rev(colorsnames),cex=1.2,pos=4)



    ## Main ACP scatterplot

    par(mar = c(0.1,0.1, 0.1,0.1))
    selscores=pcr$scores[,c(xax,yax)]

    zi=apply(datac,1,FUN=function(vec)return(sum(vec^2)))
    cosinus= cbind(selscores[,1]^2 / zi,selscores[,2]^2 / zi)
    cosinus= cbind(cosinus,apply(cosinus,1,sum))
    ww= (cosinus[,wcos])*4 +0.5

    ## Outliers? Test with median+1.5*IQ

    ## Factor #1
    out <- selscores[,1] < median(selscores[,1]) - 1.5 * diff(quantile(selscores[,1],c(0.25,0.75)))
    out = out | selscores[,1] > median(selscores[,1]) + 1.5 * diff(quantile(selscores[,1],c(0.25,0.75)))
    ## factor #2
    out = out | selscores[,2] < median(selscores[,2]) - 1.5 * diff(quantile(selscores[,2],c(0.25,0.75)))
    out = out | selscores[,2] > median(selscores[,2]) + 1.5 * diff(quantile(selscores[,2],c(0.25,0.75)))

    plot(selscores,axes=FALSE,main="",xlab="",ylab="",type="n")
    abline(h=0,col="black")
    abline(v=0,col="black")


    points(selscores[!out,1:2],col=(colorsnames[groupes])[!out],cex=ww,pch=16)


    text(x=selscores[out,1],y=selscores[out,2],labels=dimnames(selscores)[[1]][out],
         col=(colorsnames[groupes])[out], adj=1)
    box()




    ## Factor 1
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    ltitle(paste("Factor ",xax, " [",round(pclperc[xax],0),"%]",sep="" ),cex=1.6,ypos=0.4)
    plotdens(pcr$scores[,c(xax)])

    ## Factor 2
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    ltitle(paste("Factor ",yax," [",round(pclperc[yax],0),"%]",sep=""),cex=1.6,ypos=0.4)
    plotdens(pcr$scores[,c(yax)])
}

confshade2 <- function(y, xlo, xhi, col = 8.)
{
    n <- length(y)
    for(i in 1.:(n - 1.)) {
        polygon(c(xlo[i], xlo[i + 1.], xhi[i + 1.], xhi[i]),
                c(y[i], y[i + 1.], y[i + 1.], y[i]), col = col, border = FALSE)
    }
}

confshade <- function(x, ylo, yhi, col = 8.)
{
    n <- length(x)
    for(i in 1.:(n - 1.)) {
        polygon(c(x[i], x[i + 1.], x[i + 1.], x[i]),
                c(ylo[i], ylo[i + 1.], yhi[i + 1.], yhi[i]), col = col, border = FALSE)
    }
}


plotdens <- function(X, npts = 200, range = 1.5, xlab = "", ylab = "", main = "", ...)
{
    dens <- density(X, n = npts)
    qu <- quantile(X, c(0., 0.25, 0.5, 0.75, 1.))
    x <- dens$x
    y <- dens$y
    fqux <- x[abs(x - qu[2.]) == min(abs(x - qu[2.]))]
    fquy <- y[x == fqux]
    fquX <- as.numeric(qu[2.])
    tqux <- x[abs(x - qu[4.]) == min(abs(x - qu[4.]))]
    tquy <- y[x == tqux]
    tquX <- as.numeric(qu[4.])
    medx <- x[abs(x - qu[3.]) == min(abs(x - qu[3.]))]
    medy <- y[x == medx]
    ## Prepare les donnees a dessiner
    medX <- as.numeric(qu[3.])
    dx <- dens$x

    dy <- dens$y
    dx2 <- c(dx[dx <= fquX], fquX, dx[(dx > fquX) &
                (dx <= medX)], medX, dx[(dx > medX) & (dx <= tquX)], tquX, dx[dx > tquX])

    dy2 <- 	c(dy[dx <= fquX], fquy, dy[(dx > fquX) & (dx <= medX)], medy,
                  dy[(dx > medX) & (dx <= tquX)], tquy, dy[dx > tquX])
    IQX <- dx2[(dx2 >= fquX) & (dx2 <= tquX)]
    ##
    ##
    ## Initialise le graphique
    ##

    ## Dessine la densite
    IQy <- dy2[(dx2 >= fquX) & (dx2 <= tquX)]
    ## Trace densit sous IQ
    plot(0., 0., xlim = c(min(dx2), max(dx2)), ylim = c(min(dy2), max(dy2)),
         axes = F, xlab = xlab, ylab = ylab, main = main,type="n", ...)
    ## Ajoute mediane
    confshade(IQX, rep(0., length(IQX)), IQy, col = "#bdfcc9")
    bdw <- (tquX - fquX)/20.
    x1 <- c(medX - bdw/2., medX - bdw/2.)
    x2 <- c(medX + bdw/2., medX + bdw/2.)
    y1 <- c(0., medy)
    ## Ajoute lignes wiskers
    polygon(c(x1, rev(x2)), c(y1, rev(y1)), col = 0.)
    lines(x = c(fquX, fquX), y = c(0., fquy))
    ## Ajoute wiskers
    lines(x = c(tquX, tquX), y = c(0., tquy))
    meany <- mean(dy2)
    IQrange <- tquX - fquX
    lines(x = c(medX - range * IQrange, fquX), y = c(meany, meany))
    lines(x = c(tquX, medX + range * IQrange), y = c(meany, meany))
    lines(x = c(medX - range * IQrange, medX - range * IQrange),
          y = c(meany - (max(dy2) - min(dy2))/8., meany + (max(dy2) - min(dy2))/8.))

    ## Ajoute outliers

    lines(x = c(medX + range * IQrange, medX + range * IQrange),
          y = c(meany - (max(dy2) - min(dy2))/8., meany + (max(dy2) - min(dy2))/8.))
    out <- c(X[X < medX - range * IQrange], X[X > medX + range * IQrange])

    ## Ajoute les points...
    ## Ajoute l'axe
    points(out, rep(meany, length(out)), pch = 5., col = 2.)
    ## Ajoute l'axe
    points(dx2, dy2, pch = ".", type = "l")
    ##return(x = dessinx2, y = dessiny2)
    axis(1., at = round(c(min(x), fquX, medX, tquX, max(x)), 2.), labels = F,
         pos = 0.)
    invisible(list(x = dx2, y = dy2))
}



BoxDens <- function(data, npts = 200., x = c(0., 100.), y = c(0., 50.), orientation = "paysage",
                    add = TRUE, col = 11., border=FALSE,colline = 1., Fill = TRUE)
{
    dens <- density(data, n = npts)
    dx <- dens$x
    dy <- dens$y
    if(add == FALSE)
        plot(0., 0., axes = F, main = "", xlim = x, ylim = y, xlab = "",
             ylab = "")
    if(orientation == "paysage") {
        dx2 <- (dx - min(dx))/(max(dx) - min(dx)) * (x[2.] - x[1.]) * 0.98 +
            x[1.]
        dy2 <- (dy - min(dy))/(max(dy) - min(dy)) * (y[2.] - y[1.]) * 0.98 +
            y[1.]
        seqbelow <- rep(y[1.], length(dx))
        if(Fill == T)
            confshade(dx2, seqbelow, dy2, col = col)
        if (border==TRUE) points(dx2, dy2, type = "l", col = colline)
    }
    else {
        dy2 <- (dx - min(dx))/(max(dx) - min(dx)) * (y[2.] - y[1.]) * 0.98 +
            y[1.]
        dx2 <- (dy - min(dy))/(max(dy) - min(dy)) * (x[2.] - x[1.]) * 0.98 +
            x[1.]
        seqleft <- rep(x[1.], length(dy))
        if(Fill == T)
            confshade2(dy2, seqleft, dx2, col = col)
        if (border==TRUE) points(dx2, dy2, type = "l", col = colline)
    }
    polygon(x = c(x[1.], x[2.], x[2.], x[1.]),
            y = c(y[2.], y[2.], y[1.], y[1.]), density = 0.)
}


data(swiss)
## png(file="swiss.png", width=600,height=400)
plotacpclust(swiss[,1:5], 1, 3, hcut=48)
## dev.off()

