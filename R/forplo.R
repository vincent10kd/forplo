#' @title forplo - flexible forest plots with R
#'
#' @description forplo is an R package meant to simplify the creation and customization of forest plots (alternatively called dot-and-whisker plots).
#' Input classes accepted by forplo are \code{data.frame}, \code{matrix}, \code{lm}, \code{glm}, and \code{coxph}. forplo was written in base R and does not depend on other packages.
#' For extensive examples and how to use all arguments for customization, please refer to the package vignette.
#'
#' @param mat An n*3 data.frame or matrix, or a regression model of class lm, glm or coxph.
#' @param em Effect measure to be displayed (e.g. OR, RR, HR).
#' @param row.labels Labels to display as variable names (character vector of length nrow(mat)).
#' @param linreg Set to TRUE if the estimates are from a linear regression model.
#' @param prop Set to TRUE if the estimates are proportions.
#' @param pval A numeric or character vector of same length as nrow(mat), with p-values.
#' @param xlim A numeric vector of length 2 indicating the limits of the x-axis.
#' @param fliprow A numeric vector indicating which estimates should be inverted (only for ratios).
#' @param flipbelow1 Set to TRUE to invert all ratios below 1.
#' @param flipsymbol A symbol to display besides inverted estimates. Asterisk by default.
#' @param ci.sep The separator between confidence intervals. Dash by default.
#' @param ci.lwd Line width for the confidence interval 'whiskers'.
#' @param ci.edge Set to FALSE to remove the 90 degree edges at the end of the CI whiskers.
#' @param font Controls the font family. 'Calibri' by default. Note: monospaced fonts work poorly.
#' @param groups A numeric vector of length nrow(mat) indicating group membership of each element.
#' @param grouplabs A character vector of equal length to the number of groups, with the labels of each group.
#' @param group.space A single numeric value to indicate how much empty rows should be between grouped estimates.
#' @param group.italics Set to TRUE to italicize the group labels.
#' @param indent.groups A numeric vector indicating which groups to indent (works only when left.align==TRUE)
#' @param left.align Set to TRUE to left align variable and group labels.
#' @param favorlabs A character vector of length 2, providing labels for underneath the x-axis (e.g. c('favors control','favors intervention')).
#' @param add.arrow.left Adds an arrow pointing left underneath the x-axis.
#' @param add.arrow.right Adds an arrow pointing right underneath the x-axis.
#' @param arrow.left.length Controls the length of the arrow pointing left.
#' @param arrow.right.length Controls the length of the arrow pointing right.
#' @param arrow.vadj Allows to adjust the vertical placement of the arrows.
#' @param sort Set to TRUE to sort the rows by effect size (not compatible diamond).
#' @param char Controls the character to display for the dots. Equivalent to pch in the base R plot function.
#' @param size Controls the size of the dots. Equivalent to cex in the base R plot function.
#' @param col Controls the color of the dots. Equivalent to col in the base R plot function.
#' @param insig.col Controls the color of the CI whiskers when crossing the null line. Gray by default.
#' @param scaledot.by Numeric vector of length nrow(mat) to indicate relative importance of each variable (e.g. sample size, weight).
#' @param scaledot.factor Scaling factor (scalar) for scaledot.by, to adapt the size of all scaled dots at once.
#' @param diamond Numeric vector indicating the rows that should be displayed as diamonds (e.g. for meta-analytic estimates).
#' @param diamond.col Controls the color of the diamonds.
#' @param diamond.line Shows a dotted vertical line through the last diamond. Set to FALSE to disable.
#' @param add.columns A data.frame of nrow(mat) with additional columns to add to the right of the plot.
#' @param add.colnames A character vector of length ncol(add.columns) with column labels for these columns.
#' @param column.spacing A constant that controls the horizontal spacing between added columns.
#' @param right.bar Set to TRUE to show a vertical bar directly to the left of the estimates.
#' @param rightbar.ticks Controls the tick marks on the right axis.
#' @param left.bar Set to FALSE to remove the horizontal bar on the left axis.
#' @param leftbar.ticks Controls the tick marks on the left axis.
#' @param shade.every Controls row shading option. A value of 1 colors every other row, a value of 2 per blocks of 2, etc. Non-integer values also allowed.
#' @param shade.col Controls the default row shading color. Default is 'red'.
#' @param shade.alpha Controls the transparency of the row shading color. Default is 0.05.
#' @param fill.by Numeric vector of length nrow(mat) indicating color group membership of each element.
#' @param fill.colors Character vector of length unique(fill.by), with colors for each color group.
#' @param fill.labs Character vector of length fill.colors, specifying the legend labels.
#' @param legend Set to TRUE to display a legend if fill.colors is not NULL.
#' @param legend.vadj Controls the vertical placement of the legend.
#' @param legend.hadj Controls the horizontal placement of the legend.
#' @param legend.spacing Controls the spacing between legend items.
#' @param margin.left Controls size of left margin.
#' @param margin.top Controls size of top margin.
#' @param margin.bottom Controls size of bottom margin.
#' @param margin.right Controls size of right margin.
#' @param horiz.bar Set to TRUE to display a horizontal bar below the plot.
#' @param title Title to display above the plot. Equivalent to title in the base R plot function.
#' @param save Set to TRUE to save the plot (also requires save.name and save.path) in 300 dpi resolution.
#' @param save.path Indicates folder where the plot should be saved.
#' @param save.name Name of the plot (should not include filetype extension).
#' @param save.type Filetype of the saved plot. Default is .png, but also supports .wmf on Windows.
#' @param save.width Width of the saved plot in inches. Default is 9.
#' @param save.height Height of the saved plot in inches. Default is 4.5.
#'
#' @return The function plots in the user's plot window, but does not return anything.
#' @examples
#' #==== Create some regression models ==========
#' mod1 <- lm(Sepal.Length~Sepal.Width+Species+Petal.Width+Petal.Length,iris)
#'
#' #==== Example forest plots====================
#' # default plot for linear regression model
#' forplo(mod1)
#'
#' # customized plot for linear regression model
#' forplo(mod1,
#'        row.labels=c('Sepal width','Versicolor','Virginica','Petal width','Petal length'),
#'        groups=c(1,2,2,3,3),
#'        grouplabs=c('Sepal traits','Species','Petal traits'),
#'        shade.every=1,
#'        shade.col='gray',
#'        left.align=TRUE,
#'        xlim=c(-2,2),
#'        title='Linear regression with grouped estimates')
#'
#' ## More examples are given in the vignette.
#'
#' @export
#'

forplo <- function(mat,
                   em='OR',
                   row.labels=NULL,
                   linreg=FALSE,
                   prop=FALSE,
                   pval=NULL,
                   xlim=xlimits,
                   fliprow=NULL,
                   flipbelow1=FALSE,
                   flipsymbol='*',
                   ci.sep='-',
                   ci.lwd=1.5,
                   ci.edge=TRUE,
                   font='sans',
                   groups=NULL,
                   grouplabs=NULL,
                   group.space=1,
                   group.italics=FALSE,
                   indent.groups=NULL,
                   left.align=FALSE,
                   favorlabs=NULL,
                   add.arrow.left=FALSE,
                   add.arrow.right=FALSE,
                   arrow.left.length=3,
                   arrow.right.length=3,
                   arrow.vadj=0,
                   sort=FALSE,
                   char=20,
                   size=1.5,
                   col=1,
                   insig.col='gray',
                   scaledot.by=NULL,
                   scaledot.factor=0.75,
                   diamond=NULL,
                   diamond.col=col,
                   diamond.line=TRUE,
                   add.columns=NULL,
                   add.colnames=NULL,
                   column.spacing=3,
                   right.bar=FALSE,
                   rightbar.ticks=0,
                   left.bar=TRUE,
                   leftbar.ticks=0,
                   shade.every=NULL,
                   shade.col='red',
                   shade.alpha=0.05,
                   fill.by=NULL,
                   fill.colors=NULL,
                   fill.labs=NULL,
                   legend=FALSE,
                   legend.vadj=0,
                   legend.hadj=0,
                   legend.spacing=1,
                   margin.left=NULL,
                   margin.top=0,
                   margin.bottom=2,
                   margin.right=10,
                   horiz.bar=FALSE,
                   title=NULL,
                   save=FALSE,
                   save.path=NULL,
                   save.name=NULL,
                   save.type='png',
                   save.width=9,
                   save.height=4.5){
  # checks
  if(!class(mat)[1]%in%c('matrix','data.frame','glm','lm','coxph')){
    stop('forplo() expects an object of class matrix, data.frame, lm, glm, or coxph.')}
  if(class(mat)[1]%in%c('matrix','data.frame')){
    if(ncol(mat)!=3) stop('forplo() expects a matrix or data.frame with exactly 3 columns (estimate, CI lower bound, CI upper bound)')
    if(sum(mat[,1]<0)>0&linreg==FALSE){
      message('Since column 1 of mat contains values below 0, linreg has been set to TRUE.')
      linreg <- TRUE
    }
    if(class(mat)[1]=='matrix'){
      mat <- as.data.frame(mat)
    }
  }
  if(flipbelow1==TRUE&is.null(favorlabs)!=TRUE){stop('favorlabs cannot be used when flipbelow1 is TRUE.')}
  if(!is.null(favorlabs)&length(favorlabs)!=2){stop('favorlabs should be a character vector of length 2.')}
  # round function
  Round <- function(x, digits = 0) {
    x = x + abs(x) * sign(x) * .Machine$double.eps
    round(x, digits = digits)
  }
  # convert model to data.frame
  omat <- mat
  if(class(omat)[1]%in%c('lm','glm')){
    pval <- Round(summary(mat)$coef[-1,4],4)
    pval[pval==0.0000] <- '<0.0001'
    mat <- cbind(stats::coef(mat),stats::confint(mat))[-1,]
    colnames(mat) <- c('est','lci','uci')
    if(stats::family(omat)$family=='gaussian'){linreg <- TRUE}
    else{
      linreg <- FALSE
      mat <- exp(mat)
    }
  }
  if(class(omat)[1]=='coxph'){
    em <- 'HR'
    pval <- Round(summary(mat)$coef[,5],4)
    pval[pval<=0.0000] <- '<0.0001'
    mat <- exp(cbind(stats::coef(mat),stats::confint(mat)))
    colnames(mat) <- c('est','lci','uci')
  }
  # if row.labels are given, replace rownames
  if(!is.null(row.labels)){
    if(length(row.labels)!=nrow(mat)){stop('The length of row.labels should be equal to the number of rows of mat.')}
    row.labels2 <- seq_along(row.labels)
    rownames(mat) <- row.labels2
  }
  # function to count number of characters
  charcount <- function(x){
    length(unlist(strsplit(as.character(x),'')))
  }
  # x coordinate for null line
  sigt <- 1
  # if linear regression
  if(linreg==TRUE){
    if(flipbelow1==TRUE){stop('flipbelow1 cannot be TRUE when linreg is TRUE.')}
    sigt <- 0
    ci.sep <- ifelse(ci.sep=='-',';',ci.sep)
    if(em=='OR'){em <- expression(hat(beta))}
    if(!is.null(favorlabs)){stop('favorlabs cannot be used when linreg is TRUE.')}
  }
  # if proportion
  if(prop==TRUE){
    sigt <- 0
    xlim <- c(0,1)
    if(em=='OR'){em <- 'Prop.'}
    if(!is.null(favorlabs)){stop('favorlabs cannot be used when prop is TRUE.')}
  }
  # p-value conditions
  if(!is.null(pval)&length(pval)!=nrow(mat)) stop('The length of pval should be equal to the number of rows of mat')
  if(!is.null(add.columns)&!is.null(pval)){stop('add.columns cannot be used if pval is not NULL.')}
  # store original margins
  opar <- graphics::par()$mar
  # fill colors
  if(!is.null(fill.by)){
    if(is.null(fill.colors)){stop('fill.colors must be specified if fill.by is not NULL.')}
    fill.by <- as.numeric(fill.by)
    fill.colors <- fill.colors[fill.by]
  }
  # create vector indicating rows containing diamonds
  diavec <- rep(0,nrow(mat))
  if(!is.null(diamond)){diavec[diamond] <- 1}
  # if groups are given, modify matrix to add empty rows and labels
  if(!is.null(groups)){
    if(is.null(grouplabs)){stop('grouplabs should be provided when groups is not NULL')}
    if(length(grouplabs)!=length(unique(groups))){stop('grouplabs should be of equal length to the number of groups.')}
    grouplabs <- as.character(grouplabs)
    groups <- as.numeric(groups)
    mat <- mat[order(groups),]
    groups <- sort(groups)
    # indent subgroups by group index
    if(!is.null(indent.groups)){
      if(left.align==FALSE){
        message('If indent.groups is not NULL, left.align should be set to TRUE.\n')
        left.align <- TRUE
      }
      iv <- indent.groups
      rownames(mat)[which(groups%in%iv)] <- paste0('    ',rownames(mat)[which(groups%in%iv)])
      grouplabs[iv] <- paste0('    ',grouplabs[iv])
    }
    g.ind <- which(diff(groups)==1)
    g.start <- c(1,g.ind+1)
    g.end <- c(g.ind,nrow(mat))
    mat2 <- data.frame(matrix(nrow=0,ncol=3))
    # since rownames have to be unique, add variable lengths of whitespace as rownames for empty rows
    for(i in 1:length(g.start)){
      spacemat <- data.frame(matrix(NA,nrow=group.space,ncol=3))
      colnames(spacemat) <- colnames(mat)
      m <- rbind(rep(NA,3),mat[g.start[i]:g.end[i],],spacemat)
      space.names <- character(group.space)
      for(j in 1:group.space){
        space.names[j] <- paste0(rep(' ',i+j*nrow(mat)),collapse='')
      }
      rownames(m) <- c(paste0(rep(' ',i),collapse=''),rownames(mat)[g.start[i]:g.end[i]],space.names)
      mat2 <- rbind(mat2,m)
      rm(m)
    }
    if(!is.null(pval)){
      pval2 <- numeric(0)
      for(i in 1:length(g.start)){
        p <- c(NA,pval[g.start[i]:g.end[i]],rep(NA,group.space))
        pval2 <- c(pval2,p)
        rm(p)
      }
      opval <- pval
      pval <- pval2
    }
    if(!is.null(scaledot.by)){
      scale2 <- numeric(0)
      for(i in 1:length(g.start)){
        s <- c(NA,scaledot.by[g.start[i]:g.end[i]],rep(NA,group.space))
        scale2 <- c(scale2,s)
        rm(s)
      }
      oscale <- scaledot.by
      scaledot.by <- scale2
    }
    if(!is.null(fill.by)){
      fill.colors2 <- character()
      for(i in 1:length(g.start)){
        fc <- c(NA,fill.colors[g.start[i]:g.end[i]],rep(NA,group.space))
        fill.colors2 <- c(fill.colors2,fc)
        rm(fc)
      }
      fill.colors <- fill.colors2
    }
    diavec2 <- numeric(0)
    for(i in 1:length(g.start)){
      d <- c(0,diavec[g.start[i]:g.end[i]],rep(0,group.space))
      diavec2 <- c(diavec2,d)
      rm(d)
    }
    diavec <- diavec2
    omat <- mat
    mat <- mat2
  }
  lHR <- nrow(mat)
  if(!is.null(groups)){select <- -which(!rownames(mat)%in%rownames(omat))} else{select <- 1:length(seq(lHR,1))}
  if(flipbelow1==TRUE){
    fliprow <- which(mat[select,1]<1)
    rownames(mat)[select][fliprow] <- paste0(rownames(mat)[select][fliprow],flipsymbol)
  }
  if(!is.null(fliprow)){
    for(i in 1:length(fliprow)){
      mat[select,][fliprow[i],] <- 1/mat[select,][fliprow[i],c(1,3:2)]
    }
  }
  if(sort==TRUE){
    if(!is.null(diamond)){stop('sort is not compatible with diamond.')}
    if(!is.null(groups)){
      # sort within groups
      grind <- !is.na(mat[,1])
      dind <- diff(grind)
      grp.count <- 0
      grp.vec <- rep(0, length(grind))
      for(i in 2:length(grind)){
        if(grind[i] == 1 & dind[(i-1)] == 1){
          grp.count <- grp.count + 1
        }
        grp.vec[i] <- grp.count
        if(grind[i] == 0) grp.vec[i] <- 0
      }
      grp.sort <- 1:length(grind)
      for(g in 1:length(unique(groups))){
        grp.sort[which(grp.vec == g)] <- which(grp.vec == g)[order(mat[grp.vec == g,1], decreasing=TRUE)]
      }
      sort.index <- grp.sort
      if(!is.null(add.columns)){
        add.columns2 <- data.frame(matrix(nrow=nrow(mat),ncol=ncol(add.columns)))
        add.columns2[select,] <- add.columns
        add.columns <- add.columns2
      }
    }
    if(is.null(groups)){sort.index <- order(mat[,1],decreasing=T)}
    mat <- mat[sort.index,]
    pval <- pval[sort.index]
    fill.colors <- fill.colors[sort.index]
    scaledot.by <- scaledot.by[sort.index]
    add.columns <- add.columns[sort.index,]
  }
  # set par
  margin.bottom <- ifelse((!is.null(favorlabs)|
                             !is.null(add.arrow.left)|
                             !is.null(add.arrow.right)|
                             legend==TRUE&!is.null(fill.labs))&margin.bottom<5,
                          margin.bottom+3,margin.bottom)
  margin.right <- ifelse(!is.null(pval)&margin.right<15,15,margin.right)
  margin.right <- ifelse(!is.null(add.columns),
                         margin.right+3*ncol(data.frame(add.columns)),margin.right)
  if(is.null(margin.left)){
    lablen <- max(sapply(rownames(mat),charcount))
    margin.left <- pmin(13,pmax(8,lablen-8))
  }
  margin.top <- ifelse(!is.null(title),3,0)
  graphics::par(mar=c(margin.bottom,margin.left,margin.top,margin.right))
  if(!is.null(margin.right)){graphics::par(mar=c(margin.bottom,margin.left,margin.top,margin.right))}
  # save plot
  if(save==TRUE){
    if(!save.type%in%c('wmf','.wmf','WMF','png','.png','PNG')){
      message('forplo() only accepts png and wmf as save formats. Your plot will not be saved.')}
    if(save.type%in%c('wmf','.wmf','WMF')){
      grDevices::dev.new(save.width,save.height)}
    if(save.type%in%c('png','.png','PNG')){
      grDevices::png(paste0(save.path,save.name,'.png'),width=save.width,height=save.height,units='in',res=300)}
    graphics::par(mar=c(margin.bottom,margin.left,margin.top,margin.right))
  }
  # plot
  if(linreg==TRUE){xlimits <- c(min(mat[,2],na.rm=TRUE)*ifelse(min(mat[,2],na.rm=TRUE)<0,1.2,-1.2),max(mat[,3],na.rm=TRUE)*1.2)}
  else if(linreg==FALSE){
    if(min(mat[,2],na.rm=TRUE)==0){
      xlimits <- exp(c(min(log(mat[,2]+1e-10),na.rm=TRUE)*1.2,max(log(mat[,3]),na.rm=TRUE)*1.2))
    }
    if(min(mat[,2],na.rm=TRUE)>0){
      xlimits <- exp(c(min(log(mat[,2]),na.rm=TRUE)*1.2,max(log(mat[,3]),na.rm=TRUE)*1.2))
    }
  }
  if(linreg==FALSE&xlim[1]==0){xlim[1] <- 1e-2}
  HR <- mat[,1]
  CI <- mat[,2:3]
  yvec <- seq(lHR,1)
  plot(y=yvec,
       x=HR[1:lHR],
       xlim=xlim,
       ylim=c(0,lHR+1),
       pch='',
       xlab='',
       yaxt="n",
       log=ifelse(linreg==TRUE|prop==TRUE,'','x'),
       ylab="",
       bty="n",
       main=title,
       family=font)
  # shade rows
  if(!is.null(shade.every)){
    shade_index <- nrow(mat)/shade.every
    for(s in seq(1,shade_index,2)){
      graphics::rect(xlim[1],0.5+shade.every*(s-1),
           xlim[2],0.5+shade.every+shade.every*(s-1),
           col=grDevices::adjustcolor(shade.col,alpha.f=shade.alpha),border=FALSE)
    }
  }
  # draw CIs
  for(i in seq(1,lHR)){
    j <- seq(lHR,1)[i]
    if(is.na(CI[j,1])|diavec[j]==1){next}
    graphics::arrows(y0=i,
           x0=CI[j,1],
           y1=i,
           x1=CI[j,2],
           length=ifelse(ci.edge==FALSE,0,0.03),angle=90,code=3,lwd=ci.lwd,
           lty=1,
           col=ifelse(!is.null(fill.by),fill.colors[j],
                      ifelse(sigt%in%Round(seq(Round(CI[j,1],3),Round(CI[j,2],3),by=0.001),3),insig.col,1)))
  }
  # dotted null line
  graphics::abline(v=sigt,lty=3)
  # draw points
  if(is.null(fill.colors)){
    graphics::points(y=yvec[which(diavec==0)],x=HR[1:lHR][which(diavec==0)],pch=char,col=col,cex=size)
  }
  if(!is.null(fill.colors)){
    graphics::points(y=yvec[which(diavec==0)],x=HR[1:lHR][which(diavec==0)],pch=char,
           col=fill.colors[which(diavec==0)],cex=size)
  }
  # if scaledot.by is given, draw each dot with different size
  if(!is.null(scaledot.by)){
    for(i in 1:length(yvec[which(diavec==0)])){
      graphics::points(y=yvec[which(diavec==0)][i],x=HR[1:lHR][which(diavec==0)][i],pch=char,
             col=ifelse(!is.null(fill.by),fill.colors[which(diavec==0)][i],col),
             cex=(scaledot.by[which(diavec==0)][i]/max(scaledot.by,na.rm=T))*4*scaledot.factor)
    }
  }
  # draw diamonds
  if(!is.null(diamond)){
    for(i in 1:length(diamond)){
      y1 <- yvec[select][diamond[i]]
      x1 <- CI[,1][select][diamond[i]]
      x2 <- HR[1:lHR][select][diamond[i]]
      x3 <- CI[,2][select][diamond[i]]
      dia.x <- c(x1,x2,x3,x2,x1)
      dia.y <- c(y1,y1+0.15,y1,y1-0.15,y1)
      graphics::polygon(dia.x,dia.y,col=diamond.col,border=diamond.col)
    }
    if(diamond.line!=FALSE){
      graphics::abline(v=x2,lty=3,col=diamond.col)
    }
  }
  # display arrows below x-axis
  if(add.arrow.left==TRUE){
    ex <- paste0(c('\\254',rep('\\276',arrow.left.length)),collapse='')
    graphics::mtext(side=1, line=1.7-arrow.vadj, parse(text=paste0("''*symbol('",ex,"')*''")),adj=0)
  }
  if(add.arrow.right==TRUE){
    ex <- paste0(c(rep('\\276',arrow.right.length),'\\256'),collapse='')
    graphics::mtext(side=1, line=1.7-arrow.vadj, parse(text=paste0("''*symbol('",ex,"')*''")),adj=1)
  }
  # display labels below x-axis
  if(!is.null(favorlabs)){
    graphics::mtext(side=1, line=2.5-arrow.vadj, favorlabs[1],adj=0,font=3,family=font)
    graphics::mtext(side=1, line=2.5-arrow.vadj, favorlabs[2],adj=1,font=3,family=font)
  }
  # add legend
  if(!is.null(fill.labs)&legend==TRUE){
    u_int <- graphics::par('usr')[3]+legend.vadj
    graphics::mtext('Legend',side=4, at=u_int, line=1+legend.hadj, family=font,las=2, font=2)
    for(f in 1:length(unique(stats::na.omit(fill.colors)))){
      graphics::mtext(expression(''*symbol('\267')*''), side=4, at=u_int-(f*.5*legend.spacing),
            line=1.5+legend.hadj, family=font,las=2, col=unique(stats::na.omit(fill.colors))[f])
      graphics::mtext(fill.labs[f], side=4, at=u_int-(f*.5*legend.spacing), line=2+legend.hadj, family=font, las=2)
    }
  }
  # horizontal bar
  if(horiz.bar==TRUE){graphics::abline(h=0,lty=1)}
  # left bar
  if(left.bar==TRUE){
    graphics::axis(2,at=seq(lHR,1),las=2,lwd=1,labels=FALSE,lwd.ticks=leftbar.ticks,tick=left.bar)
  }
  # write row names and group labels (bold)
  if(is.null(row.labels)){
    row.labels <- rownames(mat)
  }
  else{
    row.labels <- row.labels[as.numeric(rownames(mat))]
  }
  graphics::axis(2,at=seq(lHR,1),
                 labels=row.labels,
                 las=2,family=font,
       lwd=0,lwd.ticks=FALSE,tick=FALSE,
       hadj=ifelse(left.align==TRUE,0,NA),
       line=ifelse(left.align==TRUE,margin.left-2.5,NA))
  if(!is.null(grouplabs)){
    lab.ind <- which(!rownames(mat)%in%rownames(omat))
    lab.ind <- lab.ind[seq(1,length(lab.ind),group.space+1)]
    graphics::axis(2,at=seq(lHR,1)[lab.ind],labels=grouplabs,las=2,family=font,font=ifelse(group.italics==TRUE,4,2),
         lwd=ifelse(left.align==TRUE,0,left.bar*1),
         hadj=ifelse(left.align==TRUE,0,NA),
         line=ifelse(left.align==TRUE,margin.left-2,NA))
  }
  graphics::axis(4,at=lHR+1,labels=em,las=2,line=1,tick=F,font=2,las=2,family=font)
  graphics::axis(4,at=lHR+1,labels='95% CI',line=4,tick=F,font=2,las=2,family=font)
  # write CIs
  graphics::axis(4,at=seq(lHR,1)[select],labels=sprintf('%.2f',Round(stats::na.omit(mat[,1]),2)),las=2,line=1,
       tick=right.bar,lwd.ticks=rightbar.ticks,family=font)
  graphics::axis(4,at=seq(lHR,1)[select],labels=paste0(sprintf('[%.2f',Round(stats::na.omit(mat[,2]),2)),ci.sep),las=2,line=4,tick=F,family=font)
  graphics::axis(4,at=seq(lHR,1)[select],labels=paste0(ifelse(max(sapply(Round(stats::na.omit(mat[,2]),2),charcount))<5,' ','   '),
                                             sprintf('%.2f',Round(stats::na.omit(mat[,3]),2)),']'),las=2,line=6,tick=F,family=font)
  # add additional columns
  if(!is.null(add.columns)){
    if(sort){add.columns <- add.columns[select,]}
    startline=9
    for(k in 1:ncol(data.frame(add.columns))){
      if(!is.null(add.colnames)){graphics::axis(4,at=lHR+1,labels=add.colnames[k],las=2,line=startline,tick=F,font=2,family=font)}
      graphics::axis(4,at=seq(lHR,1)[select],labels=data.frame(add.columns)[,k],las=2,line=startline,tick=F,family=font)
      startline <- startline+column.spacing
    }
  }
  # add p-values
  if(!is.null(pval)){
    graphics::axis(4,at=lHR+1,labels='p-value',line=9,tick=F,font=2,las=2,family=font)
    graphics::axis(4,at=seq(lHR,1),labels=pval,las=2,line=9,tick=F,family=font)
  }
  # end saving plot if type is .wmf
  if(save==TRUE){
    if(save.type%in%c('wmf','.wmf','WMF')){grDevices::savePlot(paste0(save.path,save.name,'.wmf'),type='wmf')}
    grDevices::dev.off()
  }
  # restore original plot settings
  graphics::par(mar=opar)
  graphics::layout(1)
}
