get_directory <- function(){
  current_dir <- tryCatch({
    dirname(rstudioapi::getActiveDocumentContext()$path)
    },error=function(cond){
    dirname(regmatches(commandArgs(), regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)))
  })
  return(current_dir)
}

install_personal_github <- function(user,package,branch='main',path=NULL,delete_tar=T){
  
  lnk <- paste0('https://github.com/',user,'/',package,'/archive/',branch,'.tar.gz')
  download.file(url = lnk,destfile = paste0(package,'.tar.gz'))
  
  if(is.null(path))current_path <- getwd() else current_path <- path
  install.packages(paste0(current_path,'/',package,'.tar.gz'), repos = NULL, type = 'source')
  
  if(delete_tar)file.remove(paste0(package,'.tar.gz'))
}


plt <- function(
  x=NULL,
  bar=NULL,
  line.1=NULL,
  line.2=NULL,
  border=F,
  bar.col='#36435490',
  line.col=c('#364354','#543643'),
  line.type=c('p','b'),
  line.lty =c(3,3),
  line.lwd =c(1,1),
  line.cex =c(1,1),
  xlab='',
  ylab.1='',
  ylab.2='',
  main=''
)
{

  if(is.null(line.1) & is.null(line.2)){
    bar.lim=1.2
    main.bar=main
  }else{
    bar.lim=4
    main.bar=''
  }

  if(!is.null(bar)){
    barry=barplot(
      height = bar,
      names.arg = x,
      border = border,
      ylim   = c(0,max(bar)*bar.lim),
      col    = bar.col,
      xlab   = xlab,
      ylab   = ylab.1,
      main   = main.bar
    )

    text(
      x=barry,
      y=bar,
      labels = paste0(round(100*bar/sum(bar),1),'%'),
      pos=3,
      cex=.7
    )

    barry.step <- (barry[2]-barry[1])/2
    xlim <- c(head(barry,1)-barry.step,tail(barry,1)+barry.step)
    if(!is.null(line.1) | !is.null(line.2))par(new=T)
    axis.x='no need'
    axis.y=4
  }else{
    barry <- x
    barry.step <- (barry[2]-barry[1])/2
    xlim <- NULL
    axis.x=1
    axis.y=2
  }

  if(!is.null(line.1) & is.null(line.2)){

    rng  <- max(line.1)-min(line.1)

    ylim <- c(
      min=max(
        min(line.1)-rng*.50,
        min(line.1)*.50
      ),
      max=min(
        max(line.1)+rng*.1,
        max(line.1)*1.1
      )
    )


    plot(
      line.1 ~ barry,
      xlim   = xlim,
      ylim   = ylim,
      col    = line.col[1],
      frame  = F,
      type   = line.type[1],
      lty    = line.lty[1],
      lwd    = line.lwd[1],
      cex    = line.cex[1],
      pch    = 19,
      axes   = F,
      main   = main,
      xlab   = xlab,
      ylab   = ylab.1,
      yaxs   = "i")

    if(axis.x==1)axis(1)
    axis(axis.y)

  }else if(!is.null(line.1) & !is.null(line.2)){

    rng  <- max(line.1,line.2)-min(line.1,line.2)

    ylim <- c(
      min=max(
        min(line.1,line.2)-rng*.50,
        min(line.1,line.2)*.50
      ),
      max=min(
        max(line.1,line.2)+rng*.1,
        max(line.1,line.2)*1.1
      )
    )

    plot(
      line.1 ~ barry,
      xlim   = xlim,
      ylim   = ylim,
      col    = line.col[1],
      frame  = F,
      type   = line.type[1],
      lty    = line.lty[1],
      lwd    = line.lwd[1],
      cex    = line.cex[1],
      pch    = 19,
      axes   = F,
      main   = main,
      xlab   = xlab,
      ylab   = ylab.1,
      yaxs   = "i")

    if(axis.x==1)axis(1)
    axis(axis.y)

    lines(
      line.2 ~ barry,
      col    = line.col[2],
      type   = line.type[2],
      lty    =line.lty[2],
      lwd    = line.lwd[2],
      pch    = 19,
      cex    = line.cex[2])


  }

}


pretty_axis <- function(side=2){
  axis(
    side,
    axTicks(side),
    ifelse(axTicks(side)<1000,axTicks(side),ifelse(axTicks(side)<1000000,paste0(axTicks(side)/1000,'k'),paste0(axTicks(side)/1000000,'M'))))
}

barry_lim <- function(barry){
  c(head(barry,1)-.5*(barry[2]-barry[1]),tail(barry,1)+.5*(barry[2]-barry[1]))
  }
