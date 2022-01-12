get_directory<- function(){
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    current_dir <- dirname(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    current_dir <- dirname(normalizePath(sys.frames()[[1]]$ofile))
  }
  return(current_dir)
}

get_directory_az=function(){
  tryCatch({
  current_dir <- (dirname(rstudioapi::getActiveDocumentContext()$path))
  },error=function(cond){
      getScriptPath <- function(){
          cmd.args <- commandArgs()
          m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
          script.dir <- dirname(regmatches(cmd.args, m))
          if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
          if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
          return(script.dir)
          }
      current_dir <-  getScriptPath()
      
      })
  return(current_dir)
}


get_directory_az_2=function(){
  tryCatch({
    current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  },error=function(cond){
    current_dir <- dirname(regmatches(commandArgs(), regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)))
  })
  return(current_dir)
}

plt=function(
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
