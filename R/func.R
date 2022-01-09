get_directory<- function(){
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    x <- normalizePath(sub(needle, "", cmdArgs[match]))
  } else {
    # 'source'd via R console
    x <- normalizePath(sys.frames()[[1]]$ofile)
  }
  return(dirname(x))
}
