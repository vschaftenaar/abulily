
get_directory=function(){
  tryCatch({
    dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    },error=function(cond){
      dir <- dirname(regmatches(commandArgs(), regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)))
      }
    )

  return(dir)

}
