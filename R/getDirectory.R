# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

get_directory=function(){
tryCatch({
  dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  },error=function(cond){
  dir <- dirname(regmatches(commandArgs(), regexpr("(?<=^--file=).+", commandArgs(), perl=TRUE)))
  }
)
  return(dir)
}
