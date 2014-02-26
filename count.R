count <- function(cause = NULL){
  ## Check that "cause" is non-NULL; else throw error 
  if(is.null(cause)) stop("invalid error")
  x <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  
  ##  Check that specific "cause" is allowed; else throw error
  if(!any(x==as.character(cause))) stop("invalid error")
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  asp <- regexpr("<dd>Cause: [Aa][Ss][Pp][Hh][Yy][Xx][Ii][Aa][Tt][Ii][Oo][Nn]</dd>",homicides)
  blf <- regexpr("<dd>Cause: [Bb][Ll][Uu][Nn][Tt] [Ff][Oo][Rr][Cc][Ee]</dd>",homicides)
  oth <- regexpr("<dd>Cause: [Oo][Tt][Hh][Ee][Rr]</dd>",homicides)
  shot <- regexpr("<dd>Cause: [Ss][Hh][Oo][Oo][Tt][Ii][Nn][Gg]</dd>",homicides)
  stab <- regexpr("<dd>Cause: [Ss][Tt][Aa][Bb][Bb][Ii][Nn][Gg]</dd>",homicides)
  unk <-  regexpr("<dd>Cause: [Uu][Nn][Kk][Nn][Oo][Ww][Nn]</dd> ",homicides)
  if(cause == "asphyxiation")
    cnt <- regmatches(homicides,asp)
  else if(cause == "blunt force")
    cnt <- regmatches(homicides,blf)
  else if(cause == "other")
    cnt <- regmatches(homicides,oth)
  else if(cause == "shooting")
    cnt <- regmatches(homicides,shot)
  else if(cause == "stabbing")
    cnt <- regmatches(homicides,stab)
  else
    cnt <- regmatches(homicides,unk)
  
  ## Return integer containing count of homicides for that cause
  length(cnt)
}