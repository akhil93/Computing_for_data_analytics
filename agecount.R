agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (length(age) == 0)
    stop("age is null");
  ## Read "homicides.txt" data file
  homicides <- readLines("Books/courserahomicides.txt");
  ## Extract ages of victims; ignore records where no age is
  regQuery <- sprintf(" %s years old", age);
  ## given
  ## Return integer containing count of homicides for that age
  ans <- grep(regQuery, homicides);
  ## Return integer containing count of homicides for that cause
  return (length(ans));
}