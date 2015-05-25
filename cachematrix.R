# initialize cached matrix
cacheMatrix <- FALSE

makeCacheMatrix <- function(x = matrix()) {
 # solve inverse for x, save it as inverse
 inverse <- solve(x)
 
 # set parent environments "cacehMatrix" var to be our list with matrix and inverse 
 cacheMatrix <<- list(inverse=inverse, matrix=x)
}


cacheSolve <- function(x = matrix) {
  # check to see if we've cached x alread
  if(identical(x, cacheMatrix$matrix)){
    print("Matrix is cached")
    # return cached inverse if already cahced
    cacheMatrix$inverse
  }else{
    # return non cached inverse if not cached already
    print("Matrix is not cached")
    solve(x)
  }
}

test <- function(){
  c <- matrix(
    c(1,0,0,0,1,0,0,0,1),
    nrow = 3,
    ncol = 3
  )
  
  d <- matrix(
    c(0,0,1,0,1,0,1,0,0),
    nrow = 3,
    ncol = 3
  )
  
  makeCacheMatrix(c)
  print(cacheSolve(c))
  print(cacheSolve(d))
}