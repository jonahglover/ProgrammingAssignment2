## make a cache
cache <- matrixCache()

makeCacheMatrix <- function(x = matrix()) {
  cache$cache(x)
}


cacheSolve <- function(x, ...) {
  cm <- cache$cache(x)
  cm$inverse()
}

####################################################################################

cacheMatrix <- function(x = matrix()){
  inverse <- solve(x)
  getInverse <- function(){
    inverse
  }
  get <- function(){
    x
  }
  list(  
    get = get,
    inverse = getInverse
  )
}

# let x be a matrix with n cells
matrixCache <- function() {
  
  matrixTable <- list()
  
  # O(n)
  # hashed matrix looks like
  # c1c2c3c4...cnnrow(n)ncol(n)
  hashMatrix <- function(mat = matrix()){
    hashStr <- vector()
    for (cell in B){
      hashStr <- paste(c(hashStr, cell), collapse="")
    }
    hashStr <- paste(c(hashStr, nrow(B)), collapse="")
    hashStr <- paste(c(hashStr, ncol(B)), collapse="")
    hashStr
  }
  
  # returns a cacheMatrix of x
  cache <- function(x = matrix()){
    key <- hashMatrix(x)
    if(is.null(matrixTable[[key]])){
      matrixTable[[key]] <- cacheMatrix(x)
    }
    # matrixTable[[key]]
  }
  
  list(cache=cache)
}