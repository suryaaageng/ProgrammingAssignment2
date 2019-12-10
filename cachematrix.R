## in this script, i will try to make function that caching the inversion of matrix 


## this function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function()inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse) 
}


## if the inverse has already been calculated and the matrix has not change, than it should retrieve the inverse from cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse
  if (!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse
  inv
}
