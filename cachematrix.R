## used the following matrices to use as tests after functions were written


mdat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2")))
mdat

gdat <- matrix(c(4,6, 14,16), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2")))
gdat



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##assumption that x: is a square invertible matrix
  ##1. set the matrix
  ##2. get the matrix
  ##3. set the inverse
  ##4. get the inverse
  
  i <- NULL
  set <- function(y) {
    ## note the use of '<<-' to put variable in an environment you currently are not in
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inv) {i <<- inv}
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  
}
