## The R code caches a matrix x that can be inversed.
## 

## In the function makeCacheMatrix, four functions are stored that are later needed
## in the CacheSolve dialogue. This is more or less what was done in the makeVector example.

makeCacheMatrix <- function(x = matrix()) {
## to repeatedly use the function m must be emptied
    m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## the values of our function are inversed by the inverse function
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve should do the job of inversing the matrix.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
