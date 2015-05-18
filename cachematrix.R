## Many computations such as matrix inversion can be costly in terms of CPU memory
## Here, I have written a set of functions that computes the inverse of a square matrix and 
## and caches it's inverse in order to save the program from having to re-run the computation
## thereby saving memory

## The makeCacheMatrix function is an R object that creates a special square matrix that can be
## cached for future use (for example. by cacheSolve).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function is an R object that computes the inverse of a matrix (x) 
##created by makeCacheMatrix. If the inverse of the matrix has already been computed (i.e. the
## program has already been run) then the cached inverse is returned after printing the message
## "getting cached data".

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
