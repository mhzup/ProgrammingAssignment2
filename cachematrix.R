## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 CurrMatrix <- NULL
 PrevMatrix <- NULL
 set <- function(PrevMatrix) {
  x <<- PrevMatrix
  CurrMatrix <<- NULL
 }
 get <- function() x
 setsolve <- function(solve) CurrMatrix <<- solve
 getsolve <- function() CurrMatrix
 list(set = set, get = get,
  setsolve = setsolve,
  getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
 CurrMatrix <- x$getsolve()
 if(!is.null(CurrMatrix)) {
   message("getting cached data")
   return(CurrMatrix)
 }
 data <- x$get()
 CurrMatrix <- solve(data, ...)
 x$setsolve(CurrMatrix)
 CurrMatrix
}

