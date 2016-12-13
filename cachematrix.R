## Very much as for the examples given for the Mean of a Vector, two functions are provided
## for the matrix inversion and caching: MakeCacheMatrix basically creates the necessary objects
## (which includes the input data x, status information of whether the input has already been 
## inverted (variable "inverted") and the according getters and setters). For each new input or 
## change of an existing value already inverted, the function makeCacheMatrix automatically sets
## "inverted" to NULL and provides the necessary overall objects for cacheSolve to be processed. 
## So as a first step, an input matrix needs to be processed by makeCacheMatrix first, so the created
## object can be processed by cacheSolve. 
## CacheSolve checks first if the according matrix has already been inverted. If so, it refers
## to the cached data (object "inverted"). If not, the function fetches the input matrix put into
## makeCacheMatrix and inverts it using function solve(). Once done, it calls function "setinverse"
## defined in makeCacheMatrix to store the according inverted matrix. Now "inverted" is also not NULL
## any more. So with the next run or runs for this particular object, the cached interted matrix
## will be called. No recalculation is necessary. 

## makeCacheMatrix creates/defines/initializes all necessary objects for compuations in cacheSolve().
## Note that it needs to be executed prior to cacheSolve, since it creates the necessary objects.

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y){
    x <<-y
    inverted <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) inverted <<- inverse
  getinverse <- function() inverted
  list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function requires the objects created in makeCacheMatrix as input, since it refers to its
## objects. The most important function used here, though, is function solve(), which calculates
## the inverted matrix of the input - but without cacheing. Cacheing is determined by checking if
## an inverted matrix for the input give already exists, which is retrieved using getinverse(). Only
## if the inverted matrix has not been calculated for the according object yet (so: value equals
## NULL), the inverted matrix is calculated, stored in "inverted" and "written back" using
## setinverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## No check for non-invertable matrix, since assignment assumes that all matrices used can be inverted
  ## solve() would cast an error and exit upon processing a non-invertible matrix
  inverted<-x$getinverse()
  if(!is.null(inverted)){
    message("Getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data,...)
  x$setinverse(inverted)
  inverted
}
