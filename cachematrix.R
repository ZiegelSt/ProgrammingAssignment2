## Put comments here that give an overall description of what your
## functions do

## Takes a matrix as input. Returns a list of 4 functions which 
## may be used to get and set a matrix as well as get and set its 
## inverse. Upon initialization or call to set function the inverse 
## gets (re)set to NULLº

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mIn){
    m <<- mIn
    inv <<- NULL
  }
  get <- function() m
  setInv <- function (inInv) inv <<- inInv
  getInv <- function () inv
  list(set=set,get=get,
       setInv=setInv,getInv=getInv)
}


## takes a 'cachematrix' and a argument list as input. Computes
## the inverse of the matrix (using the solve function subject to
## the argument list), uses the setInv to cache the inverse and 
## returns the inverse. If called again the inverse is returned 
## from cache without computation. Resetting the matrix causes loss
## of cached data.

cacheSolve <- function(m, ...) {
  inv <- m$getInv()
  if (!is.null(inv)){
    print("getting cached inverse")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data,...)
  m$setInv(inv)
  inv
}
