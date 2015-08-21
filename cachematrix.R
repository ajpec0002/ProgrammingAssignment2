################################################################################################################################################################################################################
##
## Description:
## 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions makeCacheMatrix() and cacheSolve() below implements this caching.
## 
## Sample Run:
##
## rsrcloc <- '~/rprog/ProgrammingAssignment2/cachematrix.R'     # location of the cachematrix.R file. This is just a sample.
## source(rsrcloc)                                               # source the R file
## m <- matrix( c(2,3,4,5), nrow=2,ncol=2)                       # create a square matrix
## cm <- makeCacheMatrix(m)                                      # pass the matrix to the makeCacheMatrix() and assign output to a variable e.g. cm
## cacheSolve(cm)                                                # pass list cm to cacheSolve(). Since cache is still empty at this point the matrix inverse will be computed using solve() and store to cache.
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## cacheSolve(cm)                                               # calling it again will retrieve the value from cache.
## returning cached matrix inverse
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
##
################################################################################################################################################################################################################






## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #variable to hold the cache inverse
  inv_cache <- NULL
  
  #set the input matrix
  #set inverse matrix (initialize to NULL)
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  
  #get the value of the input matrix
  get <- function() x
  
  #set the computed matrix inverse to cache
  setinv <- function(inv_val) inv_cache <<- inv_val
  
  #get the matrix inverse from cache
  getinv <- function() inv_cache
  
  #returns the list of getters and setters of the input matrix and matrix inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated (and the matrix has not changed), it retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  #get matrix inverse from cache      
  inv_cache <- x$getinv()
  
  # if value from cache is not empty, return the cache matrix inverse
  if(!is.null(inv_cache)) {
    message("returning cached matrix inverse")
    # return cache matrix inverse
    return(inv_cache)
  }
  
  # get the input matrix from the cache
  data <- x$get()
  
  #compute the inverse of the input matrix
  inv_cache <- solve(data)
  
  # set the computed matrix inverse to the cache
  x$setinv(inv_cache)
  
  # return the computed matrix inverse
  inv_cache
}
