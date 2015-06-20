## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   ## Initializing result variable to NULL
   minv <- NULL
   ## This function is to change the matrix stored in the main function
   set <- function(y) {
    x <<- y
    minv <<- NULL
   }
   ## This function simply return the matrix stored in the main function
   get <- function() { 
     x
   }
   ## This function stores the computed result of the inverse matrix in 
   ## the variable minv of the main function
  setminv <- function(result) { 
    minv <<- result
  }
  ## This function simply return the value stored in the minv value
  getminv <- function() minv
  ## Use the list function to store the four functions in the makeCacheMatrix function
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## getting the cache data
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  ## If there is not cache data, we calculate and store the result
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
