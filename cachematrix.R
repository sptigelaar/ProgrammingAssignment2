## These functions calculate the inverse of a matrix. 
## If the inverse was calculated before, the result is stored in the cache
## These functions are to be called as cacheSolve(makeCacheMatrix(M)) 
## where M is an invertible matrix.

## The nested function which stores the cached values and 
## sends either the cached value or the calculated value to the parent function

makeCacheMatrix <- function(x = matrix()) {

## Set the inverse to NULL at the first run of the function
      inverse <- NULL

## The functions to transfer the matrix or data to the parent function
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inverse <<- solve
      getInverse <- function() inverse

## Put the functions in a list so they can be accesssed by the parent function
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



cacheSolve <- function(x,...){
## Check whether the inverse is already cached
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("Inverse matrix from cache")
## If the inverse was already in cache, return the inverse and end function
            return (inverse)
      }
## if the inverse was not cached, get the matrix from the nested function and calculate the inverse
      matrix_data <- x$get()
      inverse <- solve(matrix_data,...)
## Set the calculated inverse to the cache via the nested function
      x$setInverse(inverse)
      inverse
}
