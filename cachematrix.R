##Coursera - R Programming
##Programming Assignment 2
##April 2012

## The purpose of the function below are to enable caching of the inverse matrix
## to avoid re-calculating the inverse of the matrix each time it is required

## This is achieved using two functions.  
## The first is a special function to create the matrix (makeCacheMatrix)
## The second uses the special function and creates an inverse matrix caching the answer (cacheSolve)
## the cached inverse can then be used when required without recalculating it.



## The makeCacheMatrix is a function that creates a special "matrix"
## It contains a list of functions that can be used to 
## Get and set the Matrix and get and set the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  #initilise 
  m <- NULL
  
  #the set functions for the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #function to get the matrix
  get <- function() x
  
  #get and set functions for the inverse of the matrix
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  #a list of the functions defined
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  

}


## The cache Solve function is a special function that calculates the inverse of a matrix
## It takes the makeCacheMatrix as a parameter and uses its get and set methods
## Before it calculates the inverse it checks if the inverse has been calculated and used the cached version if it exists
## Otherwise it will calculate the inverse and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  #Check if the invese exists if it dies return the cached version
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #get the stored matrix
  data <- x$get()
  
  #calcluate the invese of the matrix
  m <- solve(data, ...)
  
  #set the inverse
  x$setInverse(m)
  
  #retrun the inverse
  m


}
