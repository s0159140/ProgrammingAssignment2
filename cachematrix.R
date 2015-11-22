# Coursera Data Science Course - R programming - Assignment 2

## makeCacheMatrix is a function that returns a list of functions
## Its purpose is to store a martix and a cached value of the inverse of the 
## matrix. The function will return a list of the following functions:
## * setMatrix      set the value of a matrix
## * getMatrix      get the value of a matrix
## * cacheInverse   store the inverse of the matrix in the cache
## * getInverse     get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  ### The purpose of the makeCacheMatrix is to store a cached value or hold NULL when
  ### when nothing is cached yet.
  ### At the beginning of running the function, there is nothing cached yet, so
  ### we set it to NULL
  cachedValue <- NULL
  
  ### setMatrix defines a new matrix
  setMatrix <- function(newValue) {
    ### To define a matrix, we assign newValue to x and clear the cashedValue 
    ### by setting it to NULL.
    ### This part of coding also shows the use of the 'superassignment' operator
    ### "<<-" in combination with lexical scoping. It starts looking in the
    ### enclosing environment and works it's way up to the global
    ### environment until it finds a variable x and assigns newValue to it
    ### If no variable x can be found, the superassignment operator will create it in
    ### the global environment.
    x <<- newValue
    cachedValue <<- NULL
  }
  
  ### getMatrix returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  #### cacheInverse caches the given argument 
  cacheInverse <- function(solve) {
    cachedValue <<- solve
  }
  
  ### getInverse returns the cached value
  getInverse <- function() {
    cachedValue
  }
  
  ### The output of the makeCacheMatrix function is a list of our 4 defined 
  ### functions: setMatrix, getMatrix, cacheInverse, getInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix we created using the makeCacheMatrix.
## To save up computing time, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache, prints the message "getting cached data" and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setInverse function.

cacheSolve <- function(y, ...) {
  ### We first get the cached value using getInverse()
  inverse <- y$getInverse()
  ### If a cached value exists in "inverse",  it will be returned...
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  ### ... if there is no value for "inverse" in the cache, the function will get 
  ### the matrix, caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  ### The output of cacheSolve is to return the inverse matrix
  inverse
