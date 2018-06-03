## The cachematrix.R script contains the makeCacheMatrix and cacheSolve functions
## Written by Steve Jones for the Coursera "R Programming" week 3 assignment
## These were developed from the cacheMean, makeVecter functions, and the get/set
## tutorial written by Leonard Greski https://github.com/lgreski/datasciencectacontent
## These functions will cache the inverse of a matrix (if the matrix has a valid inverse)

## The makeCacheMatrix takes a matrix and returns a list of four functions:
## get and set the value of a matrix, and get and set an inverse of the matrix  
makeCacheMatrix <- function(x = matrix()) {
  
## First, initialize the objects: 
## x (the matrix) has already been intialized in function declaration.
## m will be an inverse of the matrix
  m<-NULL
  
## the set function will assign the objects x and m to the parent environment
  set <- function(y) {
    x <<- y           ## this will assign the value of x, without repeating name
    m <<- NULL        ## any value of m cached in memory will be cleared
  }

## the get function will retrieve x from the parent environment  
  get <- function() x
  
## setinverse and getinverse do what set and get did, but uses the inverse of 
## the matrix (as defined by the solve function)  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
## Finally, return a list of the functions that were just defined  
  list(set = set,                ##names the set() function as set 
       get = get,                ##names the get() function as get
       setinverse = setinverse,  ##names the setinverse() function as setinverse
       getinverse = getinverse)  ##names the getinverse() function as getinverse
}


## The cacheSolve function will return the inverse from either the cache if 
## it has been calculated, or calculate the solution if has not been stored
cacheSolve <- function(x, ...) { 
## get the inverse matrix from makeCacheMatrix
    m <- x$getinverse()
 
## if the inverse has been calculated it has data (not null)  
  if(!is.null(m)) {
## if the inverse has been calculated, the value is returned
## with message that cached data is being retrieved
    message("getting cached data")
    return(m)
  }

## if the inverse has not been calculated it is null
  data <- x$get()         ## data is retrieved using get() from makeCacheMatrix 
  m <- solve(data, ...)   ## the inverse is calculated using solve()
  x$setinverse(m)         ## the inverse is cached using setinverse()
  m                       ## the inverse is returned
}
