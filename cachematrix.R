## Functions to cache the inverse of a matrix 
## R Programming Assignment 2

## This Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  
  i <- NULL   #set inv to NULL
  set <- function(y) {  #set matrix
    x <<- y      #caches matrix
    i <<- NULL   
  }
  get <- function() x    #get value of the matrix
  setinverse <- function(inverse) i<<-inverse  #set value of inverse of the function
  getinverse <- function() i  #get value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()  
  if(!is.null(i)) {   #If the inverse has already been created
    message("getting cached data")  #gets the cached result instead of computation  
    return(i)
  }
  d<-x$get()
  i<-solve(d)
  x$setinverse(i)
  i
}
