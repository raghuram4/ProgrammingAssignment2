
## Aim : To write a pair of functions that cache the inverse of a matrix
## The two functions are "makeCacheMatrix" and "cacheSolve"

## 1. makeCacheMatrix - creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve
  
     t <- NULL
     set <- function(y) {
       
       # use `<<-` to assign a value to an object in an environment 
       # different from the current environment. 
       
       x <<- y
       t <<- NULL
     }
     get <- function() x
     setcacheInverse <- function(cacheInverse)  t <- cacheInverse
     getcacheInverse <- function() t
     list(set = set, get = get, setcacheInverse = setcacheInverse, getcacheInverse = getcacheInverse)
}



## 2. cacheSolve -  computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
        t <- x$getcacheInverse()
        
        # if the inverse has already been calculated
        
        if(!is.null(t)) {
          message("getting cached data")
          return(t)
        }
        
        # otherwise, calculates the inverse
        
        data <- x$get()
        t <- solve(data,...)
        
        # sets the value of the inverse in the cache via the setinv function.
        
        x$setcacheInverse(t)
        t
}

## ---------------Checking the program------------------------
   ## m <- matrix(rnorm(16),4,4)
   ## m1 <- makeCacheMatrix(m)
   ## cacheSolve(m1)
   
   ## [,1]       [,2]       [,3]       [,4]
   ## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
   ## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
   ## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
   ## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830






