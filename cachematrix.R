##makeCacheMatrix takes a matrix as an argument and caches it. makeCacheMatrix with its argument goes in cacheSolve as an argument

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                              
    set <- function(y) {                     
        x <<- y                             
        inv <<- NULL                        
    }
    get <- function() x                     
     
      setinverse <- function(inverse) inv <<- inverse  
      getinverse <- function() inv                     
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
    }

## this function gives output as the inverse of the matrix which was the argument of makeCacheMatrix
## if there is the inverse of the matrix which was the argument of makeCacheMatrix in the cache memory this function will retrieve it else solve for the inverse
cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
 }
