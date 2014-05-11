## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is really a list 
## containing a function to 
## 1. set the value of the matrix, 
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created 
## with the makeCacheMatrix function. If the inverse has already been 
## calculated, then it retrieve the inverse from the cache. Otherwise, 
## it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
