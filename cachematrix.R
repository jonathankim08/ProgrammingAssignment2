## The following two functions help compute the inverse of a matrix. Because computing
## the inverse can be computationally intensive, the functions allow for an initial check
## to see if the inverse has already been calculated and cached, potentially saving time.


## The below function creates a list of functions that can set the matrix, get the matrix, 
## set the value of the matrix inverse, and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## The below function calculates and returns the inverse of the matrix in the above function. 
## If the inverse has already been calculated, the function returns the cached matrix inverse. 
## If it has not been calculated, the fuction calculates and returns the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}