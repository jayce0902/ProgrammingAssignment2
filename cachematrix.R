## This is the first function named as makeCacheMatrix. It's purpose is to create a,
## that is a list that contains functions to:

## 1. Set the elements of the  special "matrix".
## 2. Get the elements of the  special "matrix".
## 3. Set the elements of the  inverse of the special "matrix". 
## 4. Get the elements of the  inverse of the special "matrix". 

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


# This 2nd function named cacheSolve computes the inverse of the special "matrix" created with the 1st function.
# But, first it checks to see if the inverse has already been computed. If so, it gets the inverse from the 
# cache and skips the calculation. Otherwise, it computes the inverse of the matrix and sets the value of the 
# inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
