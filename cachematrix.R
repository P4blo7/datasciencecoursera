## Put comments here that give an overall description of what your
## functions do

## This creates a special matrix object that stores
## itself then caches its inverse, allowing it to
## be retrieved later.
## As an example type or copy the following code to
## the console and enter it. Make sure the script
## has been run and the functions are in the environment.
## a <- makeCacheMatrix(x = matrix(c(1, 2, 3 ,4), nrow = 2, ncol = 2, byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This calculates the matrix inverse, and is able
## to store it, retrieve it, and will let you know
## if the matrix has already been cached.
## It makes use of the framework set up earlier.
## To test, type or copy the following code in the
## console:
## cacheSolve(a)
## Enter this a second time.
## You will get the 'getting cached data' message.
## You can enter the following code:
## solve(matrix(c(1, 2, 3 ,4), nrow = 2, ncol = 2, byrow = TRUE))
## You will get the same result as cacheSolve(a)
## Proving it has been inverted correctly.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}