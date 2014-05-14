## Two functions makeCacheMatrix and cacheSolve make use of lexical scoping
## to cache matrix inverse calculations.  The cacheSolve function
## is used in a similar way to the Solve function for
## matrix inverse computation


## makeCacheMatrix creates a special "matrix" which is a list, 
## as in the case in the question which is 
## really a matrix containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the matrix inverse
## of the special "matrix" created with the above function: makeCacheMatrix
## However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the matrix inverse
## from the cache and skips the computation. Otherwise, 
## it calculates the matrix inverse of the data and sets the value 
## of the matrix inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
