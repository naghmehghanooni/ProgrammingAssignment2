## The first function, makeCacheMatrix, make a matrix and the
## function cacheSolve inverse the input matrix. If the inversed value
## has already been calculated the cacheSolve only get that value from
## the containing environment. Else if the inversed value has not 
## calculated the cacheSolve calculates it. 

## makeCacheMatrix puts null for the inverse of the matrix x, then
## the methods set (set the matrix x), get (get the matrix x), 
## setinversed (set the inversed matrix of x) and 
## getinversed (get the inversed matrix of x) are written
## which the last three will be used in the function
## cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinversed <- function(inversed) inv <<- inversed
        getinversed <- function() inv
        list(set = set, get = get,
                setinversed = setinversed,
                getinversed = getinversed)
}


## This function solves the inverse of matrix x. 
## It first checks if the inversed matrix has already been calculated.
## If yes, it goes to the if loop and will get the cached data. 
## If no, it will skip the if loop and goes to the next lines and
## calculates the inverse matrix of x with the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinversed()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinversed(inv)
        inv
}
