## These functions create a special object that
## stores a numeric matrix and cache's its inverse


## This first function creates a special "matrix" which
## is really a list containing a function to
## (1) set the value of the matrix, (2) get the value of the matrix
## (3) set the value of the inverse matrix, (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This next function calculates the inverse of the special "matrix" created
## by the above function, after first checking to see if the inverse has
## already been calculated, returning the cached value if it exists

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
