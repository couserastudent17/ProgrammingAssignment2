## Overall comment: the function makeCacheMatrix() creates a structure of functions to operate with cache
## The funciton cacheSolve() checks if the matrix had already been inverted and if not it inverts the matrix and
## writes the inverted value to cache

## We assume that the matrix is always invertible as in was told in the assignment so if the matrix is singular, 
## we unfortunately will get an error like that: 
## Error in solve.default(data, ...) : Lapack routine dgesv: system is exactly singular: ... 


## The function makeCacheMatrix() takes a matrix (we assume it to be square and invertible for the assignment) and creates a structure 
## in which the functions set, get, setinverse and getinverse are defined with x and inv_matrix as local matrices
## the set() function will assign the matrix we want to invert to x in the cache
## the get() function will just return the initial matrix x from cache
## the setinverse() function will assign the inverse of x to inv_matrix in the cache
## the getinverse() function will just return the inverted matrix inv_matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    ## returning the list of functions to operate with cache
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The functio cacheSolve() returns a matrix that is inverse of x. It checks if the have the inverse of x in cache
## and if not, it calculates the inverse and writes it to cache

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinverse()
    ## checking the cache
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    ## calculating inverse
    inv_matrix <- solve(data, ...)
    ## writing to cache
    x$setinverse(inv_matrix)
    inv_matrix
}
