## Functions written below (makeCacheMatrix and cacheSolve) allow the user 
## to cache the inverse of a matrix

## makeCacheMatrix: creates a special "matrix" object of the numeric 
## matrix to set and get the numeric matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<-solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: computes the inverse of the special"matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    return(inv)
}
