## Functions below are used to create a special object that
## stores matrix and cache's its inverse

## Creates a list of functions, that can set the matrix,
## get the matrix, set and get its inverse (cache the
##inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    SetInverse <- function(inverse) inv <<- inverse
    GetInverse <- function() inv
    list(set=set, get=get, SetInverse=SetInverse, GetInverse=GetInverse)

}


## Calculates the inverse of the matrix which is returned by
## makeCache Matrix. If the inverse was already calculated,
##prints out a message("getting cached data"), then retrieves
##the inverse matrix from cache.

cacheSolve <- function(x, ...) {
    inv <- x$GetInverse()
    if(!is.null(inv)) {
        print("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$SetInverse(inv)
    inv
        
}
