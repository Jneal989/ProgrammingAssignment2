## Allows a matrix and it's inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvs <- function(invs) i <<- invs
        getinvs <- function() i
        list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## Inverts the cached matrix, if the matrix has been inverted, the function
## prints the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinvs()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinvs(i)
        i
}
