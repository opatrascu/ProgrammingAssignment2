## The purpose of the two functions below is the retrieve the inverse of a matrix from cache
## instead of calculating it repeatedly. This can be important, as calculating the inverse of 
## a matrix is typically a costly computational operation.


## makeCacheMatrix is a function creating a special matrix which is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinvs <- function(solve) invs <<- solve
        getinvs <- function() invs
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)
}


## cacheSolve computes the inverse of a special matrix created above.
## If the inverse has already been calculated, then this number is retrieved from Cache, instead of it
## being calculated once again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invs <- x$getinvs()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinvs(invs)
        invs
        
}
