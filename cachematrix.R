# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly. The following functions cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It assumes the matrix is always invertible.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x){
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("please wait, getting cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
}

##### EXAMPLE EXECUTION OF CODE #####
# > example <- makeCacheMatrix(rbind(c(4,7),c(2,6)))
# > example$get()
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > cacheSolve(example)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > cacheSolve(example)
# please wait, getting cached data...
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
