## Below are 2 functions that are used to create a special "matrix" object that can cache its inverse.

## The first function, "makeCacheMatrix", creates a special "matrix", which contains a function to
##1. set value of the matrix
##2. get value of the matrix
##3. set value of the inverse
##4. get value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The second function, "cacheSolve", computes the inverse of the special "matrix" created from the above function, "makeCacheMatrix".
## It first checks if the inverse has already been computed. If so, it will get the inverse from the cache and skip the computation. 
## If not, it will calculate the inverse of the data(mat) and set the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
