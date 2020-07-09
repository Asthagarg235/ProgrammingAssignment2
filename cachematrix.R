## This function create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL                       ## intialise inv as NULL
        set <- function(y){                ##define the set function to assign new
                x <<- y                    ##value of  matrix in parent environment
                inv <<- NULL               ##if there is a new matrix then reset it as NULL
        }
        get <- function() {x}              ## define the get function
        setInverse <- function(inverse) {inv <<- inverse}    ## assign the value of inv in parent environment
        getInverse <- function() {inv}                       ## get the value of inv where called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {   ##return a matrix that is inverse of x If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
        inv <- x$getInverse()     
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- xget()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}

