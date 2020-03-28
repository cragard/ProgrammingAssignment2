makeVector <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        invMatrix <- function(solve) m <<- solve
        getinvMatrix <- function() m
        list(set = set, get = get,
             invMatrix = invMatrix,
             getinvMatrix = getinvMatrix)
}

cacheSolve <- function(x, ...) {
        m <- x$getinvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$invMatrix(m)
        m
}