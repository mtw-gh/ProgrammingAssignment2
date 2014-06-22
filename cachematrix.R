## makeCacheMatrix creates a vector containing a set of functions:
# 1. set: store the value of a matrix in the global environment
# 2. get: get the value of the stored matrix
# 3. set_im:  calculate the inverse of the stored matrix and store in global space
# 4. get_im:  get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # print(m)
        set <- function(y) {
                #set "x" in parent environment
                x <<- y
                # set "m" in parent environment
                m <<- NULL
        }
        get <- function() x
        set_im <- function(solve) m <<- solve
        get_im <- function() m
        list(set = set, get = get,
             set_im = set_im,
             get_im = get_im)
   
}



## cachSolve returns the inverse of the existing matrix "x".
# first it checks to see if the inverses matrix "m" has already been
# calculated and stored, if not it will calculate and store it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_im()
        rows <- nrow(x$get())
        cols <- ncol(x$get())
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- matrix(x$get(), nrow=rows,ncol=cols)
        m <- solve(data, ...)
        x$set_im(m)
        m
}
