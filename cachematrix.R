## These two functions are to caching the inverse of a matrix, 
## if the contents of a matrix are not changing

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() {
				x
		}
        setinverse <- function(inversematrix) {
				i <<- inversematrix
		}
        getinverse <- function() {
				i
		}
        l <- list(setmatrix, getmatrix, setinverse, getinverse)
		d <- list(c("set", "get"), c("matrix", "inverse"))
		setmatrix(matrix(l, nrow=2, ncol=2, byrow=FALSE, dimnames=d))
}


## computes the inverse of the special "matrix". If the inverse has 
## already been calculated, then the cacheSolve retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x["get", "inverse"][[1]]()
        if(!is.null(i)) {
                message("getting inverse matrix")
                return(i)
        }
        data <- x["get", "matrix"][[1]]()
        i <- solve(data)
        x["set", "inverse"][[1]](i)
        i
}
