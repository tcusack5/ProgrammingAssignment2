## This R document contains a pair of documents that will 
## cache the inverse of a matrix. This program will help to replicate 
## a time consuming computation. This program will will take advantage
## of the scoping rules of the R language and show how they can be 
## manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                     ##initializes inverse matrix
        set <- function(y){           ##sets matrix
                x <<- y
                i <<- NULL
        }
        
        get <- function(){            ##returns matrix
                x
        }
        setinverse <- function(inverse){ ##sets inverse of matrix to i
                i <<- inverse
        }
        getinverse <- function(){     ##gets inverse of matrix
                i
        }
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse) ##returns list of functions used
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()           ##gets inverse matrix set to i
        if(!is.null(i)) {             ##returns matrix if already inverse
                message("Retrieving cached matrix")
                return(i)
        }
        data <- x$get()               ##retrieves matrix from object
        i <- solve(data, ...)         ##creates inverse of matrix
        x$setinverse(i)               ##sets inverse to object i
        i                             ##returns inverse matrix i
}
