## Functions that cache the inverse of a matrix (based on the 
##assumption that matrix always invertible)

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                   ##Initialize the inverse
    
    set <- function(matrix) {
        m <<-matrix
        i <<-NULL
    }                           ##Set the Matrix
    
    get <- function() m         ##Get the Matrix
    
    setInverse <- function(inverse) {
        i <<- inverse
    }                           ##Set the inverse of Matrix
    
    getInverse <- function() i  ##Get the inverse of Matrix
    
    list( set = set, 
          get = get, 
         setInverse = setInverse, 
         getInverse = getInverse 
         )                      ##Return a list of Methods
}


## The following function computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()         ## Return a matrix that is the inverse of 'x'
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }                           ##Return the inverse if it's already set
    
    data <- x$get()             ##Get Matrix from object
    
    m <- solve(data)            ##Compute the inverse of Matrix
    
    x$setInverse(m)             ##Set the inverse to the object
    
    m                           ##Return Matrix
}
