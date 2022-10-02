## `makeCacheMatrix` and `cacheSolve` together can be used to cache
## computations of inversions of matrices. Example:
## >>> mat <- rbind(c(4, -3, 5), c(1, 0, 3), c(-1, 5, 2))
## >>> x <- makeCacheMatrix(mat)
## >>> inverse <- cacheSolve(x)  # Only the first call will result in a call to `solve`.
## >>> inverse
##       [,1]  [,2]  [,3]
## [1,]  0.75 -1.55  0.45
## [2,]  0.25 -0.65  0.35
## [3,] -0.25  0.85 -0.15

## This function takes in a matrix (`x`) and returns a list of functions
## which acts as a "classic object" with getters and setters for the passed
## matrix (`x`) as well it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    # This variable acts as our cache to hold the inverse of our matrix.
    inverse <- NULL

    # Getter for the original matrix.
    getOriginalMatrix <- function()
        x

    # Setter for the original matrix.
    setOriginalMatrix <- function(newMat) {
        x <<- newMat

        # If original matrix has changed, we reset it's cached inverse.
        inverse <<- NULL
    }

    # Getter for the inverse.
    getInverseMatrix <- function()
        inverse

    # Setter for the inverse.
    setInverseMatrix <- function(inv)
        inverse <<- inv

    # Return a named list of the getters/setters of the matrix
    # and its inverse.
    list(
        getOriginalMatrix = getOriginalMatrix,
        setOriginalMatrix = setOriginalMatrix,
        getInverseMatrix = getInverseMatrix,
        setInverseMatrix = setInverseMatrix
    )
}


## This function takes in an object returned by `makeCacheMatrix`
## and returns its inverse. The inverse is cached (by closure) such
## that for the same matrix, only the first call to `cacheSolve` computes
## the inverse and subsequent calls return the inverse from cache without
## recomputation.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverseMatrix()

    # If inverse hasn't been computed yet, compute and cache it.
    if (is.null(inverse)) {
        original <- x$getOriginalMatrix()
        inverse <- solve(original)
        x$setInverseMatrix(inverse)
    }

    inverse
}
