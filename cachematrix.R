## Comment on the overall assignment
## ref to instruction on assigment from https://github.com/rdpeng/ProgrammingAssignment2
## Given that solve(x) works with quare matrices and non-singular matrices have inverses the code below is setup
## to take as input " x = numeric" where x is the side of the resultant matrix. The matrix is created by random sampling of numbers
## between 1 to 100. Once the matrix is constructed, the inverse of the matrix is stored in the cache.

## Comment describing function makeCachMatrix()
## This function requires a user to input the desired square dimenion of a matrix. For example,
## myMatrix <- makeCacheMatrix(5) will create a (5,5) sqaure matrix populated by a random sample of 25 numbers.
## the rest of the function defines additional functions that can be used to retrieve the matrix or even the inverse matrix
## when called through cacheSolve()per assignment instructions.

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        setMatrixSide <- function(y) {
                x <<- y
                invM <<- NULL
        }
        sampleData <- sample(1:100, (x^2))
        m <- matrix(sampleData, nrow = x,ncol = x)
        getMatrixSide <- function() x
        invM <- solve(m)
        setInvMatrix <- function(invM) invM <<- invM
        getMatrix <- function() m
        getInvMatrix <- function() invM
        list(setMatrixSide = setMatrixSide, getMatrixSide = getMatrixSide, setInvMatrix = setInvMatrix, getMatrix = getMatrix, getInvMatrix = getInvMatrix)
}

## Comment describing function cacheSolve() 
## This is  designed to retrieve the inverse matrix as created by the makeCacheMatrix function above which is stored
## in the cache. If there is no inverse matrix stored in cache, it will proceed to create a new inverse matrix of a desired
## square dimension that be created by x$setMatrixSide() call.
## For example myMatrix$setMatrixSide(4) will create a new 4 X 4 matrix, the inverse of which, is returned to the user,
## and stored in the cache for future retrieval.

cacheSolve <- function(x,...){
        invM <- x$getInvMatrix()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data1 <- x$getMatrixSide()
        sampleData <- sample(1:100, data1^2)
        invM <- solve(matrix(sampleData, nrow = data1, ncol=data1))
        x$setInvMatrix(invM)
        invM
}