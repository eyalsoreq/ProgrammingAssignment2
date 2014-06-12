##  makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
##  To run start by sourcing the file: source("cachematrix.R") and create an empty object: m<- makeCacheMatrix( )
##  initailize with a matrix: m$set( matrix( c(0, 2, 2, 0 ), 2, 2))
##  m$get() will return the matrix, then cacheSolve( m ) will return either the computed inv or the cached one depending 
##  on any changes to the matrix in question

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL # a handle to indicate change
  set <- function(y) {
    x <<- y # assign the matrix y into the global var x
    m <<- NULL # set the global inv matrix to null
  }
  get <- function() x
  setInv <- function(solve) m <<- solve # place the cahched inv in the global matrix 
  getInv <- function() m 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## This is the function that actually does all the work. Upon getting data it frist checks wheter the global invmatrix is NULL
## If it isn't Null it returns it and indicates that by a console messege, if not it computes the inv and stores it in the 
## global invmatrix  

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
