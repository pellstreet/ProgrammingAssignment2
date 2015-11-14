## JH Data Science Certificate - R Programming Course - Week 3
## Assignment 2: Caching the Inverse of a Matrix
## Emanuele Pellichero

## The pair of functions below are used to cache the inverse of a matrix.

## The first function, makeCacheMatriX creates a special "matrix", which is really a list containing a 
## function to:

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

## The second function, cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function


## THe makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

