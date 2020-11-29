## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#there are two functions makeCacheMatrix and cacheSolve
#the function is taking an argument of x which is the matrix
makeCacheMatrix <- function(x = matrix()) {
  #NULL is assigned to variable inv
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #assigns the inverse of x to inv
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  #computing the inverse of the matrix
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
