##The following functions will help to cache the inverse of given matrix to
##save running time

##Store the inverse matrix of given matrix which has been calculated before

makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) m<<-solve
  getinverse<-function()m
  list(set=set,setinverse=setinverse,getinverse=getinverse)
}


##If the inverse matrix of griven matrix has been calculate before,then 
##the function can immediately get the result from cache data.If not 
##however, the function can calculate the inverse function by itself.

cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m                ##Return a matrix that is the inverse of "x"
}
