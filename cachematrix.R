     
                #//ProgrammingAssignment2#//

#//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#//
     #makeCacheMatrix is a function which creates the "matrix" object.
     # Return value of this function is a list of following four functions  
     #set,get,setinverse and getinverse
#//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#//
      makeCacheMatrix<-function(x=matrix()){ 
        
         Inv<-NULL            
         
   #::::::::::caching  and receiving matrix variable "x" ::::::::::::  
         set<-function(y){
              x<<-y           
              Inv<<- NULL
            }  
        get<-function() x     
        
   #::::::::::: caching and receiving inverse matrix "Inv" :::::::::::
        setinverse<-function(inverse) 
        Inv<<-inverse          
        getinverse<-function()
            Inv 
   
     list(set=set,get=get,setinverse=setinverse,
             getinverse=getinverse)
        }
      
  #//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#//
       # cacheSolve function takes argument x which is a 
       #list of functions created by makeCacheMatrix function
       # It returns the inverse of matrix created by makeCacheMatrix
 #//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#//
      
cacheSolve<-function(x,...){
      
  #:::::::: search for inverse if it was cache::::::::::::::::::::
          Inv<-x$getinverse() 
      if(!is.null(Inv)){ 
             message("Getting Cache Data...")
             return(Inv)
             }
  #:::::::::: getting matrix and checking  invertibility :::::::::   
          data <-x$get() 
           if(det(data)==0){           
           message("Sorry, matrix is not invertible !!")
           }
  #::::::::computing and catching computed inverse:::::::::::::::;     
           Inv<-solve(data)  
           x$setinverse(Inv) 
           Inv
           
   }
      
      