# Test 1


test1 <- function(){
  myvar1 <- matrix(1:10,nrow=5,ncol=2)
  print("testing normal matrix variable")
  
  testBoolean <- TRUE
  result <- mytranspose(myvar1)

  for(row in 1:nrow(myvar1)){
    for(col in 1:ncol(myvar1)){
      if (result[col,row] != myvar1[row,col]){
        testBoolean <- FALSE
      }
    }
  }
  if(testBoolean){
    print("Normal Matrix variable test has been succesfully done!!")
  } else{
    print("Normal Matrix variable test has been failed!!")
  }
}


test2 <- function(){
  myvar1 <- matrix(NA, nrow=0, ncol=0)
  print("testing Null varaible")
  
  result <- mytranspose(myvar1)
  testBoolean <- identical(result,myvar1)
  if(testBoolean){
    print("Null variable test has been succesfully done!!")
  }else{
    print("Null variable test has been failed!!")
  }
}

test3 <- function(){
  myvar1 <- matrix(c(1,2),nrow=1,ncol=2)
  
  print("Testing Vector type variable")
  
  testBoolean <- TRUE
  result <- mytranspose(myvar1)
  
  for (row in 1:nrow(myvar1)){
    for(col in 1:ncol(myvar1)){
      if (result[col,row]!=myvar1[row,col]){
        testBoolean <- FALSE
      }
    }
  }
  if(testBoolean){
    print("Vector type variable test1 has been succesfully done!!")
  }else{
    print("Vector type variable test1 has been failed!!")
  }
  
  
} # end of test 3

test4 <- function(){
  myvar1 <- matrix(c(1,2),nrow=2,ncol=1)
  
  print("Testing Vector type variable")
  
  testBoolean <- TRUE
  result <- mytranspose(myvar1)
  
  for (row in 1:nrow(myvar1)){
    for(col in 1:ncol(myvar1)){
      if (result[col,row]!=myvar1[row,col]){
        testBoolean <- FALSE
      }
    }
  }
  if(testBoolean){
    print("Vector type variable test2 has been succesfully done!!")
  }else{
    print("Vector type variable test2 has been failed!!")
  }
} # end of test 4

test5 <- function(){
  myvar2 <- c(1,2,NA,3)
  
  print("Testing Vector type variable")
  
  testBoolean <- TRUE
  result <- mytranspose(myvar2)
  
  for (row in length(myvar2)){
    if(result[row,1] != myvar2[row]){
      testBoolean <- FALSE
    }
  }
  
  if(testBoolean){
    print("Vector type variable test has been successfully done!!")
  }else{
    print("Vector type variable test has been failed!!")
  }
}

test6 <- function(){
  myvar2 <- c(NA)
  print("Testing NA type vector")
  result <- mytranspose(myvar2)
  testBoolean <- TRUE
  
  for(row in length(myvar2)){
    if(identical(result[row,1], myvar2[row])){
      testBoolean <- FALSE
    }
  }
  if(testBoolean){
    print("Vector containing NA test has been successfully done!!")
  }else{
    print("Vector containing Na test has been failed!!")
  }
}

test7 <- function(){
  myvar2 <- c()
  result <- mytranspose(myvar2)
  
  print("Testing NULL input")
  testBoolean <- identical(myvar2,result)
  
  if(testBoolean){
    print("Null input test has been successfully done!!")
  }else{
    print("Null input test has been failed!!")
  }
}

test8 <- function(){
  d <- c(1,2,3,4)
  e <- c("red","white","red",NA)
  f <- c(TRUE,TRUE,TRUE,FALSE)
  mydata3 <- data.frame(d,e,f)
  result <- mytranspose(mydata3)
  print("Testing Data Frame variable!!")
  testBoolean <- TRUE
  if(!identical(colnames(mydata3),rownames(result))){
    testBoolean <- FALSE
  }
  if(!identical(colnames(result),rownames(mydata3))){
    testBoolean <- FALSE
  }
  
  for(row in 1:nrow(mydata3)){
    for(col in 1:ncol(mydata3)){
      if(!identical(as.character(mydata3[row,col]),as.character(result[col,row]))){
        print("here?3")
        print(row)
        print(col)
        testBoolean <- FALSE
      }
    }
  }
  if(testBoolean){
    print("Data Frame test has been successfully done!!")
  }else{
    print("Data Frame test has been failed!!")
  }
}

test1()
test2()
test3()
test4()
test5()
test6()
test7()
test8()