This is plain simple recursive matrix multiplication using four multiplication ( http://en.wikipedia.org/wiki/Strassen_algorithm ). 
It's not very efficient but there are lot of chance for improvement and will be working on it. Currently it assumes that you will 
be providing the matrix which will be power of 2. See the test case for 4 X 4 matrices. 

*Main> tempMult ( [  [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] ] )  ( [  [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] ] )
[[10,20,30,40],[10,20,30,40],[10,20,30,40],[10,20,30,40]] 
