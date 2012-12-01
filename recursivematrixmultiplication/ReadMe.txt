This is plain simple recursive matrix multiplication using eight multiplication rather than seven ( http://en.wikipedia.org/wiki/Strassen_algorithm ) but it's not hard 
to convert it to Strassen. The real glitch is countering with laziness. proper data structure for matrix represenation. 
It's not very efficient but there are lot of chance for improvement and will be working on it. Currently it assumes that you will 
be providing the matrix which will be power of 2. See the test case for 4 X 4 matrices. 

*Main> tempMult ( [  [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] ] )  ( [  [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] , [ 1 , 2 , 3 , 4 ] ] )
[[10,20,30,40],[10,20,30,40],[10,20,30,40],[10,20,30,40]] 
