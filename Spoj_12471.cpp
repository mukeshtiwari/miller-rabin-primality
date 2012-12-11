#include<cstdio>
#include<iostream>
#include<cstring>
using namespace std;

int memo[1100][1100] ;

int recurse( int h , int a , int cnt , bool flag ) 
    {
      if ( h <= 0 || a <= 0 ) return cnt ;
      if ( memo[h][a] ) return memo[h][a] ;
      if ( flag ) memo[h][a] = max ( memo[h][a] , recurse ( h + 3 , a + 2 , cnt + 1 , !flag ) ) ; 
      else 
         memo[h][a] = max ( memo[h][a] ,  max ( recurse ( h - 5 , a - 10 , cnt + 1 , !flag ) , recurse ( h - 20 , a + 5 , cnt + 1 , !flag ) ) ) ;
      
     return memo[h][a];
   }

int main()
  { 
    int n , a , b ;
    scanf( "%d", &n );
    for(int i = 0 ; i < n ; i++)
    { 
     memset ( memo , 0 , sizeof memo ) ;
     scanf("%d%d", &a , &b );
     printf("%d\n" , recurse( a , b , -1 ,  1 ));
     if( i != ( n - 1 ) ) printf("\n");
    }
  
  }
