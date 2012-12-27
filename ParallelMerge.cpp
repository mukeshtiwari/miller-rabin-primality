#include<cstdio>
#include<pthread.h>
#include<algorithm>
using namespace std;

int  T[20000] , M[20000];

struct data 
  { 
    int p_1 , r_1 , p_2 , r_2 , pos ;
  };
int bsearch( int value , int left, int right )
{
    long low  = left;
    long high = max( left, right + 1 );
    while( low < high )
    {
        long mid = ( low + high ) / 2;
        if ( value <= T[ mid ] ) high = mid;
        else low  = mid + 1; 
    }
    return high ;
}

void* pMerge ( void *args ) 
   {

          pthread_t tid[2];
          printf("id = %li\n" ,( unsigned long int )  pthread_self() );
          struct data* t =  ( struct data* ) ( args ) ;  
          int p_1 = t->p_1 , r_1 = t->r_1 , p_2 = t->p_2 , r_2 = t->r_2  , pos = t->pos ;
          int n_1 = r_1 - p_1 + 1 , n_2 = r_2 - p_2 + 1 ;
          if ( n_1 < n_2 ) 
          {
            swap ( p_1 , p_2 ) ;
            swap ( r_1 , r_2 ) ;
            swap ( n_1 , n_2 ) ;
          }
          if ( n_1 <=  0 ) return NULL ; 
          int q_1 = ( p_1 + r_1 ) >> 1 ;
          int q_2 = /*upper_bound ( &T[p_2] , &T[r_2] , T[q_1] ) - &T[p_2] ; */ bsearch ( T[q_1]  , p_2 , r_2 )  ;
          int q_3 = pos + ( q_1 - p_1 ) + ( q_2 - p_2 ) ;
          //printf("p_1 = %d r_1 = %d p_2 = %d r_2 = %d pos = %d  q_1 = %d q_2 = %d\n" , p_1 , r_2 , p_2 , r_2 , pos , q_1 , q_2 );
          M[q_3] = T[q_1] ;
          struct data m , n ;
          m.p_1 = p_1 , m.r_1 = q_1 - 1 , m.p_2 = p_2 , m.r_2 = q_2 - 1 ,  m.pos = pos ; 
          n.p_1 = q_1 + 1 , n.r_1 = r_1 , n.p_2 = q_2 , n.r_2 = r_2 , n.pos = q_3 + 1 ;
          pthread_create( &tid[0] , NULL , pMerge , &m );
          pthread_create( &tid[1] , NULL , pMerge , &n ); 
          pthread_join ( tid [ 0 ]  , NULL );
          pthread_join ( tid [ 1 ]  , NULL );
          return NULL; 
   } 


int main()
  {
        int m , n ;
        scanf("%d%d",&m , &n ) ;
        for(int i = 0 ; i < m ; i++ ) T[i] = ( int ) random() % 10000 ;
        for( int i = 0 ; i < n ; i++ ) T[i+m] = ( int ) random() % 10000 ;
        sort( &T[0], &T[m] );
        sort( &T[m] , &T[m+n] );
        pthread_t tid ;
        struct data d; 
        d.p_1 = 0 , d.r_1 = m-1 , d.p_2 = m , d.r_2 = m+n-1 , d.pos = 0 ; 
        pthread_create ( &tid , NULL , pMerge , &d );
        pthread_join ( tid , NULL );
        for( int i = 0 ; i < m+n ; i++ ) printf("%d " , M[i]);
        printf("\n" );
  }
