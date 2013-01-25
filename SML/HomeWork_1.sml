
fun is_older ( ( d_1 : int * int * int ) , ( d_2 : int * int * int ) ) = 
    case ( d_1 , d_2 ) of 
      ( ( yr_1 , mn_1 , dy_1 ) , ( yr_2 , mn_2 , dy_2 ) ) =>
        if ( ( yr_1 < yr_2 ) orelse ( yr_1 = yr_2 andalso mn_1 < mn_2 ) orelse ( yr_1 = yr_2 andalso mn_1 =  mn_2 andalso dy_1 < dy_2 ) ) then true else false

fun number_in_month ( ( dates : ( int * int * int ) list ) , ( month : int ) ) = 
     case dates of 
	 [] => 0
      | ( y , m , d ) :: xs  => if m = month then 1 + number_in_month ( xs , month )
				 else number_in_month ( xs , month )  
								       
fun number_in_months ( ( dates : ( int * int * int ) list ) , ( months : int list ) ) = 
     case months  of 
	 [] => 0
      | m :: xs  => number_in_month ( dates , m ) + number_in_months ( dates , xs ) 


fun dates_in_month ( ( dates : ( int * int * int ) list ) , ( month : int ) ) = 
    case dates of 
	[] => []
      | ( y , m , d ) :: xs  =>
	  if m = month then ( y , m , d ) :: dates_in_month ( xs , month ) 
	   else dates_in_month ( xs , month ) 


fun dates_in_months ( ( dates : ( int * int * int ) list ) , ( months : int  list ) ) =
     case months of 
	 [] => []
      | m :: xs  => dates_in_month ( dates , m ) @ dates_in_months ( dates , xs )  

fun get_nth ( ( str : string list ) , ( n : int ) ) = let 
   fun  get_nth_helper ( ( x :: xs ) : string list ) ( n : int ) ( m : int ) = 
      case m = n of 
	true =>  x 
      | false => get_nth_helper xs n ( m + 1 ) 
    in get_nth_helper ( str : string list ) ( n : int ) ( 1 : int )  end 

fun date_to_string ( date : ( int * int * int ) ) = let 
    val x = [ "January", "February", "March", "April", "May", "June", "July", "August", 
	       "September", "October", "November", "December" ]
    in case date of 
        ( y , m , d ) => get_nth ( x , m ) ^ " " ^ Int.toString d ^ ", " ^ Int.toString y end

fun number_before_reaching_sum ( ( sum : int ) , ( lst : int list ) ) = let 
     fun number_helping ( sum : int ) ( ( x :: xs ) : int list ) ( acc : int ) ( cnt : int )  = 
           case  ( acc + x ) >= sum of 
	       true => cnt 
	     | false => number_helping sum xs ( acc + x ) ( cnt + 1 ) 
 in number_helping sum lst 0 0 end

fun what_month ( day : int )  = let 
      val x =  [ 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 ]
      val y =  [ 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 ]
      fun what_month_helper d ( x :: xs )  ( y :: ys )  = 
            case d <= x of 
               true  =>  y
	     | false  => what_month_helper ( d - x ) xs ys 
 in what_month_helper day x y end

fun month_range (   day_1 : int  ,  day_2 : int  ) = 
   case day_1 > day_2 of 
       true => []
     | false => what_month day_1 :: month_range (  day_1 + 1  , day_2 ) 

fun oldest ( day : ( int * int * int ) list ) = 
    case day of 
	[] => NONE
     | d  => SOME ( let 
       fun oldest_helper (  xs   : ( int * int * int ) list ) ( ( y_c , m_c , d_c ) : ( int * int * int ) ) = 
     case xs  of 
         [] => ( y_c , m_c , d_c )
      | ( ( y , m , d ) :: xs )  =>  if y < y_c orelse ( y = y_c andalso m < m_c ) orelse ( y = y_c andalso m = m_c andalso d < d_c ) then oldest_helper xs  ( y , m , d ) 
     else oldest_helper xs ( y_c , m_c , d_c ) 
     in oldest_helper  day ( hd day )   end  )  


