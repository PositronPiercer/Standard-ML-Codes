
local 
fun reverse []=[] |
		reverse (h::t)=reverse(t) @ [h]

fun length []=0 |
	length (h::t)= 1+length(t)

fun defect(text,0)=
	if Char.isDigit(String.sub(text,0)) then false
	else true |
	defect(text,n)=
	if n= ~1 then true
	else
	if Char.isDigit(String.sub(text,n)) then defect(text,n-1)
	else true

fun cut_leading_zeros int_list=
    if (hd int_list = 0   andalso length(int_list)>1) then cut_leading_zeros(tl int_list)
    else int_list

fun greater_than([x],[y])=
	if x>y then true
	else false |
	greater_than(x,y)=
	let
	  val length_x=length(x)
	  val length_y=length(y)
	in
	  if length_x=0 then false
	  else if length_y = 0 then true
	  else
	  if hd x =0 then greater_than(tl x,y)
	  else if hd y =0 then greater_than(x,tl y)
	  else
	  if length_x>length_y then
	  	true
	  else if length_y>length_x then
	  	false
	  else 
	  	if hd x>hd y then true
		else if hd x<hd y then false
		else greater_than(tl x, tl y)
	end

fun pad_front(x,0)=x |
    pad_front(x,n)=
        0::pad_front(x,n-1)

fun pad_end(x,0)=x |
    pad_end(x,n)=
        pad_end(x,n-1) @ [0]

fun split(n,x)= 
	if n=0 then []
	else
	(hd x)::split(n-1,tl x)

fun com_nine []=[] | (*Calculating 9s complement*)
    com_nine (h::t)= 
        (9-h)::com_nine(t)

fun to_int_list (text,n,text_size)=
    if n = text_size then
    []
    else
    (Char.ord(String.sub(text,n)) - Char.ord( #"0" ))::to_int_list(text,n+1,text_size)

fun to_string []="" |
    to_string (h::t)= 
        Int.toString(h) ^ to_string(t)

fun add_compute ([],[],carry)= (* Input : Both lists of equal length *)
		if carry=0 then []
		else
		[carry] |
		add_compute ((h1::t1),(h2::t2),carry)=
				let
					val sum=(h1+h2+carry) mod 10;
					val car=(h1+h2+carry) div 10;
				in
					sum::add_compute(t1,t2,car)
				end

fun add(x,y)=
    let
      val length_x=length(x)
      val length_y=length(y)
    in
      if length_x=length_y then
        reverse(add_compute(reverse(x),reverse(y),0))
      else 
      if length_x>length_y then
        reverse(add_compute(reverse(x),reverse(pad_front(y,length_x-length_y)),0))
      else
        reverse(add_compute(reverse(pad_front(x,length_y-length_x)),reverse(y),0))
    end 
			 
fun com_10 x=add(com_nine x,[1])
fun subtract_compute(x,y)= (*x>y*)
	let
	  val res=add(x,com_10(y))
	in
	  if(length(res)>length(x)) then
	  	tl res
	  else
	  	res
	end

fun subtract(x,y)=
	let
	  val x_greaterThan_y=greater_than(x,y)
	  val length_x=length(x)
	  val length_y=length(y)
	in
	if x_greaterThan_y then
		if length_x>length_y then
			(1,subtract_compute(x, pad_front(y,length_x-length_y)))
		else
			(1,subtract_compute(pad_front(x,length_y-length_x), y ))
	else
		if length_y>length_x then
			(~1,subtract_compute( y, pad_front(x,length_y-length_x )))
		else
			(~1,subtract_compute(pad_front(y,length_x-length_y), x ))
	end







fun karat_compute([x],[y])=
	let
	  val prod=x*y
	in
	  if prod>9 then
	  	(prod div 10)::[(prod mod 10)]
		  else
		  [prod]
	end
	|
	karat_compute((h1::t1),(h2::t2))= 
		let
		  val length_x=length((h1::t1))
		  val length_y=length((h2::t2))
		  val m=(length_x+1) div 2
		  val x1=split(length_x-m,(h1::t1))
		  val x0=reverse(split(m,reverse((h1::t1))))
		  val y1=split(length_y-m,(h2::t2))
		  val y0=reverse(split(m,reverse((h2::t2))))
		  val z2=karat_compute(x1,y1)
		  val z0=karat_compute(x0,y0)
		  val (sign1,sub1)=subtract(x0,x1)
		  val (sign2,sub2)=subtract(y1,y0)
		  val sub_sum=add(z0,z2)
		  val final_sign=sign1*sign2
		  val d=subtract(sub_sum,karat_compute( sub1 , sub2 ))
		in

			
			if final_sign = ~1 then
				let
				  val (c,v)=subtract(sub_sum,karat_compute( sub1 , sub2 ))
				in
				  ( 
				add (add ( pad_end( z2, 2*m ) , pad_end( v,m) ) ,z0))
				end
				
				
			else
			

				add(add(pad_end(z2,2*m) , pad_end(add(sub_sum,karat_compute(sub1,sub2)),m)) , z0) 
				
		  
		end  

	
exception NotUnsignedIntegers

in

fun karatsuba(x,y) = 
		if defect(x,String.size(x)-1) orelse defect(y,String.size(y)-1) then raise NotUnsignedIntegers
		else
		let
		  val xx=to_int_list(x,0,String.size(x))
		  val yy=to_int_list(y,0,String.size(y))
		  val length_xx=length(xx)
		  val length_yy=length(yy)
		in
		  if length_xx>length_yy then
			to_string(cut_leading_zeros(karat_compute(xx , pad_front(yy,length_xx-length_yy))))
		  else if length_xx<length_yy then
		    to_string(cut_leading_zeros(karat_compute(pad_front(xx,length_yy-length_xx) , yy)))
		  else to_string(cut_leading_zeros(karat_compute(xx, yy)))
		end
	  	  

end