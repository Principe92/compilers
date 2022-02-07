fun cubed x = x*x*x;

fun rectPrism len width height = ((len*width*height), 2*(len*width +
  len*height + width*height));


fun isEven x = if x mod 2 = 0
               then true
               else false;


fun clampZero x = if x > 0
                  then x
                  else 0;

fun clamp x = if x < 0
              then 0
              else if x > 10
                   then 10
                   else x;


fun fact 1 = 1
  | fact x = x * fact (x-1);


fun sumBetween x y = if x = y
                     then x
                     else x + sumBetween (x+1 y);

fun sb x y = if x = y
             then x
             else sb(x y-1) + y;


fun makeInts 0 = nil
  | makeInts x = x::makeInts(x-1);


fun reverse nil = nil
  | reverse (x::xs) = reverse(xs)::[x];

