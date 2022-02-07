fun polyEval x = (3*x*x) + (6*x) + 12;

fun digitToInt x = if (ord x) >= 48
                   then
                       if (ord x) <= 57
                       then (ord x) - 48
                       else ~1
                   else ~1;


fun implies true true = true
|   implies true false = false
|   implies false true = true
|   implies  _ _ = true;  

fun constantOperation #"+" lhs rhs = lhs + rhs
|   constantOperation #"-" lhs rhs = lhs - rhs
|   constantOperation #"*" lhs rhs = lhs * rhs
|   constantOperation _ lhs rhs = ~1; 

val clampValue = fn (low:real) => fn(high: real) => fn(x: real) => if x < low
                            then low
                            else if x > high
                            then high
                            else x;