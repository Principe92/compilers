(* 1. Successfully define the following values in an SML file (6 points): *)
val _ = print "\nPart 1\n\n:";

(* a = 5 *)
val a = 5;

(* e = 2.71828 *)
val e = 2.71828;

(* b = the negation of the first item in this section *)
val b = ~a;

(* c = the character C *)
val c = #"C";

(* n = a string containing your name *)
val n = "Princewill Okorie";

(* f = the boolean value false *)
val f = false;

(* Successfully evaluate the following expressions in an SML le (7 points): *)
val _ = print "\n\nPrint 2\n\n:";

(* 5 + 7 *)
5+7;

(* 5 - 7 *)
5 - 7;

(* 5 * 3 *)
5*3;

(* 5 * 2.5 *)
5.0 * 2.5;

(* 13 / 4 (integer division, result is 3) *)
13 div 4;

(* 13 mod 4 (modulus, result is 1) *)
13 mod 4;

(* 13/4 (real division, result is 3.25) *)
13.0 / 4.0;


(* Accomplish the following in an SML le (4 points): *)
val _ = print "\n\nPrint 3\n\n:";

(* Decode the ASCII sequence 83 76 85 using the chr function *)
chr 83;
chr 76;
chr 85;

(* Convert Hello into ASCII by using the ord function *)
ord #"H";
ord #"e";
ord #"l";
ord #"l";
ord #"o";

(* Dene strings first and last that contain your first and last name,
* respectively, and concatenate those strings together with a space between for
* your full name *)
val first = "Princewill";
val last = "Okorie";
val fullname = first ^ " " ^ last;

(* Determine the length of the string from the last item with the size function *)
size fullname;

(*4. Successfully evaluate the following expressions to a boolean value in the SML
* interpreter (3 points): *)
val _ = print "\n\nPart 4:\n\n";

(* 5 is less than 10 *)
5 < 10;

(* -5 is less than -7 *)
~5 < ~7;

(* pie is less than 10 *)
3.14 <= 10.0;

(* 12 is equal to 12 *)
12 = 2;

(* 12 is not equal to 24 *)
12 <> 24;

(* character a is less than character A *)
#"a" < #"A";
