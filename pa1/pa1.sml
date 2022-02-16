type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

    and exp = IdExp of id
            | NumExp of int
            | OpExp of exp * binop * exp
            | EseqExp of stm * exp

val getId = fn(var_name:id, value) => var_name;

val getValue = fn(var_name:id, value) => value;

(* Function to update symbol table*)
val update = fn
                (nil, var_name:id, value) => (var_name, value)::[]
             |  (tb, var_name:id, value) => (var_name, value)::tb;

(* Function to look up symbol*)
fun lookup (nil, var_name:id) = ~9999999999
|   lookup (x::tb, var_name:id) = if getId x = var_name
                                  then getValue x
                                  else lookup(tb, var_name);

(* Function to perform binary operation*)
val doBinaryOp = fn
                    (x:int, Plus, y:int) => x + y
                 |  (x:int, Minus, y:int) => x - y
                 |  (x:int, Times, y:int) => x * y
                 |  (x:int, Div, y:int) => x div y;

fun InterpStm (CompoundStm(st1:stm, st2:stm), tb) = let
                                                        val new_t = InterpStm(st1, tb)
                                                    in
                                                        InterpStm(st2, new_t)
                                                    end

|   InterpStm (AssignStm(var_name:id, e:exp), tb) = let
                                                        val (res, new_t) = InterpExp(e, tb)
                                                    in
                                                        update(new_t, var_name, res)
                                                    end
                
|   InterpStm (PrintStm(ls), tb) =   let 
                                            val _ = doPrint(ls, tb)
                                        in
                                            (print "\n"; tb)
                                        end

and InterpExp (IdExp(var_name:id), tb) =                    (lookup(tb, var_name), tb)
    | InterpExp (NumExp(value:int), tb) =                   (value, tb)
    | InterpExp (OpExp(e1:exp, oper:binop, e2:exp), tb) =   let
                                                                val (left, new_tl) = InterpExp(e1, tb)
                                                                val (right, new_tr) = InterpExp(e2, new_tl)
                                                            in
                                                                (doBinaryOp(left, oper, right), new_tr)
                                                            end

    | InterpExp (EseqExp(st:stm, e:exp), tb) =  let
                                                    val new_t = InterpStm(st, tb)
                                                in
                                                    InterpExp(e, new_t)
                                                end

and doPrint (nil, tb) = print ""
    | doPrint(e::ls, tb) =  let 
                                val (res, new_t) = InterpExp(e, tb)
                            in
                                (print (Int.toString(res) ^ " "); doPrint(ls, new_t))
                            end;

(* Main Function to be called by user *)
val interp = fn(x:stm) => InterpStm(x, []);

(* Test *)
val prog = 
CompoundStm(
    AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)), 
    CompoundStm(
        AssignStm("b", EseqExp(PrintStm [IdExp "a",OpExp(IdExp "a", Minus,NumExp 1)], OpExp(NumExp 10, Times, IdExp "a"))),
        PrintStm [IdExp "b"]
    )
);

interp prog;