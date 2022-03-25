functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\126\000\002\000\126\000\007\000\126\000\009\000\126\000\
\\010\000\126\000\011\000\126\000\014\000\126\000\015\000\126\000\
\\016\000\126\000\018\000\126\000\026\000\126\000\030\000\126\000\
\\032\000\126\000\033\000\126\000\038\000\126\000\039\000\126\000\
\\040\000\126\000\043\000\126\000\044\000\126\000\045\000\126\000\000\000\
\\001\000\002\000\015\000\007\000\017\000\030\000\014\000\033\000\013\000\
\\038\000\031\000\043\000\011\000\044\000\010\000\045\000\009\000\000\000\
\\001\000\002\000\015\000\007\000\017\000\030\000\014\000\033\000\013\000\
\\039\000\069\000\043\000\011\000\044\000\010\000\045\000\009\000\000\000\
\\001\000\002\000\015\000\030\000\014\000\033\000\013\000\037\000\012\000\
\\043\000\011\000\044\000\010\000\045\000\009\000\000\000\
\\001\000\002\000\015\000\030\000\014\000\033\000\013\000\043\000\011\000\
\\044\000\010\000\045\000\009\000\000\000\
\\001\000\002\000\019\000\000\000\
\\001\000\002\000\020\000\000\000\
\\001\000\002\000\021\000\000\000\
\\001\000\002\000\023\000\000\000\
\\001\000\002\000\024\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\002\000\038\000\012\000\037\000\029\000\036\000\000\000\
\\001\000\002\000\045\000\003\000\044\000\004\000\043\000\008\000\042\000\
\\016\000\041\000\000\000\
\\001\000\002\000\049\000\042\000\048\000\000\000\
\\001\000\002\000\071\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\005\000\067\000\009\000\066\000\000\000\
\\001\000\005\000\067\000\013\000\072\000\000\000\
\\001\000\006\000\029\000\028\000\028\000\000\000\
\\001\000\006\000\068\000\000\000\
\\001\000\006\000\084\000\020\000\083\000\000\000\
\\001\000\008\000\030\000\000\000\
\\001\000\009\000\081\000\015\000\062\000\016\000\061\000\018\000\060\000\
\\020\000\059\000\026\000\058\000\000\000\
\\001\000\011\000\088\000\015\000\062\000\016\000\061\000\018\000\060\000\
\\020\000\059\000\026\000\058\000\000\000\
\\001\000\020\000\027\000\000\000\
\\001\000\020\000\091\000\000\000\
\\001\000\028\000\065\000\000\000\
\\001\000\031\000\033\000\000\000\
\\001\000\036\000\032\000\000\000\
\\001\000\040\000\053\000\000\000\
\\094\000\002\000\015\000\007\000\017\000\030\000\014\000\033\000\013\000\
\\043\000\011\000\044\000\010\000\045\000\009\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\032\000\070\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\015\000\062\000\016\000\061\000\018\000\060\000\020\000\059\000\
\\026\000\058\000\000\000\
\\109\000\015\000\062\000\016\000\061\000\018\000\060\000\020\000\059\000\
\\026\000\058\000\000\000\
\\110\000\015\000\062\000\016\000\061\000\018\000\060\000\020\000\059\000\
\\026\000\058\000\000\000\
\\111\000\015\000\062\000\016\000\061\000\018\000\060\000\020\000\059\000\
\\026\000\058\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\005\000\067\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\015\000\062\000\016\000\061\000\018\000\060\000\020\000\059\000\
\\026\000\058\000\000\000\
\\122\000\010\000\057\000\014\000\056\000\040\000\055\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\020\000\059\000\000\000\
\\127\000\018\000\060\000\020\000\059\000\026\000\058\000\000\000\
\\128\000\018\000\060\000\020\000\059\000\026\000\058\000\000\000\
\\129\000\020\000\059\000\026\000\058\000\000\000\
\\130\000\020\000\059\000\026\000\058\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\132\000\028\000\025\000\000\000\
\"
val actionRowNumbers =
"\004\000\044\000\042\000\040\000\
\\041\000\031\000\032\000\006\000\
\\007\000\008\000\004\000\009\000\
\\010\000\069\000\033\000\005\000\
\\025\000\068\000\019\000\022\000\
\\002\000\029\000\028\000\011\000\
\\034\000\012\000\013\000\006\000\
\\014\000\004\000\005\000\005\000\
\\036\000\043\000\030\000\014\000\
\\049\000\059\000\045\000\013\000\
\\013\000\061\000\060\000\055\000\
\\027\000\017\000\052\000\020\000\
\\003\000\037\000\038\000\015\000\
\\018\000\013\000\016\000\013\000\
\\013\000\013\000\013\000\013\000\
\\013\000\066\000\023\000\013\000\
\\021\000\014\000\006\000\035\000\
\\005\000\051\000\050\000\058\000\
\\056\000\024\000\062\000\001\000\
\\065\000\064\000\063\000\067\000\
\\046\000\013\000\006\000\054\000\
\\053\000\039\000\057\000\047\000\
\\026\000\013\000\048\000\000\000"
val gotoT =
"\
\\002\000\091\000\003\000\006\000\004\000\005\000\009\000\004\000\
\\010\000\003\000\011\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\014\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\013\000\016\000\000\000\
\\000\000\
\\000\000\
\\003\000\006\000\004\000\020\000\009\000\004\000\010\000\003\000\
\\011\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\024\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\014\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\033\000\000\000\
\\001\000\038\000\012\000\037\000\000\000\
\\013\000\044\000\000\000\
\\008\000\045\000\000\000\
\\003\000\006\000\004\000\048\000\009\000\004\000\010\000\003\000\
\\011\000\002\000\013\000\001\000\000\000\
\\003\000\049\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\003\000\050\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\052\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\061\000\012\000\037\000\000\000\
\\001\000\062\000\012\000\037\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\014\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\071\000\012\000\037\000\000\000\
\\000\000\
\\001\000\073\000\012\000\037\000\000\000\
\\001\000\074\000\012\000\037\000\000\000\
\\001\000\075\000\012\000\037\000\000\000\
\\001\000\076\000\012\000\037\000\000\000\
\\001\000\077\000\012\000\037\000\000\000\
\\001\000\078\000\012\000\037\000\000\000\
\\000\000\
\\000\000\
\\001\000\080\000\012\000\037\000\000\000\
\\000\000\
\\008\000\083\000\000\000\
\\013\000\084\000\000\000\
\\000\000\
\\003\000\085\000\009\000\004\000\010\000\003\000\011\000\002\000\
\\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\087\000\012\000\037\000\000\000\
\\013\000\088\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\090\000\012\000\037\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 92
val numrules = 39
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 32) => true | (T 33) => true | (T 34) => true | (T 40) => true
 | (T 36) => true | (T 37) => true | (T 38) => true | (T 42) => true
 | (T 43) => true | (T 44) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 35) => true | (T 39) => true
 | (T 41) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 31))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "ASSIGN"
  | (T 28) => "ARRAY"
  | (T 29) => "IF"
  | (T 30) => "THEN"
  | (T 31) => "ELSE"
  | (T 32) => "WHILE"
  | (T 33) => "FOR"
  | (T 34) => "TO"
  | (T 35) => "DO"
  | (T 36) => "LET"
  | (T 37) => "IN"
  | (T 38) => "END"
  | (T 39) => "OF"
  | (T 40) => "BREAK"
  | (T 41) => "NIL"
  | (T 42) => "FUNCTION"
  | (T 43) => "VAR"
  | (T 44) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID stmlist1, stmlist1left, 
stmlist1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  (stmlist as stmlist1) = stmlist1 ()
 in (print("program -> stmlist\n"))
end; ()))
 in ( LrTable.NT 1, ( result, stmlist1left, stmlist1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.ntVOID stm1, stm1left, stm1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
stm as stm1) = stm1 ()
 in (print("stmlist -> stm\n"))
end; ()))
 in ( LrTable.NT 3, ( result, stm1left, stm1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: ( _, ( 
MlyValue.ntVOID stmlist1, stmlist1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (stmlist as stmlist1) = 
stmlist1 ()
 val  (stm as stm1) = stm1 ()
 in (print("stmlist -> stmlist stm\n"))
end; ()))
 in ( LrTable.NT 3, ( result, stmlist1left, stm1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: ( _, ( 
MlyValue.ntVOID stmlist1, stmlist1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (stmlist as stmlist1) = 
stmlist1 ()
 val  (stm as stm1) = stm1 ()
 in (print("stmlist -> stmlist SEMICOLON stm\n"))
end; ()))
 in ( LrTable.NT 3, ( result, stmlist1left, stm1right), rest671)
end
|  ( 4, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID stmlist2,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID stmlist1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  (stmlist as stmlist1) = stmlist1 ()
 val  stmlist2 = stmlist2 ()
 in (print("stmlist -> LET stmlist IN stmlist END\n"))
end; ()))
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  ID2 = ID2 ()
 in (print("stm -> ID ASSIGN ae\n"))
end; ()))
 in ( LrTable.NT 2, ( result, ID1left, ID2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 val  (stm as stm1) = stm1 ()
 in (print("stm -> WHILE ID DO stm\n"))
end; ()))
 in ( LrTable.NT 2, ( result, WHILE1left, stm1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID stm1, _, stm1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1
 ()
 val  (stm as stm1) = stm1 ()
 in (print("stm -> IF ID THEN stm\n"))
end; ()))
 in ( LrTable.NT 2, ( result, IF1left, stm1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID stm2, _, stm2right)) :: _ :: ( _, ( 
MlyValue.ntVOID stm1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) ::
 ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (stm as stm1) = stm1 ()
 val  stm2 = stm2 ()
 in (print("stm -> IF ID THEN stm ELSE stm\n"))
end; ()))
 in ( LrTable.NT 2, ( result, IF1left, stm2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (vardec as vardec1) = vardec1 ()
 in (print("stm -> vardec\n"))
end; ()))
 in ( LrTable.NT 2, ( result, vardec1left, vardec1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID tydec1, tydec1left, tydec1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
tydec as tydec1) = tydec1 ()
 in (print("stm -> tydec\n"))
end; ()))
 in ( LrTable.NT 2, ( result, tydec1left, tydec1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (fundec as fundec1) = fundec1 ()
 in (print("stm -> fundec\n"))
end; ()))
 in ( LrTable.NT 2, ( result, fundec1left, fundec1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  tyid1 = 
tyid1 ()
 val  (ty as ty1) = ty1 ()
 in (print("tydec -> TYPE ID ASSIGN ty\n"))
end; ()))
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID tyid1, tyid1left, tyid1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
tyid as tyid1) = tyid1 ()
 in (print("tydec -> tyid\n"))
end; ()))
 in ( LrTable.NT 8, ( result, tyid1left, tyid1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1
 ()
 val  (exp as exp1) = exp1 ()
 in (print("vardec -> VAR ID ASSIGN exp\n"))
end; ()))
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (tyid as tyid1) = tyid1 ()
 val  (exp as exp1) = exp1 ()
 in (print("vardec -> VAR ID COLON tyid ASSIGN exp\n"))
end; ()))
 in ( LrTable.NT 9, ( result, VAR1left, exp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID tyfds1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _
, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (tyfds as tyfds1) = tyfds1 ()
 val  exp1 = exp1 ()
 in (print("fundec -> FUNCTION ID LPAREN tyfds RPAREN\n"))
end; ()))
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID tyid1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID 
tyfds1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (tyfds as tyfds1) = tyfds1 ()
 val  (tyid as tyid1) = tyid1 ()
 val  (exp as exp1) = exp1 ()
 in (
print("fundec -> FUNCTION ID LPAREN tyfds RPAREN COLON tyid EQ exp\n")
)
end; ()))
 in ( LrTable.NT 10, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 in (print("ty -> ID\n"))
end; ()))
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
tyfds1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (tyfds as tyfds1) = 
tyfds1 ()
 in (print("ty -> LBRACE tyfds RBRACE\n"))
end; ()))
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  (ID as ID1) = ID1 ()
 in (print("ty -> ARRAY OF ID\n"))
end; ()))
 in ( LrTable.NT 6, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 21, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (print("tyfds -> NIL\n")))
 in ( LrTable.NT 7, ( result, NIL1left, NIL1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID tyid1, _, tyid1right)) :: _ :: ( _, 
( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (tyid as tyid1) = tyid1 ()
 in (print("tyfds -> ID COLON tyid\n"))
end; ()))
 in ( LrTable.NT 7, ( result, ID1left, tyid1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID tyfds2, _, tyfds2right)) :: _ :: ( _
, ( MlyValue.ntVOID tyfds1, tyfds1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (tyfds as tyfds1) = 
tyfds1 ()
 val  tyfds2 = tyfds2 ()
 in (print("tyfds -> tyfds COMMA tyfds\n"))
end; ()))
 in ( LrTable.NT 7, ( result, tyfds1left, tyfds2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 in (print("lvalue -> ID\n"))
end; ()))
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = 
lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (print("lvalue -> lvalue DOT ID\n"))
end; ()))
 in ( LrTable.NT 11, ( result, lvalue1left, ID1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (print("lvalue -> lvalue LBRACK exp RBRACK\n"))
end; ()))
 in ( LrTable.NT 11, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  (lvalue as lvalue1) = 
lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (print("lvalue -> lvalue OF exp\n"))
end; ()))
 in ( LrTable.NT 11, ( result, lvalue1left, exp1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (lvalue as lvalue1) = lvalue1 ()
 in (print("exp -> lvalue\n"))
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (INT as 
INT1) = INT1 ()
 in (print("exp -> INT\n"))
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 (STRING as STRING1) = STRING1 ()
 in (print("exp -> STRING\n"))
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (print("exp -> exp AND exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (print("exp -> exp EQ exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (print("exp -> exp PLUS exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (print("exp -> exp MINUS exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 val  exp2 = exp2 ()
 in (print("exp -> exp TIMES exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  (exp as exp1) = exp1 ()
 in (print("exp -> MINUS exp\n"))
end; ()))
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  (exp as exp1) = exp1 ()
 in (print("exp -> LPAREN exp RPAREN\n"))
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 in (print("tyid -> ID\n"))
end; ()))
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
