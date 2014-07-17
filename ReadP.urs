(* Requires the List and Monad modules
 * so please add:
 * $/list
 * $/monad
 * to your .urp file
 *)

con p :: Type -> Type
datatype readp a = R of (b ::: Type -> (a -> p b) -> p b)

val monad : monad readp

val get : readp char
val look : readp (list char)
val pfail : a ::: Type -> readp a
val pplus : a ::: Type -> readp a -> readp a -> readp a

val satisfy : (char -> bool) -> readp char
val char : char -> readp char
val eof : readp unit
val string : list char -> readp (list char)
val munch : (char -> bool) -> readp (list char)
val munch1 : (char -> bool) -> readp (list char)
val choice : a ::: Type -> list (readp a) -> readp a
(*
val skipSpaces : readp unit
val count : a ::: Type -> int -> readp a -> readp (list a)
val between : x ::: Type -> y ::: Type -> a ::: Type ->
	      readp x -> readp y -> readp a -> readp a
*)
val option : a ::: Type -> a -> readp a -> readp a
val optional : a ::: Type -> readp a -> readp unit
val many : a ::: Type -> readp a -> readp (list a)
val many1 : a ::: Type -> readp a -> readp (list a)
val skipMany : a ::: Type -> readp a -> readp unit
val skipMany1 : a ::: Type -> readp a -> readp unit
val sepBy : a ::: Type -> sep ::: Type ->
	    readp a -> readp sep -> readp (list a)
val sepBy1 : a ::: Type -> sep ::: Type -> 
	     readp a -> readp sep -> readp (list a)
val endBy : a ::: Type -> sep ::: Type -> 
	    readp a -> readp sep -> readp (list a)
val endBy1 : a ::: Type -> sep ::: Type -> 
	     readp a -> readp sep -> readp (list a)
val chainr : a ::: Type -> 
	     readp a -> readp (a -> a -> a) -> a -> readp a
val chainl : a ::: Type -> 
	     readp a -> readp (a -> a -> a) -> a -> readp a
val chainr1 : a ::: Type -> 
	      readp a -> readp (a -> a -> a) -> readp a
val chainl1 : a ::: Type -> 
	      readp a -> readp (a -> a -> a) -> readp a
(*
val manyTill : a ::: Type -> y ::: Type -> 
	       readp a -> readp y -> readp (list a)
*)
