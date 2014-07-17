datatype p a =
  Get of (char -> p a)
| Look of (list char -> p a)
| Fail
| Result of a * p a
| Final of list (a * list char)

fun run [a] (p : p a) (s : list char) : list (a * list char) =
    case p of
	Get f => (case s of
		      [] => []
		    | c :: s' => run (f c) s')
      | Look f => run (f s) s
      | Result (x, q) => (x, s) :: run q s
      | Final r => r
      | _ => []

val mzero = @@Fail

fun mplus [a] (x : p a) (y : p a) : p a =
    case (x, y) of
	(Get f1, Get f2) => Get (fn c => mplus (f1 c) (f2 c))

      | (Result (x,p), q) => Result (x,mplus p q)
      | (p, Result (x,q)) => Result (x,mplus p q)

      | (Fail, p) => p
      | (p, Fail) => p

      | (Final r, Final t) => Final (List.append r t)
      | (Final r, Look f)  => Look (fn s => Final (List.append r (run (f s) s)))
      | (Final r, p)       => Look (fn s => Final (List.append r (run p s)))
      | (Look f, Final r)  => Look (fn s => Final (List.append (run (f s) s) r))
      | (p, Final r)       => Look (fn s => Final (List.append (run p s) r))

      | (Look f, Look g) => Look (fn s => mplus (f s) (g s))
      | (Look f, p)      => Look (fn s => mplus (f s) p)
      | (p, Look f)      => Look (fn s => mplus p (f s))

fun final [a] (rs : list (a * list char)) : p a =
    case rs of
	[] => Fail
      | r => Final r

fun concat [a] (l : list (list a)) : list a =
    List.foldr List.append [] l

val lmonad = mkMonad { Return = fn [a] (x : a) => x :: [],
		       Bind = fn [a] [b] x f => concat (List.mp f x) }

val pmonad = mkMonad { Return = let
			   fun return [a] (x : a) : p a = Result (x, Fail)
		       in
			   @@return
		       end,
		       Bind = let
			   fun bind [a] [b] (m : p a) (k : a -> p b) : p b =
			       case m of
				   Get f => Get (fn c => bind (f c) k)
				 | Look f => Look (fn s => bind (f s) k)
				 | Fail => Fail
				 | Result (x,f) => mplus (k x) (bind f k)
				 | Final r => final (xs <- r ; run (k xs.1) xs.2)
		       in
			   @@bind
		       end }


datatype readp a = R of (b ::: Type -> (a -> p b) -> p b)

val monad = mkMonad { Return = let
			  fun return [a] (x : a) : readp a = R (fn [b] k => k x)
		      in
			  @@return
		      end,
		      Bind = let
			  fun bind [a] [b] (m : readp a) (f : a -> readp b) : readp b =
			      R (fn [b'] k =>
				    case m of
					R m' => m' (fn  a => case f a of
								 R n => n k))
		      in
			  @@bind
		      end }

val get : readp char = R @@Get
val look : readp (list char) = R @@Look
val pfail : a ::: Type -> readp a = fn [a] => R (fn [b] f => Fail)
fun pplus [a] (r : readp a) (s : readp a) =
 case (r,s) of (R f1, R f2) => R (fn [b] (k : a -> p b) => mplus (f1 k) (f2 k))

fun satisfy (p : char -> bool) : readp char =
 c <- get ;
 if p c then return c else pfail
fun char c = satisfy (fn k => eq c k)
val eof : readp unit = s <- look ; if eq s [] then return () else pfail
fun string this : readp (list char) =
    let
	fun scan xs ys =
	    case (xs, ys) of
		([], _) => return this
	      | (x::xs, y::ys) => if eq x y then (_ <- get ; scan xs ys) else pfail
	      | (_, _) => pfail
    in
	s <- look ; scan this s
    end
fun munch p : readp (list char) =
    let
	fun scan s =
	    case s of
		(c::cs) => if p c then
			       _ <- get ; s <- scan cs ; return (c::s)
			   else return []
	      | _ => return []
    in
	s <- look ; scan s
    end
fun munch1 p =
    c <- get ;
    if p c then (s <- munch p ; return (c :: s))
    else pfail
fun choice [a] (ps : list (readp a)) : readp a = (* need this last annotation for inference *)
    case ps of
	[]      => pfail
      | p :: [] => p
      | p :: ps => pplus p (choice ps)
 

(*
skipSpaces : readp unit
skipSpaces =
     s <- look
     skip s
 where
  skip (c:s) | isSpace c =  _ <- get; skip s
  skip _                 = return unit

count : Int -> readp a -> readp (list a)
count n p = sequence (replicate n p)

between : readp open -> readp close -> readp a -> readp a
between open close p =   _ <- open
                          x <- p
                          _ <- close
                          return x
*)
fun option [a] (x : a) (p : readp a) : readp a = pplus p (return x)
and optional [a] (p : readp a) : readp unit = pplus (_ <- p ; return ()) (return ())
and many [a] (p : readp a) : readp (list a) = pplus (return []) (many1 p)
and many1 [a] (p : readp a) : readp (list a)= Monad.liftM2 (fn x xs => x :: xs) p (many p)
and skipMany [a] (p : readp a) : readp unit = (_ <- many p ; return ())
and skipMany1 [a] (p : readp a) : readp unit = (_ <- p ; skipMany p)
and sepBy [a] [sep] (p : readp a) (sep : readp sep) : readp (list a) = pplus (sepBy1 p sep) (return [])
and sepBy1 [a] [sep] (p : readp a) (sep : readp sep) : readp (list a) = Monad.liftM2 (fn x xs => x :: xs) p (many (_ <- sep ; p))
and endBy [a] [sep] (p : readp a) (sep : readp sep) : readp (list a) = many (x <- p ; _ <- sep ; return x)
and endBy1 [a] [sep] (p : readp a) (sep : readp sep) : readp (list a) = many1 (x <- p ; _ <- sep ; return x)

and chainr [a] (p : readp a) (op : readp (a -> a -> a)) (x : a) : readp a = pplus (chainr1 p op) (return x)
and chainl [a] (p : readp a) (op : readp (a -> a -> a)) (x : a) : readp a = pplus (chainl1 p op) (return x)
and chainr1 [a] (p : readp a) (op : readp (a -> a -> a)) : readp a =
    let
	fun rest x = pplus (f <- op ; y <- scan () ; return (f x y))
			   (return x)
	and scan () = bind p rest
    in
	scan ()
    end
and chainl1 [a] (p : readp a) (op : readp (a -> a -> a)) : readp a =
    let
	fun rest x = pplus (f <- op ; y <- p ; rest (f x y))
			   (return x)
    in
	x <- p ; rest x
    end
(*
and manyTill [a] [y] (p : readp a) (en : readp y) : readp (list a) =
    let
	fun scan () = leftpplus (_ <- en ; return [])
				(Monad.liftM2 (fn x xs => x :: xs) p scan)
    in
	scan ()
    end
*)

