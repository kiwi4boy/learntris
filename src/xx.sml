

structure DblStrVector = struct

type dbl_str_vector = string vector vector

fun fromStringList (strlst: string list) : dbl_str_vector =
    let fun getInnerList str =
	    Vector.fromList (map Char.toString (explode (str)))
    in Vector.fromList (map getInnerList strlst)
    end

fun sub vec r c = Vector.sub(Vector.sub(vec, r), c)

fun update vec r c element =
    let val vecRowOriginal = Vector.sub(vec, r)
	val vecRowUpdated = Vector.update(vecRowOriginal, c, element)
    in Vector.update(vec, r, vecRowUpdated)
    end

val print = 
    Vector.app (fn array => (Vector.app (fn str => print (str ^ " ")) array;
			    print "\n"))

fun foldli f = 
    Vector.foldli
	(fn (r, row, returnAcc) =>
	    Vector.foldli
		(fn (c, element, returnAcc') =>
		    f (r,c,element,returnAcc'))
		returnAcc
		row)

fun rotate matrix =
    let val M = Vector.length matrix
	val N = (Vector.length o Vector.sub) (matrix, 0)
	val firstElement = sub matrix 0 0 
	val returnInitial = 
	    Vector.tabulate 
		(M, (fn _ => Vector.tabulate (N, (fn _ => firstElement))))
	fun updateAcc (r,c,e,acc) = update acc c (M-1-r) e
    in foldli updateAcc returnInitial matrix
    end

end (* DblStrVector *)

val chg = DblStrVector.fromStringList


structure Game = struct

val score = ref 0
val lines = ref 0

fun printScore () = print (Int.toString(!score) ^ "\n")
fun printLines () = print (Int.toString(!lines) ^ "\n")
fun incrScore incr = score := !score + incr
fun incrLines incr = lines := !lines + incr

end (* Game *)


structure Tetramino = struct

val curTetra = ref (chg [""])
val curTetraLoc = ref (0,0)

end (* Tetramino *)


structure Board = struct

val x_dim = 22
val y_dim = 10
val emptyRow = Vector.tabulate (y_dim, (fn _ => "."))
val emptyArr = Vector.tabulate (x_dim, (fn _ => emptyRow))
val arr = ref (emptyArr)

fun askGiven () =
    let fun askRow () =
	    String.tokens Char.isSpace ((valOf o TextIO.inputLine) TextIO.stdIn)
    in arr := (Vector.map (fn _ => Vector.fromList (askRow())) (!arr))
    end

fun empty () =
    arr := emptyArr

val lineIsFull = Vector.all (fn str => str <> ".")

fun clearLine () =
    arr := (Vector.map (fn str => if lineIsFull str
			    then (Game.incrScore 100;
				  Game.incrLines 1;
				  emptyRow)
			    else str) (!arr))


datatype upper_lowercase = Uppercase | Lowercase

fun placeBlockInArr ulCase (x,y) =
    let val convert = case ulCase of Uppercase => Char.toUpper
				   | Lowercase => Char.toLower
	fun updateAcc (r,c,e,acc) =
	    if e <> "."
	    then DblStrVector.update acc (r+x) (c+y) (String.map convert e)
	    else acc
    in DblStrVector.foldli updateAcc (!arr)
    end

fun stampBlockInArr block loc =
    arr := placeBlockInArr Lowercase loc block 

fun printMixed () =
    DblStrVector.print (placeBlockInArr 
			    Uppercase 
			    (!Tetramino.curTetraLoc) 
			    (!Tetramino.curTetra))

fun printArr () = DblStrVector.print (!arr)

end (* Board *)



structure Tetramino = struct
open Tetramino
val tetra = [
    (#"I" , chg([ "....",
		  "cccc",
		  "....",
		  "...." ]), (0,3) ),

    (#"O" , chg([ "yy",
		  "yy" ]), (0,4) ),

    (#"Z" , chg([ "rr.",
		  ".rr",
		  "..." ]), (0,3) ),

    (#"S" , chg([ ".gg",
		  "gg.",
		  "..." ]), (0,3) ),

    (#"J" , chg([ "b..",
		  "bbb",
		  "..." ]), (0,3) ),

    (#"L" , chg([ "..o",
		  "ooo",
		  "..." ]), (0,3) ),

    (#"T" , chg([ ".m.",
		  "mmm",
		  "..." ]), (0,3) )
]

datatype rotate_dir = Clockwise | CounterClockwise

fun rotate rotate_dir = 
    case rotate_dir of
	Clockwise => curTetra := DblStrVector.rotate (!curTetra)
      | CounterClockwise => curTetra := (DblStrVector.rotate o 
					  DblStrVector.rotate o 
					  DblStrVector.rotate) (!curTetra)

datatype direction = Down | Left | Right

fun move direction =
    let val (x2,y2) = 
	    case (direction, (!Tetramino.curTetraLoc)) of
		(Down , (x,y)) => (x+1,y)
	      | (Left , (x,y)) => (x,y-1)
	      | (Right , (x,y)) => (x,y+1)
	fun aux f acc e = 
	    if not acc then acc
	    else if e <> "." then f() else true
	fun checkNewLocBoundary (r,c,e,acc) =
	    aux (fn () =>
		    r+x2 >= 0 andalso r+x2 < Board.x_dim andalso
		    c+y2 >= 0 andalso c+y2 < Board.y_dim) acc e
	fun newLocIsValid () =
	    DblStrVector.foldli checkNewLocBoundary true (!Tetramino.curTetra)
	fun newLocIsEmpty (r,c,e,acc) =
	    aux (fn () => DblStrVector.sub (!(Board.arr)) (r+x2) (c+y2) = ".") acc e
	fun newLocDoNotCollide () =
	    DblStrVector.foldli newLocIsEmpty true (!Tetramino.curTetra)
	fun isValidMove () = 
	    newLocIsValid () andalso newLocDoNotCollide ()
    in if isValidMove ()
       then Tetramino.curTetraLoc := (x2,y2)
       else Tetramino.curTetraLoc := (!Tetramino.curTetraLoc)
    end

fun hard_drop () =
    let val downList = List.tabulate (Board.x_dim, (fn _ => Down))
	fun aux l = case l of 
			[] => () 
		      | (x::xs) => (move x; aux xs)

    in (aux downList; 
	Board.stampBlockInArr (!Tetramino.curTetra) (!Tetramino.curTetraLoc))
    end
end (* Tetramino *)




structure Shell = struct

fun launchCmd cmds = 
    let fun bk f r = (f; launchCmd r) in
	case cmds of
	    [] => askInput ()
	  | (#"q"::_) => (OS.Process.exit OS.Process.success;
			  OS.Process.success)
	  | (#"p"::r) => bk (Board.printArr()) r
	  | (#"P"::r) => bk (Board.printMixed()) r
	  | (#"g"::r) => bk (Board.askGiven()) r
	  | (#"c"::r) => bk (Board.empty()) r
	  | ((#"?")::(#"s")::r) => bk (Game.printScore()) r
	  | ((#"?")::(#"n")::r) => bk (Game.printLines()) r
	  | (#"("::r) => bk (Tetramino.rotate Tetramino.CounterClockwise) r
	  | (#")"::r) => bk (Tetramino.rotate Tetramino.Clockwise) r
	  | (#"<"::r) => bk (Tetramino.move Tetramino.Left) r
	  | (#">"::r) => bk (Tetramino.move Tetramino.Right) r
	  | (#"v"::r) => bk (Tetramino.move Tetramino.Down) r
	  | (#"V"::r) => bk (Tetramino.hard_drop()) r
	  | (#";"::r) => bk (print "\n") r
	  | (#"t"::r) => bk (DblStrVector.print (!Tetramino.curTetra)) r
	  | (#"s"::r) => bk (Board.clearLine()) r
	  | _ => generateTetra cmds
    end

 and generateTetra cmds =
    let fun assoc _ [] = launchCmd (tl cmds)
	  | assoc (x : char) ((z : char, w, l) :: xs') = 
	    if x = z then (Tetramino.curTetra := w;
			   Tetramino.curTetraLoc := l;
			   launchCmd (tl cmds))
	    else assoc x xs'
    in assoc (hd cmds) Tetramino.tetra
    end 

and askInput () =
    case TextIO.inputLine TextIO.stdIn of
	NONE => askInput ()
      | SOME xs => launchCmd 
		       (List.filter (not o Char.isSpace) 
				    (explode xs))
				 
fun main (progName: string, args: string list) = askInput ()

end

val _ = SMLofNJ.exportFn ("xx", Shell.main)
