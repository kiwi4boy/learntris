

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
	fun update_acc (r,c,e,acc) = update acc c (M-1-r) e
    in foldli update_acc returnInitial matrix
    end

end

val chg = DblStrVector.fromStringList


structure Game = struct
val score = ref 0
val lines = ref 0

fun printScore () = print (Int.toString(!score) ^ "\n")
fun printLines () = print (Int.toString(!lines) ^ "\n")
fun incrScore incr = score := !score + incr
fun incrLines incr = lines := !lines + incr

end


structure Tetramino = struct
val cur_tetra = ref (chg [""])
val cur_tetra_loc = ref (0,0)
end


structure Board = struct
val x_dim = 22
val y_dim = 10
val empty_row = Vector.tabulate (y_dim, (fn _ => "."))
val empty_arr = Vector.tabulate (x_dim, (fn _ => empty_row))
val arr = ref (empty_arr)

fun ask_given () =
    let fun ask_row () =
	    String.tokens Char.isSpace ((valOf o TextIO.inputLine) TextIO.stdIn)
    in arr := (Vector.map (fn _ => Vector.fromList (ask_row())) (!arr))
    end

fun empty () =
    arr := empty_arr

val line_is_full = Vector.all (fn str => str <> ".")

fun clear_line () =
    arr := (Vector.map (fn str => if line_is_full str
			    then (Game.incrScore 100;
				  Game.incrLines 1;
				  empty_row)
			    else str) (!arr))


datatype upperlowercase = Uppercase | Lowercase

fun place_block_in_arr block (x,y) ulcase =
    let val convert = case ulcase of Uppercase => Char.toUpper
				   | Lowercase => Char.toLower
	fun update_acc (r,c,e,acc) =
	    if e <> "."
	    then DblStrVector.update acc (r+x) (c+y) (String.map convert e)
	    else acc
    in DblStrVector.foldli update_acc (!arr) block
    end

fun stamp_block_on_arr block loc =
    arr := place_block_in_arr block loc Lowercase

fun print_mixed () =
    DblStrVector.print (place_block_in_arr (!(Tetramino.cur_tetra)) (!(Tetramino.cur_tetra_loc)) Uppercase)

fun print_arr () = DblStrVector.print (!arr)

end




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
	Clockwise => cur_tetra := DblStrVector.rotate (!cur_tetra)
      | CounterClockwise => cur_tetra := (DblStrVector.rotate o 
					  DblStrVector.rotate o 
					  DblStrVector.rotate) (!cur_tetra)

datatype direction = Down | Left | Right

fun move direction =
    let val (x2,y2) = 
	    case (direction, (!Tetramino.cur_tetra_loc)) of
		(Down , (x,y)) => (x+1,y)
	      | (Left , (x,y)) => (x,y-1)
	      | (Right , (x,y)) => (x,y+1)
	fun aux f acc e = 
	    if not acc then acc
	    else if e <> "." then f() else true
	fun newLoc_is_valid_f (r,c,e,acc) =
	    aux (fn () =>
		    r+x2 >= 0 andalso r+x2 < Board.x_dim andalso
		    c+y2 >= 0 andalso c+y2 < Board.y_dim) acc e
	fun newLoc_is_valid () =
	    DblStrVector.foldli newLoc_is_valid_f true (!Tetramino.cur_tetra)
	fun newLoc_do_not_collide_f (r,c,e,acc) =
	    aux (fn () => DblStrVector.sub (!(Board.arr)) (r+x2) (c+y2) = ".") acc e
	fun newLoc_do_not_collide () =
	    DblStrVector.foldli newLoc_do_not_collide_f true (!Tetramino.cur_tetra)
	fun is_valid_move () = 
	    newLoc_is_valid () andalso newLoc_do_not_collide ()
    in if is_valid_move ()
       then (Tetramino.cur_tetra_loc := (x2,y2))
       else (Tetramino.cur_tetra_loc := (!Tetramino.cur_tetra_loc))
    end

fun hard_drop () =
    let val list_of_down = List.tabulate (Board.x_dim, (fn _ => Down))
	fun aux l = case l of 
			[] => () 
		      | (x::xs) => (move x; aux xs)

    in (aux list_of_down; 
	Board.stamp_block_on_arr (!Tetramino.cur_tetra) (!Tetramino.cur_tetra_loc))
    end
end






fun launch_cmd cmds = 
    let fun bk f cmds = (f; launch_cmd cmds) in
	case cmds of
	    [] => ask_for_input ()
	  | (#"q"::_) => (OS.Process.exit OS.Process.success;
			  OS.Process.success)
	  | (#"p"::r) => bk (Board.print_arr()) r
	  | (#"P"::r) => bk (Board.print_mixed()) r
	  | (#"g"::r) => bk (Board.ask_given()) r
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
	  | (#"t"::r) => bk (DblStrVector.print (!Tetramino.cur_tetra)) r
	  | (#"s"::r) => bk (Board.clear_line()) r
	  | _ => check_tetra_gen cmds
    end

 and check_tetra_gen cmds =
    let fun assoc _ [] = launch_cmd (tl cmds)
	  | assoc (x:char) ((z:char,w,l)::xs') = 
	    if x = z then (Tetramino.cur_tetra := w;
			   Tetramino.cur_tetra_loc := l;
			   launch_cmd (tl cmds))
	    else assoc x xs'
    in assoc (hd cmds) Tetramino.tetra
    end 

and ask_for_input () =
    case TextIO.inputLine TextIO.stdIn of
	NONE => ask_for_input ()
      | SOME xs => launch_cmd 
		       (List.filter (not o Char.isSpace) 
				    (explode xs))
				 
fun main (prog_name: string, args: string list) =
    ask_for_input ()

val _ = SMLofNJ.exportFn ("xx", main)
