val x_dim = 22
val y_dim = 10
val empty_row = Vector.tabulate (y_dim, (fn _ => "."))
val empty_arr = Vector.tabulate (x_dim, (fn _ => empty_row))
val arr = ref (empty_arr)
val score = ref 0
val lines = ref 0

fun chg lst = 
    Vector.fromList 
	(map (fn str => Vector.fromList(map Char.toString (explode(str))))
	     lst)

val cur_tetra = ref (chg [""])
val cur_tetra_loc = ref (0,0)

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

fun sub vec r c =
	    Vector.sub(Vector.sub(vec, r), c)

fun update vec r c element =
    let val vec_row_original = Vector.sub(vec, r)
	val vec_row_updated = Vector.update(vec_row_original, c, element)
    in Vector.update(vec, r, vec_row_updated)
    end


val print_arr = 
    Vector.app (fn array => (Vector.app (fn str => print (str ^ " ")) array;
			    print "\n"))



fun ask_given () =
    let fun ask_row () =
	    String.tokens Char.isSpace (valOf (TextIO.inputLine TextIO.stdIn))
    in arr := (Vector.map (fn _ => Vector.fromList (ask_row())) (!arr))
    end
	
fun empty_arr () =
    arr := (Vector.map (fn _ => empty_row) (!arr))

val line_is_full =
    Vector.all (fn str => str <> ".") 

fun clear_line () =
    arr := (Vector.map (fn str => if line_is_full str
			    then (lines := !lines + 1;
				  score := !score + 100;
				  empty_row)
			    else str) (!arr))

fun print_score () =
    print (Int.toString(!score) ^ "\n")

fun print_lines () =
    print (Int.toString(!lines) ^ "\n")

fun double_foldli f initial target = 
    Vector.foldli
	(fn (r, row, returnAcc) =>
	    Vector.foldli
		(fn (c, element, returnAcc') =>
		    f (r,c,element,returnAcc'))
		returnAcc
		row)
	initial
	target

fun rotate_arr matrix =
    let val M = Vector.length matrix
	val N = Vector.length (Vector.sub (matrix, 0))
	val firstElement = sub matrix 0 0 
	val returnInitial = 
	    Vector.tabulate 
		(M, (fn _ => Vector.tabulate (N, (fn _ => firstElement))))
	fun update_acc (r,c,e,acc) = update acc c (M-1-r) e
    in double_foldli update_acc returnInitial matrix
    end

fun place_block_in_arr block (x,y) =
    let fun update_acc (r,c,e,acc) =
	    if e <> "."
	    then update acc (r+x) (c+y) (String.map Char.toUpper e)
	    else acc
    in double_foldli update_acc (!arr) block
    end

datatype direction = Down | Left | Right

fun move direction =
    let val new_loc = 
	    case (direction, (!cur_tetra_loc)) of
		(Down, (x,y)) => (x+1,y)
	      | (Left, (x,y)) => (x,y-1)
	      | (Right, (x,y)) => (x,y+1)
	fun aux f acc e = 
	    if not acc
	    then acc
	    else if e <> "."
	    then f ()
	    else true
	fun newLoc_is_valid_f (r,c,e,acc) =
	    case new_loc of
		(x,y) => aux (fn () =>
				 r+x >= 0 andalso r+x < x_dim andalso
				 c+y >= 0 andalso c+y < y_dim) acc e
	fun newLoc_is_valid () =
	    double_foldli newLoc_is_valid_f true (!cur_tetra)
	fun newLoc_do_not_collide_f (r,c,e,acc) =
	    case new_loc of
		(x,y) => aux (fn () =>
				 sub (!arr) (r+x) (c+y) = ".") acc e
	fun newLoc_do_not_collide () =
	    double_foldli newLoc_do_not_collide_f true (!cur_tetra)
	fun is_valid_move () = 
	    newLoc_is_valid () andalso newLoc_do_not_collide ()
    in if is_valid_move ()
       then (cur_tetra_loc := new_loc)
       else (cur_tetra_loc := (!cur_tetra_loc))
    end

fun print_mixed () =
    print_arr (place_block_in_arr (!cur_tetra) (!cur_tetra_loc))

fun launch_cmd cmds = 
    let fun bk f cmds = (f; launch_cmd cmds) in
	case cmds of
	    [] => ask_for_input ()
	  | (#"q"::_) => (OS.Process.exit OS.Process.success;
			  OS.Process.success)
	  | (#"p"::r) => bk (print_arr (!arr)) r
	  | (#"P"::r) => bk (print_mixed ()) r
	  | (#"g"::r) => bk (ask_given()) r
	  | (#"c"::r) => bk (empty_arr()) r
	  | ((#"?")::(#"s")::r) => bk (print_score()) r
	  | ((#"?")::(#"n")::r) => bk (print_lines()) r
	  | (#"("::r) => bk (cur_tetra := 
			     rotate_arr 
				 (rotate_arr   
				      (rotate_arr (!cur_tetra)))) r
	  | (#")"::r) => bk (cur_tetra := rotate_arr (!cur_tetra)) r
	  | (#"<"::r) => bk (move Left) r
	  | (#">"::r) => bk (move Right) r
	  | (#"v"::r) => bk (move Down) r
	  | (#";"::r) => bk (print "\n") r
	  | (#"t"::r) => bk (print_arr (!cur_tetra)) r
	  | (#"s"::r) => bk (clear_line()) r
	  | _ => check_tetra_gen cmds
    end

and check_tetra_gen cmds =
    let fun assoc _ [] = launch_cmd (tl cmds)
	  | assoc x ((z,w,l)::xs') = if x = z
				 then (cur_tetra := w;
				       cur_tetra_loc := l;
				       launch_cmd (tl cmds))
				 else assoc x xs'
    in assoc (hd cmds) tetra
    end 

and ask_for_input () =
    case TextIO.inputLine TextIO.stdIn of
	NONE => ask_for_input ()
      | SOME xs => launch_cmd (
		      List.filter (not o Char.isSpace) (explode xs))
				 
fun main (prog_name: string, args: string list) =
    ask_for_input ()

val _ = SMLofNJ.exportFn ("xx", main)
