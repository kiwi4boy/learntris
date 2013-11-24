val x_dim = 22
val y_dim = 10
val empty_row = Vector.tabulate (y_dim, (fn _ => "."))
val empty_arr = Vector.tabulate (x_dim (fn _ => empty_row))
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

fun rotate_arr matrix =
    let val M = Vector.length matrix
	val N = Vector.length (Vector.sub (matrix, 0))
	val firstElement = sub matrix 0 0 
	val returnInitial = Vector.tabulate 
		      (M, (fn _ => Vector.tabulate (N, (fn _ => firstElement))))
    in Vector.foldli 
	   (fn (r, matrixRow, returnAcc) =>   
	       Vector.foldli 
		   (fn (c, element, returnAcc') =>
		       update returnAcc' c (M-1-r) element)
		   returnAcc
		   matrixRow)
	   returnInitial
	   matrix 
    end  

(* datatype direction = Down | Left | Right

fun move direction =
    Ve   *)
    

fun place_block_in_arr block (x,y) =
    Vector.foldli
	(fn (r, blockRow, returnAcc) =>
	    Vector.foldli
		(fn (c, element, returnAcc') =>
		    if element <> "."
		    then update returnAcc' (r+x) (c+y) 
				(String.map Char.toUpper element)
		    else returnAcc')
		returnAcc
		blockRow)
	(!arr)
	block

fun launch_cmd cmds = 
    let fun bk f cmds = (f; launch_cmd cmds) in
	case cmds of
	    [] => ask_for_input ()
	  | (#"q"::_) => (OS.Process.exit OS.Process.success;
			  OS.Process.success)
	  | (#"p"::r) => bk (print_arr (!arr)) r
	  | (#"P"::r) => bk (print_arr (!arr)) r
	  | (#"g"::r) => bk (ask_given()) r
	  | (#"c"::r) => bk (empty_arr()) r
	  | ((#"?")::(#"s")::r) => bk (print_score()) r
	  | ((#"?")::(#"n")::r) => bk (print_lines()) r
	  | (#"("::r) => bk (cur_tetra := 
			     rotate_arr 
				 (rotate_arr   
				      (rotate_arr (!cur_tetra)))) r
	  | (#")"::r) => bk (cur_tetra := rotate_arr (!cur_tetra)) r
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
				       arr := place_block_in_arr w l;
				       launch_cmd (tl cmds))
				 else assoc x xs'
    in
	assoc (hd cmds) tetra
    end 

and ask_for_input () =
    case TextIO.inputLine TextIO.stdIn of
	NONE => ask_for_input ()
      | SOME xs => launch_cmd (
		      List.filter (not o Char.isSpace) (explode xs))
				 
fun main (prog_name: string, args: string list) =
    ask_for_input ()

val _ = SMLofNJ.exportFn ("xx", main)
