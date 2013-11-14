val x_dim = 22
val y_dim = 10
val empty_row = Vector.tabulate (y_dim, (fn _ => "."))
val arr = ref (Vector.tabulate (x_dim, (fn _ => empty_row)))
val score = ref 0
val lines = ref 0

fun chg lst = 
    Vector.fromList 
	(map (fn str => Vector.fromList(map Char.toString (explode(str))))
	     lst)

val cur_tetra = ref (chg [""])

val tetra = [
    (#"I" , chg([ "....",
		  "cccc",
		  "....",
		  "...." ]) ),

    (#"O" , chg([ "yy",
		  "yy" ]) ),

    (#"Z" , chg([ "rr.",
		  ".rr",
		  "..." ]) ),

    (#"S" , chg([ ".gg",
		  "gg.",
		  "..." ]) ),

    (#"J" , chg([ "b..",
		  "bbb",
		  "..." ]) ),

    (#"L" , chg([ "..o",
		  "ooo",
		  "..." ]) ),

    (#"T" , chg([ ".m.",
		  "mmm",
		  "..." ]) )
]

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
    let fun sub vec r c =
	    Vector.sub(Vector.sub(vec, r), c)
	fun update vec r c element =
	    let val vec_row_original = Vector.sub(vec, r)
		val vec_row_updated = Vector.update(vec_row_original, c, element)
	    in Vector.update(vec, r, vec_row_updated)
	    end
	val M = Vector.length matrix
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

fun main (prog_name: string, args: string list) =
    case TextIO.inputLine TextIO.stdIn of
	NONE => main (prog_name, args)
      | SOME x => 
	let fun process_cmd_lst cmds =
		case cmds of
		    [] => main(prog_name,args)
		  | (cmd::cmds') => (launch_cmd cmd cmds'; 
				     process_cmd_lst cmds')
	    and launch_cmd cmd cmds = 
		let fun bk f = (f; process_cmd_lst cmds) in
		    case Char.fromString cmd of
			SOME #"q" => (OS.Process.exit OS.Process.success;
				      OS.Process.success)
		      | SOME #"p" => bk (print_arr (!arr))
		      | SOME #"g" => bk (ask_given())
		      | SOME #"c" => bk (empty_arr())
		      | SOME #"?" => (case explode cmd of
					  (_::(#"s")::_) => bk (print_score())
					| (_::(#"n")::_) => bk (print_lines()) 
					| _ => process_cmd_lst(cmds))
		      | SOME #"(" => bk (cur_tetra := rotate_arr (rotate_arr   
					 (rotate_arr (!cur_tetra)))) 
		      | SOME #")" => bk (cur_tetra := rotate_arr (!cur_tetra))

		      | SOME #";" => bk (print "\n")
		      | SOME #"t" => bk (print_arr (!cur_tetra))
		      | SOME #"s" => bk (clear_line())
		      | _ => check_tetra_gen cmd cmds
		end
	    and check_tetra_gen cmd cmds =
		let fun bk f = (f; process_cmd_lst cmds) 
		    fun assoc x xs =
			case (x, xs) of
			    (NONE, _) => NONE
			  | (_, []) => NONE
			  | (SOME y, (z,w)::xs') => if y = z
						    then SOME w
						    else assoc x xs'
		in
		    case assoc (Char.fromString cmd) tetra of
			SOME y => bk (cur_tetra := y)
		      | NONE => process_cmd_lst cmds
		end
	in process_cmd_lst(String.tokens Char.isSpace x)
	end
		  

val _ = SMLofNJ.exportFn ("xx", main)
