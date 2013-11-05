val x_dim = 22
val y_dim = 10
val empty_row = Array.array(y_dim, ".")
val arr = Array.array(x_dim, empty_row)
val score = ref 0
val lines = ref 0

val mke = Array.fromList

fun chg str =
    Array.fromList(map Char.toString (explode(str)))

val tetra = ref (mke([ chg "" ]))
val tetra_i = mke([ chg "....",
		    chg "cccc",
		    chg "....",
		    chg "...." ])

val tetra_o = mke([ chg "yy",
		    chg "yy" ])

val tetra_z = mke([ chg "rr.",
		    chg ".rr",
		    chg "..." ])

val tetra_s = mke([ chg ".gg",
		    chg "gg.",
		    chg "..." ])

val tetra_j = mke([ chg "b..",
		    chg "bbb",
		    chg "..." ])

val tetra_l = mke([ chg "..o",
		    chg "ooo",
		    chg "..." ])

val tetra_t = mke([ chg ".m.",
		    chg "mmm",
		    chg "..." ])

fun arr_sub array x y =
    Array.sub(Array.sub(array, x), y)

val sub = arr_sub arr

val print_arr = 
    Array.app (fn array => (Array.app (fn str => print (str ^ " ")) array;
			    print "\n"))

fun ask_given_row () =
    String.tokens Char.isSpace (valOf (TextIO.inputLine TextIO.stdIn))

fun ask_given () =
    Array.modify (fn _ => Array.fromList (ask_given_row())) arr

fun empty_arr () =
    Array.modify (fn _ => empty_row) arr

fun line_is_full (line) =
    Array.all (fn str => str <> ".") line

fun clear_line () =
    Array.modify (fn str => if line_is_full str
			    then (lines := !lines + 1;
				  score := !score + 100;
				  empty_row)
			    else str) arr
(*
    let
	fun aux n = if n >= Array.length(arr)
		    then ()
		    else (
			if line_is_full(Array.sub(arr,n))
			then (Array.update(arr,n,empty_row);
			      lines := !lines + 1;
			      score := !score + 100)
			else ();
			aux (n+1))
    in aux 0
    end *)

fun print_score () =
    print (Int.toString(!score)^"\n")

fun print_lines () =
    print (Int.toString(!lines)^"\n")

fun main (prog_name: string, args: string list) =
    case TextIO.inputLine TextIO.stdIn of
	NONE => main (prog_name, args)
      | SOME x => 
	let val commands = String.tokens Char.isSpace x
	    fun launch_cmd cmd cmds = 
		let fun bk f = (f; process_cmd_lst cmds) in
		    case Char.fromString cmd of
			SOME #"q" => ((OS.Process.exit OS.Process.success);
				      OS.Process.success)
		      | SOME #"p" => bk (print_arr arr)
		      | SOME #"g" => bk (ask_given())
		      | SOME #"c" => bk (empty_arr())
		      | SOME #"?" => (case explode cmd of
					  (_::(#"s")::_) => bk (print_score())
					| (_::(#"n")::_) => bk (print_lines()) 
					| _ => process_cmd_lst(cmds))
		      | SOME #"I" => bk (tetra := tetra_i)
		      | SOME #"O" => bk (tetra := tetra_o)
		      | SOME #"Z" => bk (tetra := tetra_z)
		      | SOME #"S" => bk (tetra := tetra_s)
		      | SOME #"J" => bk (tetra := tetra_j)
		      | SOME #"L" => bk (tetra := tetra_l)
		      | SOME #"T" => bk (tetra := tetra_t)
		      | SOME #"t" => bk (print_arr (!tetra))
		      | SOME #"s" => bk (clear_line())
		      | _ => process_cmd_lst(cmds)
		end
	    and process_cmd_lst cmds =
		case cmds of
		    [] => main(prog_name,args)
		  | (cmd::cmds') => (launch_cmd cmd cmds'; 
				     process_cmd_lst(cmds'))
	in process_cmd_lst(String.tokens Char.isSpace x)
	end
		  

val _ = SMLofNJ.exportFn ("xx", main)
