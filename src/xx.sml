val x_dim = 22
val y_dim = 10
val empty_row = Array.array(y_dim, ".")
val arr = Array.array(x_dim, empty_row)
val score = ref 0
val lines = ref 0

fun chg lst = 
    Array.fromList 
	(map (fn str => Array.fromList(map Char.toString (explode(str))))
	     lst)



val cur_tetra = ref (chg [""])

val tetra = {
    I = chg([ "....",
	      "cccc",
	      "....",
	      "...." ]),

    O = chg([ "yy",
	      "yy" ]),

    Z = chg([ "rr.",
	      ".rr",
	      "..." ]),

    S = chg([ ".gg",
	      "gg.",
	      "..." ]),

    J = chg([ "b..",
	      "bbb",
	      "..." ]),

    L = chg([ "..o",
	      "ooo",
	      "..." ]),

    T = chg([ ".m.",
	      "mmm",
	      "..." ])
}

val print_arr = 
    Array.app (fn array => (Array.app (fn str => print (str ^ " ")) array;
			    print "\n"))

fun ask_given () =
    let fun ask_row () =
	    String.tokens Char.isSpace (valOf (TextIO.inputLine TextIO.stdIn))
    in Array.modify (fn _ => Array.fromList (ask_row())) arr
    end
	
fun empty_arr () =
    Array.modify (fn _ => empty_row) arr

val line_is_full =
    Array.all (fn str => str <> ".") 

fun clear_line () =
    Array.modify (fn str => if line_is_full str
			    then (lines := !lines + 1;
				  score := !score + 100;
				  empty_row)
			    else str) arr

fun print_score () =
    print (Int.toString(!score) ^ "\n")

fun print_lines () =
    print (Int.toString(!lines) ^ "\n")

fun rotate_cw_arr array =
    let val M = Array.length array
	val N = Array.length (Array.sub (array,0))
	val ele = ref (Array.sub(Array.sub(array, 0),0))
	val ret_row = ref (Array.array(0, !ele))
	val ret = Array.array (M, Array.array (N, !ele))
	val r = ref 0
	val c = ref 0
    in
	r := 0;
	while !r < M do (
	    c := 0;
	    while !c < N do (
		ele := Array.sub(Array.sub(array, !r), !c);
		ret_row := Array.sub(ret, !c);
		Array.update(!ret_row, M-1-(!r), !ele);
		Array.update(ret, !c, !ret_row);
		c := !c + 1
	    );
	    r := !r + 1
	);
	ret
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
		      | SOME #"p" => bk (print_arr arr)
		      | SOME #"g" => bk (ask_given())
		      | SOME #"c" => bk (empty_arr())
		      | SOME #"?" => (case explode cmd of
					  (_::(#"s")::_) => bk (print_score())
					| (_::(#"n")::_) => bk (print_lines()) 
					| _ => process_cmd_lst(cmds))
		      | SOME #"I" => bk (cur_tetra := #I tetra)
		      | SOME #"O" => bk (cur_tetra := #O tetra)
		      | SOME #"Z" => bk (cur_tetra := #Z tetra)
		      | SOME #"S" => bk (cur_tetra := #S tetra)
		      | SOME #"J" => bk (cur_tetra := #J tetra)
		      | SOME #"L" => bk (cur_tetra := #L tetra)
		      | SOME #"T" => bk (cur_tetra := #T tetra)
		      | SOME #")" => bk (cur_tetra := rotate_cw_arr(!cur_tetra))
		      | SOME #"t" => bk (print_arr (!cur_tetra))
		      | SOME #"s" => bk (clear_line())
		      | _ => process_cmd_lst(cmds)
		end
	in process_cmd_lst(String.tokens Char.isSpace x)
	end
		  

(* val _ = SMLofNJ.exportFn ("xx", main) *)
