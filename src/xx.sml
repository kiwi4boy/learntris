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

fun rotate_cw_arr mat =
    let val M = Vector.length mat
	val N = Vector.length (Vector.sub (mat, 0))
	val ele = ref (Vector.sub(Vector.sub(mat, 0),0))
	val ret = ref (Vector.tabulate 
		      (M, (fn _ => Vector.tabulate (N, (fn _ => !ele)))))
	val ret_row = ref (Vector.sub (!ret, 0));
	val r = ref 0
	val c = ref 0
    in
	r := 0;
	while !r < M do (
	    c := 0;
	    while !c < N do (
		ele := Vector.sub(Vector.sub(mat, !r), !c);
		ret_row := Vector.sub(!ret, !c);
		ret_row := Vector.update(!ret_row, M-1-(!r), !ele);
		ret := Vector.update(!ret, !c, !ret_row);
		c := !c + 1
	    );
	    r := !r + 1
	);
	!ret
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
		      | SOME #"I" => bk (cur_tetra := #I tetra)
		      | SOME #"O" => bk (cur_tetra := #O tetra)
		      | SOME #"Z" => bk (cur_tetra := #Z tetra)
		      | SOME #"S" => bk (cur_tetra := #S tetra)
		      | SOME #"J" => bk (cur_tetra := #J tetra)
		      | SOME #"L" => bk (cur_tetra := #L tetra)
		      | SOME #"T" => bk (cur_tetra := #T tetra)
		      | SOME #")" => bk (cur_tetra := rotate_cw_arr(!cur_tetra)) 
		      | SOME #";" => bk (print "\n")
		      | SOME #"t" => bk (print_arr (!cur_tetra))
		      | SOME #"s" => bk (clear_line())
		      | _ => process_cmd_lst(cmds)
		end
	in process_cmd_lst(String.tokens Char.isSpace x)
	end
		  

val _ = SMLofNJ.exportFn ("xx", main)
