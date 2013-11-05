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

fun print_arr (array) =
    let
	fun print_arr_col (row,col) =
	    if col < Array.length(Array.sub(array,0))
	    then (print ((arr_sub array row col)^" ");
		  print_arr_col(row, col+1))
	    else ()
	fun print_arr_row (row) =
	    if row < Array.length(array)
	    then (print_arr_col(row,0);
		  print "\n";
		  print_arr_row(row+1))
	    else ()
    in
	print_arr_row (0)
    end

fun ask_given_row () =
    String.tokens Char.isSpace (valOf (TextIO.inputLine TextIO.stdIn))

fun ask_given () =
    let
	fun aux n = if n >= Array.length(arr)
		    then ()
		    else (Array.update(arr, n,
				      Array.fromList (ask_given_row()));
			  aux (n+1))
    in aux 0
    end

fun empty_arr () =
    let
	fun aux n = if n >= Array.length(arr)
		    then ()
		    else (Array.update(arr, n, empty_row);
			  aux (n+1))
    in aux 0
    end

fun line_is_full (line) =
    Array.all (fn str => str <> ".") line

fun clear_line () =
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
    end

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
		case Char.fromString cmd of
		    SOME #"q" => ((OS.Process.exit OS.Process.success);
				  OS.Process.success)
		  | SOME #"p" => (print_arr arr; 
				  process_cmd_lst cmds)
		  | SOME #"g" => (ask_given(); 
				  process_cmd_lst cmds)
		  | SOME #"c" => (empty_arr(); 
				  process_cmd_lst cmds)
		  | SOME #"?" => (case explode cmd of
				      (_::(#"s")::_) 
				      => (print_score(); 
					  process_cmd_lst(cmds))
				    | (_::(#"n")::_) 
				      => (print_lines(); 
					  process_cmd_lst(cmds))
				    | _ => process_cmd_lst(cmds))
		  | SOME #"I" => (tetra := tetra_i;
				  process_cmd_lst(cmds))
		  | SOME #"O" => (tetra := tetra_o;
				  process_cmd_lst(cmds))
		  | SOME #"Z" => (tetra := tetra_z;
				  process_cmd_lst(cmds))
		  | SOME #"S" => (tetra := tetra_s;
				  process_cmd_lst(cmds))
		  | SOME #"J" => (tetra := tetra_j;
				  process_cmd_lst(cmds))
		  | SOME #"L" => (tetra := tetra_l;
				  process_cmd_lst(cmds))
		  | SOME #"T" => (tetra := tetra_t;
				  process_cmd_lst(cmds))
		  | SOME #"t" => (print_arr (!tetra);
				  process_cmd_lst(cmds))
		  | SOME #"s" => (clear_line(); 
				  process_cmd_lst(cmds))
		  | _ => process_cmd_lst(cmds)
	    and process_cmd_lst cmds =
		case cmds of
		    [] => main(prog_name,args)
		  | (cmd::cmds') => (launch_cmd cmd cmds'; 
				     process_cmd_lst(cmds'))

	in
	    process_cmd_lst(String.tokens Char.isSpace x)
	end
		  

val _ = SMLofNJ.exportFn ("xx", main) 
