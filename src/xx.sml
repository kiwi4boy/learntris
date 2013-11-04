val y_dim = 10
val x_dim = 22


fun print_char (character) =
    print ((Char.toString character)^" ")

fun print_arr (arr: char array array) =
    let
	fun print_arr_col (row,col) =
	    if col < y_dim
	    then (print_char (Array.sub(
		       Array.sub(arr, row),
		       col));
		  print_arr_col(row, col+1))
	    else ()
	fun print_arr_row (row) =
	    if row < x_dim
	    then (print_arr_col(row,0);
		  print "\n";
		  print_arr_row(row+1))
	    else ()
    in
	print_arr_row (0)
    end


fun create_array () =
    let
	val arr = Array.array(x_dim, Array.array(y_dim, #"."))
    in
	print_arr arr
    end


fun main (prog_name: string, args: string list) =
    case TextIO.inputLine TextIO.stdIn of
	NONE => main (prog_name, args)
      | SOME x => case Char.fromString x of
		      SOME #"q" => OS.Process.success
		    | SOME #"p" => (create_array ();
				    main(prog_name, args))
		    | _ => main (prog_name, args)
			

(*val _ = SMLofNJ.exportFn ("xx", main)*)
