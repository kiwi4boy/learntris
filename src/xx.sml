val x_dim = 22
val y_dim = 10
val arr = Array.array(x_dim, Array.array(y_dim, "."))

fun arr_sub arr x y =
    Array.sub(Array.sub(arr, x), y)

val sub = arr_sub arr

(*fun print_char (character) =
    print ((Char.toString character)^" ") *)

fun print_arr (arr) =
    let
	fun print_arr_col (row,col) =
	    if col < Array.length(Array.sub(arr,0))
	    then (print ((sub row col)^" ");
		  print_arr_col(row, col+1))
	    else ()
	fun print_arr_row (row) =
	    if row < Array.length(arr)
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

fun main (prog_name: string, args: string list) =
    case TextIO.inputLine TextIO.stdIn of
	NONE => main (prog_name, args)
      | SOME x => case Char.fromString x of
		      SOME #"q" => OS.Process.success
		    | SOME #"p" => (print_arr arr;
				    main(prog_name, args))
		    | SOME #"g" => (ask_given();
				    main(prog_name, args)) 
		    | _ => main (prog_name, args)
			

val _ = SMLofNJ.exportFn ("xx", main) 
