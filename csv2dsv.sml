exception emptyInputFile
exception UnevenFields


fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val infile = TextIO.openIn infilename
		val field = ref ""
		val ans = ref ""
		val c = ref ""
		val line_number : int ref = ref 1
		val counter : int ref = ref 0
		val counter_1 : int ref = ref 0
		val flag : bool ref = ref true
		(*flag is for 1st line*)
		val flag_2: bool ref = ref true
		(*flag_2 is for checking empty file*)
		val delim2_flag: bool ref = ref false
		val double_quotes_flag: bool ref = ref true
		val outfile = TextIO.openOut outfilename
	in
		while(c := TextIO.inputN(infile,1); !c <> "") do
			if !flag then 
				if String.compare(!c, String.str(delim1)) = EQUAL then
					if !double_quotes_flag then
						if !delim2_flag then
							if "\"" = String.str(hd (explode (!field))) andalso "\"" = String.str(List.last (explode (!field))) then
								(flag_2 := false;counter := !counter + 1; c := String.str(delim2);ans := !ans ^ !field; ans  := !ans ^ !c; field := ""; delim2_flag := false)
							else
								(flag_2 := false;counter := !counter + 1; c := String.str(delim2);ans := !ans ^ "\"" ^ !field ^ "\""; ans  := !ans ^ !c; field := ""; delim2_flag := false)
						else
							(flag_2 := false;counter := !counter + 1; c := String.str(delim2);ans := !ans ^ !field; ans  := !ans ^ !c; field := "")
					else
						field := !field ^ !c
				else if String.compare(!c, String.str(#"\n")) = EQUAL then
					if !double_quotes_flag then
						if !delim2_flag then
							if "\"" = String.str(hd (explode (!field))) andalso "\"" = String.str(List.last (explode (!field))) then
								(flag_2 := false;flag := false;ans := !ans ^ !field; ans := !ans ^ !c; field := ""; delim2_flag := false)
							else
								(flag_2 := false;flag := false;ans := !ans ^ "\"" ^ !field ^ "\""; ans := !ans ^ !c; field := ""; delim2_flag := false)
						else
							(flag_2 := false;flag := false;ans := !ans ^ !field; ans := !ans ^ !c; field := "")
					else
						field := !field ^ !c
				else if String.compare(!c, String.str(delim2)) = EQUAL
				then (delim2_flag := true; field := !field ^ !c)
				else if String.compare(!c, "\"") = EQUAL
				then (double_quotes_flag := not (!double_quotes_flag); field := !field ^ !c)
				else field := !field ^ !c
			else 
				if String.compare(!c, String.str(delim1)) = EQUAL then
					if !double_quotes_flag then
						if !delim2_flag then
							if "\"" = String.str(hd (explode (!field))) andalso "\"" = String.str(List.last (explode (!field))) then
								(counter_1 := !counter_1 + 1; c := String.str(delim2);ans := !ans ^ !field;ans  := !ans ^ !c; field := ""; delim2_flag := false)
							else
								(counter_1 := !counter_1 + 1; c := String.str(delim2);ans := !ans ^ "\"" ^ !field ^ "\"";ans  := !ans ^ !c; field := ""; delim2_flag := false)
						else
							(counter_1 := !counter_1 + 1; c := String.str(delim2);ans := !ans ^ !field;ans  := !ans ^ !c; field := "")
					else
						field := !field ^ !c
				else if String.compare(!c, String.str(#"\n")) = EQUAL then
					if !double_quotes_flag then
						if !counter_1 > !counter then
							(print("Expected : " ^ Int.toString(!counter+1) ^ " fields, Present : " ^ Int.toString(!counter_1+1) ^ " fields on Line " ^ Int.toString( !line_number + 1)); raise UnevenFields)
						else if !counter_1 < !counter then
							(print("Expected : " ^ Int.toString(!counter+1) ^ " fields, Present : " ^ Int.toString(!counter_1+1) ^ " fields on Line " ^ Int.toString( !line_number + 1)) ;raise UnevenFields)
						else
							if !delim2_flag then
								if "\"" = String.str(hd (explode (!field))) andalso "\"" = String.str(List.last (explode (!field))) then
									(line_number := !line_number + 1; counter_1 := 0;ans := !ans ^ !field;ans  := !ans ^ !c; field := ""; delim2_flag := false)
								else
									(line_number := !line_number + 1; counter_1 := 0;ans := !ans ^ "\"" ^ !field ^ "\""; ans  := !ans ^ !c; field := ""; delim2_flag := false)
							else
								(line_number := !line_number + 1; counter_1 := 0;ans := !ans ^ !field;ans  := !ans ^ !c; field := "")
					else
						field := !field ^ !c
				else if String.compare(!c, String.str(delim2)) = EQUAL
				then (delim2_flag := true; field := !field ^ !c)
				else if String.compare(!c, "\"") = EQUAL
				then (double_quotes_flag := not (!double_quotes_flag); field := !field ^ !c)
				else field := !field ^ !c;

		if !flag_2 then
			raise emptyInputFile
		else
			();

		TextIO.output(outfile, !ans);
		TextIO.closeOut outfile;
		TextIO.closeIn infile
	end

fun csv2tsv(infilename, outfilename) =
	let
		val dustbin = convertDelimiters(infilename,#",",outfilename,#"\t");
	in
		1
	end
	

fun tsv2csv(infilename, outfilename) =
	let
		val dustbin = convertDelimiters(infilename,#"\t",outfilename,#",");
	in
		1
	end
