(* Function to take a string and parse it into a list of numeric digits *)
let convert_line_to_digits line =

    (* Tuples to map string representation to ints *)
    let strings_to_ints = [
        ("one", 1);
        ("two", 2);
        ("three", 3);
        ("four", 4);
        ("five", 5);
        ("six", 6);
        ("seven", 7);
        ("eight", 8);
        ("nine", 9);
        ("zero", 0);
    ] in
    
    (* A recurssive function that iterates over a line 
       to find start index for each occurence of the substring *)
    let find_start_indices_of_substring line substring =
        let substring_length = String.length substring in
        let rec find_indices start_index =
            if start_index > String.length line - substring_length then
                []
            else if String.sub line start_index substring_length = substring then
                start_index :: find_indices (start_index + 1)
            else
                find_indices (start_index + 1)
        in
        find_indices 0
    in
    
    (* A list of tuples of string position and numeric value  *)
    let str_positions : (int * int) list =
        List.flatten (List.map (fun (word, digit) ->
            List.map (fun pos -> (pos, digit)) (find_start_indices_of_substring line word)
            ) strings_to_ints)
    in
    
    (* Handle converting the individial char digits into ints*)
    let num_positions = 
        List.init (String.length line) (fun i ->
            if '0' <= line.[i] && line.[i] <= '9' then
                Some (i, int_of_char line.[i] - int_of_char '0')
            else None)
    in

    (* Merge the str and the num position and maintain relative order by sorting the indices  *)
    let combined_positions = 
        let filtered_num_positions = List.filter_map (fun x -> x) num_positions in
        List.sort (fun (p1, _) (p2, _) -> compare p1 p2) (str_positions @ filtered_num_positions)
    in
    
    (* Return a list of the second element if each (index, value) tuple.  i.e. A list of values  *)
    List.map snd combined_positions
;;

let process_file filename : int list list =
    let file_channel = open_in filename in
    let lines_as_ints = ref [] in
    try 
        while true do
            let line = input_line file_channel in
            let line_as_ints =  convert_line_to_digits line in
            lines_as_ints := line_as_ints :: !lines_as_ints
        done; [] (* Unreachable code.  Loop will execute until EoF  *)
    with End_of_file ->
        close_in file_channel;
        !lines_as_ints
;;
    
let () = 
    let filename = "data.txt" in
    let lines_as_ints = process_file filename in
    let sum = ref 0 in
    List.iter (fun line_as_ints ->
        let tens_digit = List.hd line_as_ints in
        let ones_digits = List.hd (List.rev line_as_ints) in
        sum := ((tens_digit * 10 + ones_digits) + !sum);
    ) lines_as_ints;
    print_int !sum
