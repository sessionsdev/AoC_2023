module StringMap = Map.Make(String)

(* Validates that no color in a list of [(9, "red"), ...] exceeds the maximum for the game *)
let validate_color_set color_set =
    List.for_all (fun (num, color) ->  
        match color with
            | "blue" -> num <= 14
            | "red" -> num <= 12
            | "green" -> num <= 13
            | _ -> false;
    ) color_set

(* Converts "9 red, 10 blue, 3 green" to [(9, "red"), (10, "blue"), (3, "green")]  *)    
let convert_color_set_to_tuples color_set =
    let trimmed_colors = List.map (fun x -> String.trim x) (String.split_on_char ',' color_set) in
    List.map (fun e ->
        match (String.split_on_char ' ' e) with
        | [num; color] -> (int_of_string num, color)
        | _ -> failwith "Unexpected token"
    ) trimmed_colors

let validate_game color_set =
    validate_color_set (convert_color_set_to_tuples color_set)

let part_1_solver line = 
    match (String.split_on_char ':' line) with
    | game_num :: color_sets :: _ ->
            if List.for_all validate_game (String.split_on_char ';' color_sets) then
                begin
                    let split_game = String.split_on_char ' ' (String.trim game_num) in
                    match split_game with
                        | _ :: num :: _ -> 
                                int_of_string num
                        | _ -> 
                                print_string "No Value for game";
                                0
                end
            else
                0
    | _ -> failwith "Game does not have color sets"

(* Reduces the many color sets of the game to a list of non-dupe color and the highest value *)
let reduce_color_sets_to_highest_values color_sets =
    let update_map acc (num, color) =
        match StringMap.find_opt color acc with
            | Some existing_max -> StringMap.add color (max existing_max num) acc
            | None -> StringMap.add color num acc
        in
    let map = List.fold_left update_map StringMap.empty color_sets in
    StringMap.bindings map

let part_2_solver line =
    let game_power = ref 1 in
    match (String.split_on_char ':' line) with
    | _ :: color_sets :: _ -> 
            begin
                let color_sets_as_tuples = List.flatten (List.map convert_color_set_to_tuples (String.split_on_char ';' color_sets )) in
                let reduced_color_sets = reduce_color_sets_to_highest_values color_sets_as_tuples in
                List.iter (fun (_, num) -> game_power := !game_power * num ) reduced_color_sets;
                !game_power
            end
    | _ -> 0

let solve_for_file filename solver =
    let total = ref 0 in
    let file_channel = open_in filename in
    let rec read_loop () =
        try
            let line = input_line file_channel in
            total := (solver line) + !total;
            read_loop ()
        with
        | End_of_file -> 
            close_in file_channel
        | e -> 
            Printf.printf "Error: %s\n" (Printexc.to_string e);  (* Log any exceptions *)
            close_in file_channel
    in
    read_loop ();
    !total

let main () =
    let part_1_sum = solve_for_file "data.txt" part_1_solver in
    print_string (string_of_int part_1_sum);
    print_newline ();

    let part_2_sum = solve_for_file "data.txt" part_2_solver in
    print_string (string_of_int part_2_sum);
    print_newline ()

let () = main ()
