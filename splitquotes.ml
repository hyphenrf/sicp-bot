let chan = open_in "quotes";;
while true do
    input_line chan
    |> begin fun line ->
       if String.length line > 280 then begin
          let store = ref "" in
          let total = ref 0  in
          String.split_on_char ' ' line
          |> Stream.of_list
          |> fun stream ->
             let rec consume words = Stream.peek words |> function
             | None -> false
             | Some word ->
               if String.length !store + String.length word > 270 then
                  true
               else (
                  store := !store ^" "^ (Stream.next words);
                  consume words
               )
             in let flag  = ref true in
                let final = ref ""   in
             while !flag do
               incr total;
               flag  := consume stream;
               store := !store ^ (Printf.sprintf " (%d/\n" !total);
               final := !final ^ !store;
               store := "";
             done;
                String.split_on_char '\n' !final
                |> List.filter_map (function "" -> None | x -> Some (Printf.sprintf "%s%d)" x !total))
                |> String.concat "\n"
       end
       else line
    end
    |> print_endline
done;;
close_in chan;;
