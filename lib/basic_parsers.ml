open Parser

let char_parser (c: char) : char parser = fun input ->
  match get_input_head input with
  | Some (x, rest) when x = c -> Ok (rest, x)
  | Some (x, _) -> 
      Error (ParserError (input.loc, 
             Printf.sprintf "Expected '%c', but found '%c'" c x))
  | None -> 
      Error (ParserError (input.loc,
             Printf.sprintf "Expected '%c', but reached end of string" c))

let string_parser (s: string) : string parser = fun input ->
  let rec aux i input =
    if i >= String.length s then Ok (input, s)
    else
      match char_parser (String.get s i) input with
      | Ok (rest, _) -> aux (i + 1) rest
      | Error _ -> 
          Error (ParserError (input.loc,
                 Printf.sprintf "Expected \"%s\", but found \"%s\"" s input.str))
  in aux 0 input

(* parse a sequence of characters that match a predicate *)
let span_parser (pred: char -> bool) : string parser = fun input ->
  let rec aux acc input =
    match get_input_head input with
    | Some (c, rest) when pred c -> 
        aux (acc ^ String.make 1 c) rest
    | _ -> Ok (input, acc)
  in aux "" input

(* parse a single character that matches a predicate *)
let parse_if (desc: string) (pred: char -> bool) : char parser = fun input ->
  match get_input_head input with
  | Some (c, rest) when pred c -> Ok (rest, c)
  | Some (c, _) ->
      Error (ParserError (input.loc,
             Printf.sprintf "Expected %s, but found '%c'" desc c))
  | None ->
      Error (ParserError (input.loc,
             Printf.sprintf "Expected %s, but reached end of string" desc)) 