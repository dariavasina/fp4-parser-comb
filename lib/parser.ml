type input = {
  loc: int;
  str: string;
}

type json_value =
  | JsonNull
  | JsonBool of bool
  | JsonNumber of float
  | JsonString of string
  | JsonArray of json_value list
  | JsonObject of (string * json_value) list

type parser_error = ParserError of int * string

type 'a parser = input -> (input * 'a, parser_error) result

let get_input_head input =
  if input.str = "" then None
  else Some (String.get input.str 0, 
            { loc = input.loc + 1; 
              str = String.sub input.str 1 (String.length input.str - 1) })

(* return a parsed value *)
let return x input = Ok (input, x)

(* bind  - pass the parsed value and the remaning unconsumed input to a function *)
let (>>=) (p: 'a parser) (f: 'a -> 'b parser) : 'b parser = fun input ->
  match p input with
  | Ok (input', x) -> f x input'
  | Error e -> Error e

let (let*) = (>>=)

(* alternative operator - "or" operation for parsers*)
let (<|>) (p1: 'a parser) (p2: 'a parser) : 'a parser = fun input ->
  match p1 input with
  | Ok result -> Ok result
  | Error _ -> p2 input

let map (f: 'a -> 'b) (p: 'a parser) : 'b parser = fun input ->
  match p input with
  | Ok (input', x) -> Ok (input', f x)
  | Error e -> Error e

(* parser comes first, then function *)
let (>|=) p f = map f p

(* apply a parsed function to a parsed value *)
let (<*>) (p1: ('a -> 'b) parser) (p2: 'a parser) : 'b parser = fun input ->
  match p1 input with
  | Ok (input', f) -> 
    (match p2 input' with
     | Ok (input'', x) -> Ok (input'', f x)
     | Error e -> Error e)
  | Error e -> Error e

(* function comes first, then parser*)
let (<$>) = map 

(* run 2 parsers, ignore the result of the first *)
let ( *> ) p1 p2 = 
  let* _ = p1 in
  p2

(* run 2 parsers, ignore the result of the second *)
let ( <* ) p1 p2 =
  let* x = p1 in
  let* _ = p2 in
  return x

let rec show_json_value = function
  | JsonNull -> "null"
  | JsonBool b -> string_of_bool b
  | JsonNumber n -> string_of_float n
  | JsonString s -> 
      let escaped = String.concat "" (
        List.map (function
          | '\n' -> "\\n"
          | '"' -> "\\\""
          | '\\' -> "\\\\"
          | c -> String.make 1 c
        ) (List.init (String.length s) (String.get s))
      ) in
      Printf.sprintf "\"%s\"" escaped
  | JsonArray vs -> 
      "[" ^ String.concat ", " (List.map show_json_value vs) ^ "]"
  | JsonObject pairs ->
      let show_pair (k, v) = 
        Printf.sprintf "\"%s\": %s" k (show_json_value v)
      in
      "{" ^ String.concat ", " (List.map show_pair pairs) ^ "}" 