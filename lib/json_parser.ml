open Parser
open Basic_parsers

(* parse all whitespace characters *)
let ws_parser : string parser = span_parser (fun c -> c = ' ' || c = '\n' || c = '\t' || c = '\r')

let json_null : json_value parser = 
  string_parser "null" >|= (fun _ -> JsonNull)

let json_bool : json_value parser =
  (string_parser "true" >|= (fun _ -> JsonBool true)) <|>
  (string_parser "false" >|= (fun _ -> JsonBool false))

let json_number : json_value parser =
  let is_digit c = c >= '0' && c <= '9' in
  let digits = 
    let* first = parse_if "digit" is_digit in
    let* rest = span_parser is_digit in
    return (String.make 1 first ^ rest)
  in
  let* sign = (char_parser '-' >|= (fun _ -> -1.)) <|> return 1. in
  let* int_part = digits >|= float_of_string in
  let* frac_part = 
    (let* _ = char_parser '.' in
     let* frac = digits in
     return (float_of_string ("0." ^ frac))) <|> return 0.
  in
  let* exp_part =
    ((let* _ = char_parser 'e' <|> char_parser 'E' in
      let* sign = (char_parser '+' >|= fun _ -> 1) <|>
                  (char_parser '-' >|= fun _ -> -1) <|>
                  return 1 in
      let* exp = digits >|= int_of_string in
      return (10. ** (float_of_int (sign * exp)))) <|> return 1.)
  in
  return (JsonNumber (sign *. (int_part +. frac_part) *. exp_part))

(* apply parser many times *)
let rec many (p: 'a parser) : 'a list parser = fun input ->
  ((let* x = p in
    let* xs = many p in
    return (x :: xs)) <|> return []) input

(* apply a sequence of parsers *)
let rec sequence (ps: 'a parser list) : 'a list parser = fun input ->
  match ps with
  | [] -> return [] input
  | p :: ps' ->
      (let* x = p in
       let* xs = sequence ps' in
       return (x :: xs)) input

(* parse elements separated by something *)
let sep_by (sep: 'a parser) (elem: 'b parser) : 'b list parser = fun input ->
  ((let* x = elem in
    let* xs = many (sep *> elem) in
    return (x :: xs)) <|> return []) input

let escape_char : char parser =
  let* _ = char_parser '\\' in
  (char_parser '"' >|= fun _ -> '"') <|>
  (char_parser '\\' >|= fun _ -> '\\') <|>
  (char_parser '/' >|= fun _ -> '/') <|>
  (char_parser 'b' >|= fun _ -> '\b') <|>
  (char_parser 'f' >|= fun _ -> '\012') <|>
  (char_parser 'n' >|= fun _ -> '\n') <|>
  (char_parser 'r' >|= fun _ -> '\r') <|>
  (char_parser 't' >|= fun _ -> '\t')

(* parse a character that's not a quote or a backslash *)
let normal_char : char parser =
  parse_if "normal char" (fun c -> c <> '"' && c <> '\\')

(* parse a json string that may contain escape sequences *)
let string_literal : string parser =
  let* _ = char_parser '"' in
  let* chars = many (escape_char <|> normal_char) in
  let* _ = char_parser '"' in
  return (String.concat "" (List.map (String.make 1) chars))

let json_string : json_value parser =
  string_literal >|= fun s -> JsonString s

let rec json_value input = 
  (ws_parser *> (json_null <|> json_bool <|> json_number <|> json_string <|> 
          json_array <|> json_object) <* ws_parser) input

and json_array input =
  (let* _ = char_parser '[' *> ws_parser in
   let* elements = sep_by (ws_parser *> char_parser ',' <* ws_parser) json_value in
   let* _ = ws_parser *> char_parser ']' in
   return (JsonArray elements)) input

and json_object input =
  (let* _ = char_parser '{' *> ws_parser in
   let pair =
     let* key = string_literal <* ws_parser <* char_parser ':' <* ws_parser in
     let* value = json_value in
     return (key, value)
   in
   let* pairs = sep_by (ws_parser *> char_parser ',' <* ws_parser) pair in
   let* _ = ws_parser *> char_parser '}' in
   return (JsonObject pairs)) input

let parse_json (s: string) : (json_value, parser_error) result =
  match json_value { loc = 0; str = s } with
  | Ok (_, value) -> Ok value
  | Error e -> Error e 