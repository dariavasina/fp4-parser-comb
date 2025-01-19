open Fp4.Parser
open Fp4.Json_parser

let test_json = {|
{
  "hello": [false, true, null, 42, "foo\n\u1234\"", [1, -2, 3.1415, 4e-6, 5E6, 0.123e+1]],
  "world": null
}
|}

let () =
  match parse_json test_json with
  | Ok value -> 
      Printf.printf "Successfully parsed JSON:\n%s\n" (show_json_value value)
  | Error (ParserError (loc, msg)) ->
      Printf.printf "Error at position %d: %s\n" loc msg 