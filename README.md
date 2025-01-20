# OCaml JSON Parser

A monadic parser combinator library for JSON implemented in OCaml.

## Project Structure

### Core Parser (`lib/parser.ml`)
Contains the fundamental parser types and combinators:
- `input` type for tracking parsing position and remaining input
- `parser_error` type for error reporting with location
- Basic combinators:
  - `return` - create a parser that always succeeds with a value
  - `>>=` (bind) - sequence parsers, feeding results forward
  - `<|>` - alternative operator for trying different parsers
  - `map` - transform parser results
  - `*>` and `<*` - sequence operators that keep right or left result
  - Other helper operators like `>|=`, `<*>`, `<$>`

### Basic Parsers (`lib/basic_parsers.ml`)
Provides primitive parsing functions:
- `char_parser` - parse a specific character
- `string_parser` - parse a specific string
- `span_parser` - parse a sequence of characters matching a predicate
- `parse_if` - parse a single character matching a predicate

### JSON Parser (`lib/json_parser.ml`)
Implements the JSON specification using the parser combinators:
- JSON value types (`json_value`)
- Parsers for each JSON type:
  - `json_null` 
  - `json_bool` 
  - `json_number`
  - `json_string`
  - `json_array` 
  - `json_object`
- Helper functions:
  - `parse_json` - main entry point for parsing
  - `show_json_value` - convert parsed JSON to string

Run tests with:
```
dune runtest
```
## Building

Build the project with:
```
dune build
```

## Running

Run the project with:
```
dune exec ./bin/main.exe
```
