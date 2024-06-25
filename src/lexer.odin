package interpreter

import "core:fmt"
import "core:testing"
import "core:strings"

Lexer :: struct {
  input: string,
  position: int,      // current position in input (points to current char)
  read_position: int, // current reading position in input (after current char)
  ch: byte,
}


ident_type :: proc(ident: string) -> TokenType {
  keywords : map[string]TokenType = {
    "fn"     = .FUNCTION,
    "let"    = .LET,
    "true"   = .TRUE,
    "false"  = .FALSE,
    "if"     = .IF,
    "else"   = .ELSE,
    "return" = .RETURN,
  }

  return keywords[ident] or_else .IDENT
}

lexer_new :: proc(input: string) -> (l: Lexer) {
  l = {input=input}
  lexer_read_char(&l)
  return l
}

lexer_read_char :: proc(using l: ^Lexer) {
  if read_position >= len(input){
    ch = 0
  } else {
    ch = input[read_position]
  }
  position = read_position
  read_position += 1
}

lexer_peek_char :: proc(using l: ^Lexer) -> byte {
  if read_position > len(input) {
    return 0
  }
  return input[read_position]
}

lexer_read_identifier :: proc(using l: ^Lexer) -> string {
  start_position := position
  for is_letter(ch) || ch == '_' {
    lexer_read_char(l)
  }

  return input[start_position:position]
}

lexer_read_number :: proc(using l: ^Lexer) -> string {
  start_position := position
  for is_digit(ch) {
    lexer_read_char(l)
  }

  return input[start_position:position]
}

is_letter :: proc(ch: byte) -> bool {
  return ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z'
}

is_digit :: proc(ch: byte) -> bool {
  return ch >= '0' && ch <= '9'
}

lexer_next_token :: proc(using l: ^Lexer) -> (t: Token) {

  lexer_skip_whitespace(l)

  byte_to_str :: proc(ch: ..byte) -> string {
    return strings.clone_from_bytes(ch)
  }

  switch ch {
  case '=':{
    if lexer_peek_char(l) == '=' {
      fst := ch
      lexer_read_char(l)
      snd := ch
      t = Token{.EQ, byte_to_str(fst, snd)}
    } else {
      t = Token{.ASSIGN, byte_to_str(ch)}
    }
  }
  case ';': t = Token{.SEMICOLON , byte_to_str(ch)}
  case ',': t = Token{.COMMA     , byte_to_str(ch)}

  case '+': t = Token{.PLUS      , byte_to_str(ch)}
  case '-': t = Token{.MINUS     , byte_to_str(ch)}
  case '*': t = Token{.ASTERISK  , byte_to_str(ch)}
  case '/': t = Token{.SLASH     , byte_to_str(ch)}
  case '!':{
    if lexer_peek_char(l) == '=' {
      fst := ch
      lexer_read_char(l)
      snd := ch
      t = Token{.NEQ, byte_to_str(fst, snd)}
    } else {
      t = Token{.BANG, byte_to_str(ch)}
    }
  }

  case '>': t = Token{.GT        , byte_to_str(ch)}
  case '<': t = Token{.LT        , byte_to_str(ch)}

  case '(': t = Token{.LPAREN    , byte_to_str(ch)}
  case ')': t = Token{.RPAREN    , byte_to_str(ch)}
  case '{': t = Token{.LBRACE    , byte_to_str(ch)}
  case '}': t = Token{.RBRACE    , byte_to_str(ch)}
  case 0:   t = Token{.EOF, ""}
  case: {
    if is_letter(ch) || ch == '_'{
      ident := lexer_read_identifier(l)
      token_type := ident_type(ident)
      t = Token{token_type, ident}
      return
    } else if is_digit(ch) {
      val := lexer_read_number(l)
      t = Token{.INT, val}
      return
    } else {
      t = Token{.ILLEGAL, ""}
    }
  }
  }
  lexer_read_char(l)
  return
}

is_white_space :: proc(ch: byte) -> bool {
  return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r'
}


lexer_skip_whitespace :: proc(using l: ^Lexer) {
  for is_white_space(ch){
    lexer_read_char(l)
  }
}


@(test)
test_next_token :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator

  input :: `
  let five = 5;
  let ten = 10;

  let add = fn(x, y) {
    x + y;
  };

  let result = add(five, ten);
  !-/*5;
  5 < 10 > 5;

  if (5 < 10) {
    return true;
  } else {
    return false;
  }

  10 == 10;
  10 != 9;
  `

  l := lexer_new(input)

  tests : []struct {
    expected_type: TokenType,
    expected_literal: string
  } = {


    { .LET, "let" },
    { .IDENT, "five" },
    { .ASSIGN, "=" },
    { .INT, "5" },
    { .SEMICOLON, ";" },

    { .LET, "let" },
    { .IDENT, "ten" },
    { .ASSIGN, "=" },
    { .INT, "10" },
    { .SEMICOLON, ";" },

    { .LET, "let" },
    { .IDENT, "add" },
    { .ASSIGN, "=" },
    { .FUNCTION, "fn" },
    { .LPAREN, "(" },
    { .IDENT, "x" },
    { .COMMA, "," },
    { .IDENT, "y" },
    { .RPAREN, ")" },
    { .LBRACE, "{" },
    { .IDENT, "x" },
    { .PLUS, "+" },
    { .IDENT, "y" },
    { .SEMICOLON, ";" },
    { .RBRACE, "}" },
    { .SEMICOLON, ";" },


    { .LET, "let" },
    { .IDENT, "result" },
    { .ASSIGN, "=" },
    { .IDENT, "add" },
    { .LPAREN, "(" },
    { .IDENT, "five" },
    { .COMMA, "," },
    { .IDENT, "ten" },
    { .RPAREN, ")" },
    { .SEMICOLON, ";" },


    { .BANG, "!" },
    { .MINUS, "-" },
    { .SLASH, "/" },
    { .ASTERISK, "*" },
    { .INT, "5" },
    { .SEMICOLON, ";" },

    { .INT, "5" },
    { .LT, "<" },
    { .INT, "10" },
    { .GT, ">" },
    { .INT, "5" },
    { .SEMICOLON, ";" },


    { .IF, "if" },
    { .LPAREN, "(" },
    { .INT, "5" },
    { .LT, "<" },
    { .INT, "10" },
    { .RPAREN, ")" },
    { .LBRACE, "{" },
    { .RETURN, "return" },
    { .TRUE, "true" },
    { .SEMICOLON, ";" },
    { .RBRACE, "}" },
    { .ELSE, "else" },
    { .LBRACE, "{" },
    { .RETURN, "return" },
    { .FALSE, "false" },
    { .SEMICOLON, ";" },
    { .RBRACE, "}" },

    { .INT, "10" },
    { .EQ, "==" },
    { .INT, "10" },
    { .SEMICOLON, ";" },

    { .INT, "10" },
    { .NEQ, "!=" },
    { .INT, "9" },
    { .SEMICOLON, ";" },

    { .EOF, "" },

    // { .ASSIGN, "=" },
    // { .PLUS, "+" },
    // { .LPAREN, "(" },
    // { .RPAREN, ")" },
    // { .LBRACE, "{" },
    // { .RBRACE, "}" },
    // { .COMMA, "," },
    // { .SEMICOLON, ";" },
    // { .EOF, "" },

  }

  for test, i in tests {
    tok := lexer_next_token(&l)

    testing.expect_value(t, tok.type, test.expected_type)
    testing.expect_value(t, tok.literal, test.expected_literal)
    // if tok.type != test.expected_type {
    //   sb := strings.builder_make()
    //   fmt.sbprintf(&sb, "test[%v] - tokentype wrong. expected %v got %v", i, test.expected_type, tok.type)
    //   testing.fail_now(t, strings.to_string(sb))
    // }

    // if tok.literal != test.expected_literal {
    //   sb := strings.builder_make()
    //   fmt.sbprintf(&sb, "test[%v] - literal wrong. expected %v got %v", i, test.expected_literal, tok.literal)
    //   testing.fail_now(t, strings.to_string(sb))
    // }
  }

}
