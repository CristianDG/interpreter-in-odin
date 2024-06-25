package interpreter

import "core:testing"
import "core:fmt"
import "core:strings"
import "core:strconv"

// NOTE: https://vimeo.com/649009599 @ 15:50


prefix_parse_fn  :: #type proc(^Parser) -> Expression
infix_parse_fn :: #type proc(^Parser, Expression) -> Expression

Parser :: struct {
  lexer: ^Lexer,
  errors: [dynamic]string,
  cur_token: Token,
  peek_token: Token,
  // NOTE: será que isso não pode ser um `enumerated array`?
  // https://odin-lang.org/docs/overview/#enumerated-array
  prefix_parse_fns: map[TokenType]prefix_parse_fn,
  infix_parse_fns:  map[TokenType]infix_parse_fn,
}

ExpressionPrecedence :: enum {
  LOWEST = 0,
  EQUALS,
  LTGT,
  SUM,
  PRODUCT,
  PREFIX,
  CALL,
}

PRECEDENCES := #partial [TokenType]ExpressionPrecedence  {
  .EQ = .EQUALS,
  .NEQ = .EQUALS,
  .GT = .LTGT,
  .LT = .LTGT,
  .PLUS = .SUM,
  .MINUS = .SUM,
  .SLASH = .PRODUCT,
  .ASTERISK = .PRODUCT,
}


parser_new :: proc(l: ^Lexer, allocator := context.allocator) -> Parser {
  context.allocator = allocator
  p := Parser{
    lexer = l,
    errors = make([dynamic]string),
    prefix_parse_fns = make(map[TokenType]prefix_parse_fn),
    infix_parse_fns = make(map[TokenType]infix_parse_fn),
  }

  parser_next_token(&p)
  parser_next_token(&p)

  parser_register_prefix(&p, .IDENT, parse_identifier)
  parser_register_prefix(&p, .INT  , parse_integer_literal)
  parser_register_prefix(&p, .BANG , parse_prefix_expression)
  parser_register_prefix(&p, .MINUS, parse_prefix_expression)

  parser_register_infix(&p, .PLUS, parse_infix_expression)
  parser_register_infix(&p, .MINUS, parse_infix_expression)
  parser_register_infix(&p, .SLASH, parse_infix_expression)
  parser_register_infix(&p, .ASTERISK, parse_infix_expression)
  parser_register_infix(&p, .EQ, parse_infix_expression)
  parser_register_infix(&p, .NEQ, parse_infix_expression)
  parser_register_infix(&p, .GT, parse_infix_expression)
  parser_register_infix(&p, .LT, parse_infix_expression)

  return p
}

parse_infix_expression :: proc(using p: ^Parser, left: Expression) -> Expression {
  expr := InfixExpression {
    token=cur_token,
    operator=cur_token.literal,
    left=left,
  }

  precedence := parser_cur_precedence(p)

  parser_next_token(p)
  
  expr.right = parse_expression(p, precedence)

  e := new(InfixExpression)
  e^ = expr

  return e
}

parser_register_prefix :: proc(using p: ^Parser, t: TokenType, fn: prefix_parse_fn) {
  prefix_parse_fns[t] = fn
}

parser_register_infix :: proc(using p: ^Parser, t: TokenType, fn: infix_parse_fn) {
  infix_parse_fns[t] = fn
}

parser_peek_precedence :: proc(using p: ^Parser) -> ExpressionPrecedence {
  precedences := PRECEDENCES
  return precedences[peek_token.type]
}

parser_cur_precedence :: proc(using p: ^Parser) -> ExpressionPrecedence {
  precedences := PRECEDENCES
  return precedences[cur_token.type]
}

parse_prefix_expression :: proc(using p: ^Parser) -> Expression {
  expr := PrefixExpression{
    token = cur_token,
    // NOTE: acho melhor usar o .type, mas ¯\_(ツ)_/¯
    operator = cur_token.literal
  }

  parser_next_token(p)

  expr.right = parse_expression(p, .PREFIX)

  e := new(PrefixExpression)
  e^ = expr

  return e
}
parse_identifier :: proc(using p: ^Parser) -> Expression {
  expr := Identifier{token=p.cur_token, value=p.cur_token.literal}

  e := new(Identifier)
  e^ = expr

  return e
}

parser_peek_error :: proc(using p: ^Parser, t: TokenType) {
  sb := strings.builder_make(allocator = context.temp_allocator)

  fmt.sbprintf(
    &sb,
    "expected token to be %v, got %v instead",
    t,
    p.peek_token.type,
  )

  append(&errors, strings.to_string(sb))
}

no_prefix_parse_fn_error :: proc(using p: ^Parser, t: TokenType) {
  sb := strings.builder_make(allocator = context.temp_allocator)

  fmt.sbprintf(&sb, "no prefix function for %v found.", t,)

  append(&errors, strings.to_string(sb))
}

parser_next_token :: proc(using parser: ^Parser){
  cur_token = peek_token
  peek_token = lexer_next_token(lexer)
}

parser_expect_peek :: proc(using p: ^Parser, expected: TokenType) -> bool {
  if parser_peek_token_is(p, expected) {
    parser_next_token(p)
    return true
  }
  parser_peek_error(p, expected)
  return false
}

parser_peek_token_is :: proc(using p: ^Parser, expected: TokenType) -> bool {
  return peek_token.type == expected
}

parser_cur_token_is :: proc(using p: ^Parser, expected: TokenType) -> bool {
  return cur_token.type == expected
}

parse_integer_literal :: proc(using p: ^Parser) -> Expression {
  lit := Integer{token=cur_token}

  value, err := strconv.parse_i64(cur_token.literal)
  if err != true {
    sb := strings.builder_make(allocator = context.temp_allocator)
    fmt.sbprintf(&sb, "could not parse %v as integer", cur_token.literal)
    append(&errors, strings.to_string(sb))
  }

  lit.value = value

  e := new(Integer)
  e^ = lit

  return e
}

parse_let_statement :: proc(using p: ^Parser) -> Statement {


  stmt := LetStatement{token = p.cur_token}

  if !parser_expect_peek(p, .IDENT) {
    return nil
  }

  name :=  Identifier{token=p.cur_token, value=cur_token.literal}

  if !parser_expect_peek(p, .ASSIGN) {
    return nil
  }

  parser_next_token(p)

  // FIXME: será que isso está certo?
  //expr := parse_expression(p,.LOWEST)
  //stmt.value = expr
  
  // TODO: mudar depois
  if !parser_cur_token_is(p, .SEMICOLON){
    parser_next_token(p)
  }

  stmt.name = new(Identifier)
  stmt.name^ = name

  s := new(LetStatement)
  s^ = stmt

  return s
}

parse_expression :: proc(using p: ^Parser, precedence: ExpressionPrecedence) -> Expression {
  prefix, ok_prefix := prefix_parse_fns[cur_token.type]
  if !ok_prefix {
    no_prefix_parse_fn_error(p, cur_token.type)
    return nil
  }
  left_exp := prefix(p)

  for !parser_peek_token_is(p, .SEMICOLON) && precedence < parser_peek_precedence(p) {
    infix, ok_infix := infix_parse_fns[peek_token.type]
    if !ok_infix {
      return left_exp
    }

    parser_next_token(p)

    left_exp = infix(p, left_exp)
  }

  return left_exp
}

parse_expression_statement :: proc(using p: ^Parser) -> Statement {
  stmt := ExpressionStatement{token = cur_token}

  stmt.value = parse_expression(p, .LOWEST)

  if parser_peek_token_is(p, .SEMICOLON) {
    parser_next_token(p)
  }

  s := new(ExpressionStatement)
  s^ = stmt

  return s
}

parse_return_statement :: proc(using p: ^Parser) -> Statement {
  stmt := ReturnStatement{token = cur_token}

  parser_next_token(p)

  // TODO: será que isso ta certo?
  stmt.value = parse_expression(p, .LOWEST)

  parser_expect_peek(p, .SEMICOLON)

  // TODO: mudar depois?
  // for !parser_cur_token_is(p, .SEMICOLON) {
  //   parser_next_token(p)
  // }

  s := new(ReturnStatement)
  s^ = stmt

  return s
}

parse_statement :: proc(using p: ^Parser) -> (s: Statement) {

  #partial switch cur_token.type {
  case .LET: s = parse_let_statement(p)
  case .RETURN: s = parse_return_statement(p)
  case: s = parse_expression_statement(p)
  }

  return
}

parse_program :: proc(using parser: ^Parser, allocator := context.allocator) -> Program {
  context.allocator = allocator

  program := Program {}

  for cur_token.type != .EOF {

    s := parse_statement(parser)
    if s != nil {
      append(&program.statements, s)
    }

    parser_next_token(parser)
  }

  return program
}

