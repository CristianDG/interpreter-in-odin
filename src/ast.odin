package interpreter

import "core:testing"
import "core:reflect"
import "core:bytes"
import "core:strings"
import "core:fmt"

Node :: struct {
  token: Token,
}

Expression :: union {
  ^Identifier,
  ^Integer,
  ^PrefixExpression,
  ^InfixExpression,
}

Statement :: union {
  ^LetStatement,
  ^ReturnStatement,
  ^ExpressionStatement,
}

Program :: struct {
  statements: [dynamic]Statement
}

Identifier :: struct {
  using node: Node,
  value: string,
}

Integer :: struct {
  using node: Node,
  value: i64,
}

InfixExpression :: struct {
  using node: Node,
  // TODO: se pá não deveria ser string
  operator: string,
  left:  Expression,
  right: Expression,
}

PrefixExpression :: struct {
  using node: Node,
  // TODO: se pá não deveria ser string
  operator: string,
  right: Expression,
}

LetStatement :: struct {
  using node: Node,
  name:  ^Identifier,
  value: Expression,
}

ReturnStatement :: struct {
  using node: Node,
  value: Expression
}

ExpressionStatement :: struct {
  using node: Node,
  value: Expression
}

token_literal :: proc {
  node_token_literal,
  statement_token_literal,
  expression_token_literal,
}

node_token_literal :: proc(node: Node) -> string {
  return node.token.literal
}

expression_token_literal :: proc(e: Expression) -> string {
  switch v in e {
  case ^Integer: return node_token_literal(v)
  case ^Identifier: return node_token_literal(v)
  case ^PrefixExpression: return node_token_literal(v)
  case ^InfixExpression: return node_token_literal(v)
  }
  return ""
}

identifier_string :: proc(e: ^Identifier) -> string {
  return e.value
}

expression_string :: proc(e: Expression) -> string {
  switch v in e {
  case ^Identifier: return identifier_string(v)
  case ^Integer: return node_token_literal(v)
  case ^PrefixExpression: return prefix_expression_string(v)
  case ^InfixExpression: return infix_expression_string(v)
  case: panic("TODO")
  }
  return ""
}

statement_token_literal :: proc(s: Statement) -> string {
  switch v in s {
  case ^LetStatement: return node_token_literal(v)
  case ^ReturnStatement: return node_token_literal(v)
  case ^ExpressionStatement: return node_token_literal(v)
  }
  return ""
}

prefix_expression_string :: proc(e: ^PrefixExpression) -> string {

  buf := bytes.Buffer {}

  bytes.buffer_write_string(&buf, "(")
  bytes.buffer_write_string(&buf, e.operator)
  bytes.buffer_write_string(&buf, expression_string(e.right))
  bytes.buffer_write_string(&buf, ")")
  
  return bytes.buffer_to_string(&buf)

}

infix_expression_string :: proc(e: ^InfixExpression) -> string {

  buf := bytes.Buffer {}

  bytes.buffer_write_string(&buf, "(")
  bytes.buffer_write_string(&buf, expression_string(e.left))
  bytes.buffer_write_string(&buf, " ")
  bytes.buffer_write_string(&buf, e.operator)
  bytes.buffer_write_string(&buf, " ")
  bytes.buffer_write_string(&buf, expression_string(e.right))
  bytes.buffer_write_string(&buf, ")")
  
  return bytes.buffer_to_string(&buf)

}

let_statement_string :: proc(s: ^LetStatement) -> string {
  assert(s != nil, "nil statement")
  assert(s.name != nil, "nil identifier")

  buf := bytes.Buffer {}

  bytes.buffer_write_string(&buf, s.token.literal)
  bytes.buffer_write_string(&buf, " ")
  bytes.buffer_write_string(&buf, s.name.value)
  bytes.buffer_write_string(&buf, " = ")
  bytes.buffer_write_string(&buf, expression_string(s.value))
  bytes.buffer_write_string(&buf, ";")
  
  return bytes.buffer_to_string(&buf)
}

return_statement_string :: proc(s: ^ReturnStatement) -> string {
  assert(s != nil, "nil statement")

  buf := bytes.Buffer {}
  bytes.buffer_write_string(&buf, s.token.literal)
  if s.value != nil {
    bytes.buffer_write_string(&buf, " ")
    bytes.buffer_write_string(&buf, expression_string(s.value))
  }
  bytes.buffer_write_string(&buf, ";")

  return bytes.buffer_to_string(&buf)
}

expression_statement_string :: proc(s: ^ExpressionStatement) -> string {
  assert(s != nil, "nil statement")
  buf := bytes.Buffer {}
  bytes.buffer_write_string(&buf, expression_string(s.value))
  // NOTE: não sei pq eu coloquei isso, então vou deixar comentado
  // bytes.buffer_write_string(&buf, ";")

  return bytes.buffer_to_string(&buf)
}

statement_string :: proc(s: Statement) -> string {

  switch v in s {
  case ^ReturnStatement: return return_statement_string(v)
  case ^LetStatement: return let_statement_string(v)
  case ^ExpressionStatement: return expression_statement_string(v)
  }

  return ""

}

program_string :: proc(using p: Program, allocator := context.allocator) -> string {
  context.allocator = allocator
  buf := bytes.Buffer {}

  for s in statements {
    bytes.buffer_write_string(&buf, statement_string(s))
  }

  return bytes.buffer_to_string(&buf)
}

program_token_literal :: proc(p: Program) -> string {
  if len(p.statements) > 0 {
    return statement_token_literal(p.statements[0])
  }
  return ""
}

@(test, private="package")
test_program_string :: proc(t: ^testing.T) { 
  context.allocator = context.temp_allocator
  program := Program {
    statements = [dynamic]Statement {
      &LetStatement{
        token = Token{type= TokenType.LET, literal="let"},
        name = &Identifier {
          token = Token{type= TokenType.IDENT, literal="myVar"},
          value = "myVar"
        },
        value = &Identifier {
          token = Token{type= TokenType.IDENT, literal="anotherVar"},
          value = "anotherVar"
        }
      }
    }
  }

  testing.expect_value(t, program_string(program), "let myVar = anotherVar;")
}

