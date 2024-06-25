package interpreter

import "core:testing"
import "core:log"

// TODO: trocar todos os log.errorf por testing.expectf ou testing.expect_value


@(test)
test_parse_let :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator

  lexer := lexer_new(`
  let x = 10;
  let y = 10;
  let foobar = 10;
  `)

  parser := parser_new(&lexer)

  program := parse_program(&parser)
  check_parse_errors(t, &parser)


  expected_statements : []struct {
    identifier : string
  } = {
    { "x" },
    { "y" },
    { "foobar" },
  }


  testing.expectf(t, !(len(program.statements) == 0), "program is []")

  testing.expectf(t,
    !(len(expected_statements) != len(program.statements)),
    "program length expected %v, got %v",
    len(expected_statements),
    len(program.statements))

  for expected, i in expected_statements {
    ok := test_let_statement(t, program.statements[i], expected.identifier)
    testing.expect(t, ok)
  }

  test_let_statement :: proc(t: ^testing.T, s: Statement, name: string) -> bool {


    let_stmt, ok := s.(^LetStatement)

    testing.expectf(t, ok, "not LetStatement, got %v", s) or_return
    testing.expect_value(t, token_literal(let_stmt), "let") or_return
    testing.expect_value(t, let_stmt.name.value, name) or_return
    testing.expect_value(t, token_literal(let_stmt.name), name) or_return

    return true
  }

}

check_parse_errors :: proc(t: ^testing.T, using p: ^Parser) {
  if len(errors) == 0 do return

  log.errorf("parser had %v errors", len(errors))
  for err in errors {
    log.errorf("parser error: %v", err)
  }

  testing.fail_now(t)
}


@(test)
test_return_statements :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator
  input := `
  return 5;
  return 10;
  return 1234123;
  `

  l := lexer_new(input)
  p := parser_new(&l)

  program := parse_program(&p)
  check_parse_errors(t, &p)

  if len(program.statements) != 3 {
    log.errorf("program statements does not contain 3 statements. got %v", len(program.statements))
    testing.fail_now(t)
  }

  for stmt in program.statements {
    if token_literal(stmt) != "return" {
      log.errorf("returnStmt.TokenLiteral not 'return', got %v", token_literal(stmt))
    }

    return_stmt, ok := stmt.(^ReturnStatement)
    if !ok {
      log.errorf("stmt not *ast.returnStatement. got %v", stmt)
      continue
    }
  }

}

test_identifier :: proc(t: ^testing.T, exp: Expression, value: string) -> bool {
  ident, ok_ident := exp.(^Identifier)
  if !ok_ident {
    log.errorf("exp not Identifier, got %v", exp)
    return false
  }

  if ident.value != value {
    log.errorf("value not %v, got %v", value, ident.value)
    return false
  }

  if ident.token.literal != value {
    log.errorf("token literal not %v, got %v", value, ident.token.literal)
    return false
  }

  return true
}

@(test)
test_expression_statement :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator
  input := `foobar;`

  l := lexer_new(input)
  p := parser_new(&l)

  program := parse_program(&p)

  if len(program.statements) != 1 {
    log.errorf("program has not enought statements. got %v", len(program.statements))
    return
  }

  stmt, ok_stmt := program.statements[0].(^ExpressionStatement)

  if !ok_stmt {
    log.errorf("not ExpressionStatement. got %v", program.statements[0])
    return
  }

  ident, ok_ident := stmt.value.(^Identifier)

  if !ok_ident {
    log.errorf("not Identifier. got %v", stmt)
    return
  }

  if ident.value != "foobar" {
    log.errorf("wrong Identifier value. expected foobar, got %v", stmt)
    return
  }

  if token_literal(ident) != "foobar" {
    log.errorf("wrong token literal. expected foobar, got %v", stmt)
    return
  }
}

@(test)
test_integer_literal_expression :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator
  input := `5;`

  l := lexer_new(input)
  p := parser_new(&l)

  program := parse_program(&p)

  if len(program.statements) != 1 {
    log.errorf("program has not enought statements. got %v", len(program.statements))
    return
  }

  stmt, ok_stmt := program.statements[0].(^ExpressionStatement)

  if !ok_stmt {
    log.errorf("not ExpressionStatement. got %v", program.statements[0])
    return
  }

  if !test_integer_literal(t, stmt.value, 5) {
    return
  }

  if token_literal(stmt.value) != "5" {
    log.errorf("wrong token literal. expected 5, got %v", stmt)
    return
  }
}

test_integer_literal :: proc(t: ^testing.T, exp: Expression, expected_value: i64) -> bool{

  integer, ok_integer := exp.(^Integer)

  if !ok_integer {
    log.errorf("not Integer. got %v", exp)
    return false
  }

  if integer.value != expected_value {
    log.errorf(
      "wrong integer value. expected %v, got %v",
      expected_value,
      integer.value,
    )
    return false
  }

  return true

}

@(test)
test_parsing_prefix_expressions :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator
  prefix_tests : []struct {
    input, operator: string,
    value: i64,
  } = {
    { "!5", "!", 5 },
    { "-14", "-", 14 },
  }

  for test in prefix_tests {
    l := lexer_new(test.input)
    p := parser_new(&l)

    program := parse_program(&p)
    check_parse_errors(t, &p)

    if len(program.statements) != 1 {
      log.errorf(
        "program statements mismatch, expected 1, got %v",
        len(program.statements),
      )
      return
    }

    stmt, ok_stmt := program.statements[0].(^ExpressionStatement)
    if !ok_stmt {
      log.errorf(
        "program statement not an expression, got %v",
        program.statements[0],
      )
      return
    }

    exp, ok_exp := stmt.value.(^PrefixExpression)
    if !ok_exp {
      log.errorf(
        "statement not PrefixExpression, got %v",
        stmt,
      )
      return
    }

    if exp.operator != test.operator {
      log.errorf(
        "expected operator %v, got %v",
        test.operator,
        exp.operator,
      )
      return
    }

    if !test_integer_literal(t, exp.right, test.value) {
      return
    }

  }

}

@(test)
test_parsing_infix_expressions :: proc(t: ^testing.T) {
  context.allocator = context.temp_allocator
  Test :: struct {
    input: string,
    leftValue: i64,
    operator: string,
    rightValue: i64,
  }

  infix_tests : []Test = {
    {"5 + 5;", 5, "+", 5},
    {"5 - 5;", 5, "-", 5},
    {"5 * 5;", 5, "*", 5},
    {"5 / 5;", 5, "/", 5},
    {"5 > 5;", 5, ">", 5},
    {"5 < 5;", 5, "<", 5},
    {"5 == 5;", 5, "==", 5},
    {"5 != 5;", 5, "!=", 5},
  }

  for test in infix_tests {
    l := lexer_new(test.input)
    p := parser_new(&l)

    program := parse_program(&p)
    check_parse_errors(t, &p)

    if len(program.statements) != 1 {
      log.errorf(
        "program statements mismatch, expected 1, got %v",
        len(program.statements),
      )
      return
    }
    stmt, ok_stmt := program.statements[0].(^ExpressionStatement)
    if !ok_stmt {
      log.errorf("program.Statements[0] is not ExpressionStatement. got %v",
        program.statements[0])
      return
    }
    exp, ok_exp := stmt.value.(^InfixExpression)
    if !ok_exp {
      log.errorf("exp is not InfixExpression. got %v", stmt.value)
      return
    }
    if !test_integer_literal(t, exp.left, test.leftValue) {
      return
    }
    if exp.operator != test.operator {
      log.errorf("Operator is not '%s'. got=%s",
        test.operator, exp.operator)
      return
    }
    if !test_integer_literal(t, exp.right, test.rightValue) {
      return
    }
  }
}


@(test)
test_operator_precedence_parsing :: proc(t: ^testing.T) {

  tests : []struct {
    input: string,
    expected: string
  } = {
    {
      "-a * b",
      "((-a) * b)",
    },
    {
      "!-a",
      "(!(-a))",
    },
    {
      "a + b + c",
      "((a + b) + c)",
    },
    {
      "a + b - c",
      "((a + b) - c)",
    },
    {
      "a * b * c",
      "((a * b) * c)",
    },
    {
      "a * b / c",
      "((a * b) / c)",
    },
    {
      "a + b / c",
      "(a + (b / c))",
    },
    {
      "a + b * c + d / e - f",
      "(((a + (b * c)) + (d / e)) - f)",
    },
    {
      "3 + 4; -5 * 5",
      "(3 + 4)((-5) * 5)",
    },
    {
      "5 > 4 == 3 < 4",
      "((5 > 4) == (3 < 4))",
    },
    {
      "5 < 4 != 3 > 4",
      "((5 < 4) != (3 > 4))",
    },
    {
      "3 + 4 * 5 == 3 * 1 + 4 * 5",
      "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    },
    {
      "3 + 4 * 5 == 3 * 1 + 4 * 5",
      "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    },
  }
  
  for test in tests {
    l := lexer_new(test.input)
    p := parser_new(&l, allocator = context.temp_allocator)

    program := parse_program(&p, allocator = context.temp_allocator)
    actual := program_string(program, allocator = context.temp_allocator)

    if actual != test.expected {
      log.errorf("expected %v, got %v", test.expected, actual)
    }
  }
}
