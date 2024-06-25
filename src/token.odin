package interpreter

TokenType :: enum {
  ILLEGAL,
  EOF,

  IDENT,
  INT,

  ASSIGN,
  PLUS,
  MINUS,
  BANG,
  ASTERISK,
  SLASH,

  LT,
  GT,
  EQ,
  NEQ,

  COMMA,
  SEMICOLON,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,

  FUNCTION,
  LET,
  TRUE,
  FALSE,
  IF,
  ELSE,
  RETURN,
  
}

Token :: struct {
  type: TokenType,
  literal: string,
}
