Scanner: spaces
  $ echo "    		 " | driver --lexer
  :2.0-2.0 Parser.EOF

Scanner: integer literal
  $ echo "27348" | driver --lexer
  :1.0-1.5 (Parser.LITINT 27348)
  :2.0-2.0 Parser.EOF

Scanner: integer literal has no signal
  $ echo "-27348" | driver --lexer
  :1.0-1.1 error: illegal character '-'
  [1]
  $ echo "+27348" | driver --lexer
  :1.0-1.1 Parser.PLUS
  :1.1-1.6 (Parser.LITINT 27348)
  :2.0-2.0 Parser.EOF

Scanner: types
  $ echo "int bool" | driver --lexer
  :1.0-1.3 Parser.INT
  :1.4-1.8 Parser.BOOL
  :2.0-2.0 Parser.EOF

Scanner: let
  $ echo "let in" | driver --lexer
  :1.0-1.3 Parser.LET
  :1.4-1.6 Parser.IN
  :2.0-2.0 Parser.EOF

Scanner: if
  $ echo "if then else" | driver --lexer
  :1.0-1.2 Parser.IF
  :1.3-1.7 Parser.THEN
  :1.8-1.12 Parser.ELSE
  :2.0-2.0 Parser.EOF

Scanner: identifier
  $ echo "Idade alfa15 beta_2" | driver --lexer
  :1.0-1.5 (Parser.ID "Idade")
  :1.6-1.12 (Parser.ID "alfa15")
  :1.13-1.19 (Parser.ID "beta_2")
  :2.0-2.0 Parser.EOF

Scanner: invalid identifier
  $ echo "_altura" | driver --lexer
  :1.0-1.1 error: illegal character '_'
  [1]

Scanner: invalid identifier
  $ echo "5x" | driver --lexer
  :1.0-1.1 (Parser.LITINT 5)
  :1.1-1.2 (Parser.ID "x")
  :2.0-2.0 Parser.EOF

Scanner: operators
  $ echo "+ <" | driver --lexer
  :1.0-1.1 Parser.PLUS
  :1.2-1.3 Parser.LT
  :2.0-2.0 Parser.EOF

Scanner: punctuation
  $ echo "( ) , =" | driver --lexer
  :1.0-1.1 Parser.LPAREN
  :1.2-1.3 Parser.RPAREN
  :1.4-1.5 Parser.COMMA
  :1.6-1.7 Parser.EQ
  :2.0-2.0 Parser.EOF
