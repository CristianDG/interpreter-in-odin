package interpreter

import "core:bufio"
import "core:io"
import "core:os"
import "core:fmt"

PROMPT :: "> "

repl_start :: proc(input: os.Handle, out: os.Handle) {
  scanner : bufio.Scanner
  bufio.scanner_init(&scanner, os.stream_from_handle(input))

  scanned := true
  for {
    if scanned do fmt.print(PROMPT)

    scanned = bufio.scanner_scan(&scanner)
    if !scanned do return

    line := bufio.scanner_text(&scanner)
    lexer := lexer_new(line)

    for
      t := lexer_next_token(&lexer);
      t.type != .EOF;
      t = lexer_next_token(&lexer) {
      fmt.fprintln(out, t)
    }
  }
}

