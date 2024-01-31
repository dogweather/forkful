---
title:                "Skriva till standardfel"
date:                  2024-01-19
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel (`stderr`) är hur program rapporterar fel eller viktiga loggar. Programmerare gör det för att separera normal output från felmeddelanden, vilket gör felsökning och logghantering enklare.

## Hur man gör:
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Fel inträffade!").unwrap();
}
```
Sample output i terminalen:
```
Fel inträffade!
```

## Djupdykning
Historiskt sett kom `stderr` från UNIX där strömmarna `stdout` och `stderr` skapades för att skilja vanlig data från fel och loggar. Alternativ till att skriva direkt till `stderr` inkluderar loggbibliotek som `log` eller `env_logger`. `stderr` är implementerad i Rusts standardbibliotek via `std::io::stderr`, som returnerar en `Stderr` ström där man kan skriva.

## Se även
- Rusts dokumentation om error handling: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- `std::io` modulen i Rust standardbibliotek: https://doc.rust-lang.org/std/io/index.html
- Blogg inlägg om effektiv loggning: https://blog.rust-lang.org/2020/06/04/Rust-1.44.0.html#easier-logging-with-log-package-metadata
