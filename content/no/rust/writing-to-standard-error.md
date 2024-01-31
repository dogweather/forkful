---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Standard error (stderr) brukes for å skrive ut feilmeldinger. Programmerere bruker det for å skille vanlig output fra feil, slik at feil kan logges eller behandles separat.

## How to:
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Feil! Noe gikk galt.").unwrap();
}
```
Output:
```
Feil! Noe gikk galt.
```

## Deep Dive
Historisk sett kommer standard error fra UNIX der tre hovedstrømmer ble definert: standard input (stdin), standard output (stdout), og standard error (stderr). Alternativer til `writeln!` inkluderer lavnivåfunksjoner som `write!` eller å bruke biblioteker som `log`. `stderr` er globalt og låst, derfor thread-sikkert.

## See Also
- Rust dokumentasjon på std::io: https://doc.rust-lang.org/std/io/
- Rust by Example på standard I/O: https://doc.rust-lang.org/rust-by-example/std_misc/file/stdio.html
- Bloggpost om logging i Rust: https://www.rust-lang.org/blog/2020/12/18/logging-in-rust.html
