---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i dlaczego?"
Pisanie do standardowego błędu (stderr) to wyświetlanie informacji o błędach i diagnostyce. Programiści robią to, aby oddzielić zwykłe dane wyjściowe od komunikatów o błędach, co ułatwia debugging i logowanie.

## How to:
"Jak to zrobić:"
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Błąd: nie można otworzyć pliku").unwrap();
}
```
Powoduje wyświetlenie w stderr:
```
Błąd: nie można otworzyć pliku
```
```Rust
// Użycie eprintln! makra dla prostszych komunikatów
fn main() {
    eprintln!("To idzie do stderr!");
}
```
W konsoli zobaczysz:
```
To idzie do stderr!
```

## Deep Dive
"Głębsze spojrzenie":
Histerycznie `stderr` było używane zarówno w Unixie, jak i innych systemach operacyjnych do diagnostyki i raportowania błędów. Dwa główne strumienie to `stdout` (standardowe wyjście) i `stderr` (standardowy błąd). Użycie `stderr` pozwala na przekierowanie błędów i danych wyjściowych do różnych miejsc. W Rust możemy używać makr jak `eprintln!` lub moduł `std::io` dla większej kontroli.

## See Also
"Zobacz również":
- [Rust By Example - Stderr](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)
- [Rust std::io module](https://doc.rust-lang.org/std/io/index.html)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
