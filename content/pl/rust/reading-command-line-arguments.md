---
title:                "Rust: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista powinien umieć odczytywać argumenty wiersza poleceń, ponieważ jest to ważna umiejętność w programowaniu w języku Rust. Pozwala to na dostarczanie argumentów do programu podczas jego uruchamiania, co często jest wymagane w przypadku tworzenia aplikacji konsolowych.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń w języku Rust, najpierw musimy zaimportować bibliotekę `std::env`. Następnie użyjemy funkcji `args()` do pobrania listy argumentów przekazanych do programu. Przykładowy kod wyglądać będzie następująco:

```rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Argumenty przekazane do programu: {:?}", args);
}
```

W powyższym przykładzie wykorzystaliśmy funkcję `collect()` do zamiany iteratora `args()` na wektor `Vec<String>`, aby móc łatwo wyświetlić przekazane argumenty.

### Przykładowe wywołanie programu

`$ ./program arg1 arg2 arg3`

### Oczekiwany wynik

`Argumenty przekazane do programu: ["./program", "arg1", "arg2", "arg3"]`

## Deep Dive

Istnieje wiele innych przydatnych funkcji biblioteki `std::env`, które pozwalają na bardziej zaawansowane operacje na argumentach wiersza poleceń. Na przykład, funkcja `current_dir()` pozwala na pobranie aktualnego katalogu, a funkcja `set_var()` umożliwia ustawienie własnej zmiennej środowiskowej. Aby dowiedzieć się więcej o możliwościach biblioteki `std::env`, warto przejrzeć jej dokumentację.

## Zobacz także

- [Dokumentacja biblioteki `std::env` w języku Rust](https://doc.rust-lang.org/std/env/index.html)
- [Poradnik odczytywania argumentów wiersza poleceń w języku Rust](https://www.tutorialspoint.com/rust/rust_command_line_arguments.htm)
- [Przydatne wskazówki dotyczące programowania w języku Rust](https://dev.to/frosnerd/3-tips-for-getting-started-with-rust-3m4c)