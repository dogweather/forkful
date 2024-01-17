---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Rust: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Czego & dlaczego?

Odczytywanie argumentów wiersza poleceń jest procesem, w którym programista pobiera informacje przekazywane z linii poleceń przy uruchamianiu programu. Jest to ważne, ponieważ pozwala na dostosowanie działania programu w zależności od przekazanych argumentów i pozwala użytkownikom lepiej kontrolować jego zachowanie.

## Jak:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Podane argumenty to: {:?}", args);
}
```
Przykładowy input/ouput:
```
$ ./program argument1 argument2
Podane argumenty to: [./program, argument1, argument2]
```

## Głębszy wgląd:

Odczytywanie argumentów wiersza poleceń jest powszechną praktyką w programowaniu i ma głębokie korzenie w systemach operacyjnych. Alternatywami do odczytywania argumentów wiersza poleceń są na przykład pliki konfiguracyjne lub pobieranie danych bezpośrednio od użytkownika. W Rust, argumenty wiersza poleceń są przechowywane jako wektor ciągów znaków i mogą być przekazane do programu poprzez funkcję `env::args()`.

## Zobacz także:

- Dokumentacja dotycząca odczytywania argumentów wiersza poleceń w Rust: https://doc.rust-lang.org/std/env/fn.args.html
- Tutorial na temat odczytu argumentów wiersza poleceń w Rust: https://www.tutorialspoint.com/rust-program-to-read-command-line-arguments
- Dyskusja na forum dotycząca różnych sposobów odczytywania argumentów wiersza poleceń w Rust: https://users.rust-lang.org/t/options-for-parsing-command-line-arguments-in-rust/5778