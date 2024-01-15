---
title:                "Pisanie pliku tekstowego"
html_title:           "Rust: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać plik tekstowy?

Pisanie plików tekstowych jest nieodłączną częścią programowania w języku Rust. Jest to prosty i skuteczny sposób na przechowywanie i przetwarzanie danych.

## Jak to zrobić?

Pisanie plików tekstowych w języku Rust jest bardzo proste. Wystarczy użyć funkcji `std::fs::File::create` i przekazać jej nazwę pliku, który chcemy utworzyć. Następnie możemy użyć metody `.write_all` na obiekcie pliku, aby zapisać nasz tekst do pliku. Przykładowy kod wyglądałby tak:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::create("plik.txt").expect("Nie można utworzyć pliku.");
    file.write_all(b"Witaj, świecie!").expect("Nie można zapisać do pliku.");
}
```

Po uruchomieniu tego kodu, powinien zostać utworzony plik `plik.txt` z zawartością "Witaj, świecie!". 

Aby móc zapisywać tekst w różnych językach, należy dodać odpowiednią deklarację kodowania do metody `write_all`. Przykładowo, jeśli chcemy zapisać polskie znaki, możemy użyć kodowania "UTF-8" w ten sposób:

```Rust
file.write_all("Witaj, świecie!".as_bytes()).expect("Nie można zapisać do pliku.");
```

## Czego warto się nauczyć?

Pisanie plików tekstowych w języku Rust jest prostym procesem, jednak warto wiedzieć kilka dodatkowych rzeczy. Możemy na przykład korzystać z biblioteki `std::io::LineWriter` lub `std::io::BufWriter`, aby znacznie zwiększyć wydajność naszego programu. Możemy także użyć metody `.flush` aby natychmiastowo zapisać zawartość bufora do pliku.

## Zobacz także

- [Dokumentacja języka Rust](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Przykłady zapisu do pliku w języku Rust](https://dev.to/rubenrp/guide-to-read-write-copy-files-in-rust-4j0p)