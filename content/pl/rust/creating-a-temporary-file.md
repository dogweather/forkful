---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Tworzenie tymczasowego pliku, to proces, w którym program tworzy plik na krótki okres, aby doraźnie przechowywać dane. Programiści robią to, gdy potrzebują tymczasowego miejsca do przechowywania i manipulowania danymi, które nie są wymagane długoterminowo.

## Jak to zrobić?

W języku Rust możliwe jest tworzenie tymczasowych plików za pomocą biblioteki standardowej.

```Rust
use std::fs::File;
use std::io::Write;
use std::env;

fn main() {
    let mut output = File::create(env::temp_dir().join("temp_file.txt")).expect("failed to create file");
    write!(output, "Hello, world!").expect("failed to write to file");
}
```
Wywołanie powyższego kodu utworzy plik o nazwie `temp_file.txt` w katalogu tymczasowym z tekstem `Hello, world!`.

## Deep Dive

Tworzenie plików tymczasowych jest praktyką rozpowszechnioną od początków informatyki. To sprawdzone rozwiązanie, ale nie jedno z możliwych. Alternatywą może być użycie bazy danych lub przechowywanie danych w pamięci.
 
Rust używa internych API systemu operacyjnego do tworzenia i zarządzania tymczasowymi plikami. Ta implementacja jest bardziej wydajna i bezpieczna, ponieważ Rust automatycznie zarządza i zamyka te pliki po skończeniu z nich korzystać.
 
## Zobacz też
 
- Dokumentacja Rust na temat `File`: [https://doc.rust-lang.org/std/fs/struct.File.html]
- Dokumentacja Rust na temat `env::temp_dir()`: [https://doc.rust-lang.org/std/env/fn.temp_dir.html]
- Informacje na temat zarządzania plikami i katalogami w Rust: [https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-io-idioms.html]