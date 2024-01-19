---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Lua: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzenie, czy dany katalog istnieje, to podstawowe działanie, które sprawdza, czy dany katalog rzeczywiście istnieje w systemie plików. Programiści robią to, aby zapewnić poprawne działanie kodu i unikać błędów w czasie wykonania.

## Jak to zrobić:

Rust umożliwia proste sprawdzenie, czy katalog istnieje za pomocą funkcji `std::path::Path::exists()`. 

```Rust
use std::path::Path;

fn main() {
    let dir = Path::new("/sciezka/do/katalogu");

    if dir.exists() {
        println!("Katalog istnieje");
    } else {
        println!("Katalog nie istnieje");
    }
}
```

Wyjście z programu będzie wyglądało następująco:

```
Katalog istnieje
```

Lub:

```
Katalog nie istnieje
```
W zależności od stanu katalogu.

## Głębsze Zanurzenie

W przeszłości, w językach programowania bez wbudowanej obsługi systemu plików, takich jak C, programiści musieli korzystać z niskopoziomowych wywołań systemowych i obsługiwać wiele możliwych kodów błędów. Rust upraszcza to zadanie, zapewniając bezpieczną i wygodną abstrakcję.

Alternatywą dla użycia `Path::exists()` jest użycie `std::fs::metadata()`, które nie tylko sprawdzi, czy ścieżka istnieje, ale również zwróci informacje o niej, takie jak czy jest plikiem czy katalogiem.

```Rust
use std::fs;

fn main() {
    match fs::metadata("/sciezka/do/katalogu") {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Katalog istnieje");
            } else {
                println!("To nie jest katalog");
            }
        }
        Err(err) => println!("Błąd: {}", err),
    }
}
```

## Zobacz Również

- [Dokumentacja Rust dla `std::path::Path`](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Przewodnik "Jak używać systemu plików w Rust"](http://blog.guillaume-gomez.fr/articles/2016-03-15+How+to+use+rust+filesystem%3F)
- [Poradnik "Obsługa błędów w Rust"](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-error-patterns.html)