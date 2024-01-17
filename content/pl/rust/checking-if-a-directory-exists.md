---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "Rust: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i po co?

Sprawdzanie, czy katalog istnieje, jest często wykonywanym zadaniem przez programistów. Jest to po prostu sposób na upewnienie się, czy dany katalog istnieje przed wykonaniem innych operacji na nim. Przykładowo, może to być użyteczne w przypadku tworzenia plików lub przenoszenia ich do konkretnego katalogu.

## Jak to zrobić:

```Rust
use std::fs;

let result = fs::metadata("nazwa_katalogu"); // sprawdzenie metadanych katalogu

if let Ok(metadata) = result { // sprawdzanie czy metadane istnieją
    if metadata.is_dir() { // sprawdzenie czy to jest katalog
        println!("Katalog istnieje!");
    }
} else {  // w przypadku niepowodzenia
    println!("Katalog nie istnieje!");
}
```

## Głębszy zanurzenie:

W przeszłości, sprawdzanie czy katalog istnieje było zadaniem bardziej skomplikowanym, często wymagającym korzystania z dodatkowych bibliotek lub funkcji. Jednak od wprowadzenia Rust w wersji 1.0, funkcja `fs::metadata()` jest dostępna w standardowej bibliotece, co znacznie ułatwia zadanie.

Alternatywną metodą sprawdzania czy katalog istnieje jest użycie funkcji `fs::read_dir()`, która zwraca iterator plików w danym katalogu. Jeśli iterator jest pusty, oznacza to, że katalog nie istnieje.

Implementacja tej funkcji może się różnić w zależności od systemu operacyjnego, jednak w przypadku popularnych systemów (jak Windows czy Linux), ta metoda jest dobrze zoptymalizowana, więc nie ma potrzeby martwić się o wydajność.

## Zobacz też:

- [Seria Rust and WinAPI: Tworzenie, odczytywanie, zmienianie i usuwanie plików i katalogów](https://www.youtube.com/playlist?list=PLprxlsTjFZ96yzN1iwQzuSa6n0Y3SSBWH)
- [Dokumentacja Rust: fs::metadata()](https://doc.rust-lang.org/std/fs/fn.metadata.html)