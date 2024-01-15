---
title:                "Sprawdzanie czy istnieje katalog."
html_title:           "Rust: Sprawdzanie czy istnieje katalog."
simple_title:         "Sprawdzanie czy istnieje katalog."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#

# Dlaczego

Sprawdzanie, czy dany katalog istnieje, jest ważnym elementem w procesie programowania. Wiele aplikacji wymaga dostępu do różnych katalogów, a udostępnienie tej funkcjonalności użytkownikom może poprawić doświadczenie korzystania z aplikacji.

# Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, w języku Rust jest bardzo proste. Możemy to zrobić za pomocą metody `metadata()` z biblioteki `std::fs`. Oto przykład kodu:

```rust
use std::fs;

let result = fs::metadata("/sciezka/do/katalogu");

if result.is_ok() {
    println!("Katalog istnieje.");
} else {
    println!("Katalog nie istnieje.");
}
```

Przykładowy wynik dla istniejącego katalogu wyglądałby tak:

```
Katalog istnieje.
```

A dla nieistniejącego:

```
Katalog nie istnieje.
```

# Wnikliwsze spojrzenie

Metoda `metadata()` zwraca strukturę `std::fs::Metadata`, która zawiera informacje o danym pliku lub katalogu. Wykorzystując odpowiednie metody tej struktury, możemy uzyskać informacje takie jak data modyfikacji, rozmiar czy uprawnienia dostępu.

Możemy również wykorzystać metodę `fs::read_dir()` w celu pobrania listy plików i podkatalogów w danym katalogu. Może to być przydatne przy przeglądaniu zawartości katalogu lub w przypadku tworzenia listy plików do przetworzenia.

# Zobacz również

Możesz dowiedzieć się więcej o sprawdzaniu katalogów i plików w języku Rust, korzystając z poniższych zasobów:

- [Dokumentacja języka Rust - std::fs](https://doc.rust-lang.org/std/fs/)
- [Kurs programowania w Rust - Sprawdzanie plików i katalogów](https://www.javatpoint.com/rust-checking-files-and-directories)
- [Blog o programowaniu w Rust - Praca z katalogami i plikami](https://miladrambazam.github.io/blog/rust-working-with-directories/)