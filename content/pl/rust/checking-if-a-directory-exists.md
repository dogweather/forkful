---
title:                "Rust: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzanie, czy istnieje katalog, jest ważnym elementem programowania w Rust. Dzięki temu możemy zapewnić, że nasz program będzie działał poprawnie i nie będzie miał problemów z dostępem do potrzebnych plików.

## Jak to zrobić

Sprawdzenie, czy katalog istnieje w Rust jest bardzo proste. Możemy użyć funkcji `std::fs::metadata`, która zwróci `Result` zawierający informacje o pliku lub błąd, jeśli plik nie istnieje. Oto przykładowa implementacja:

```Rust
use std::fs;
use std::path::Path;

fn main() {
    let path = Path::new("nazwa_katalogu");

    if path.exists() && path.is_dir() {
        println!("Katalog istnieje.");
    } else {
        println!("Katalog nie istnieje.");
    }
}
```

Jeśli katalog istnieje, powinniśmy zobaczyć w konsoli napis "Katalog istnieje.". W przeciwnym przypadku, jeśli katalog nie istnieje lub wystąpił błąd podczas sprawdzania, zobaczymy napis "Katalog nie istnieje.".

## Głębsze spojrzenie

Sprawdzenie, czy katalog istnieje, jest często wykonywane w programach, które operują na plikach i folderach. W przypadku, gdy nasz program musi operować na konkretnym katalogu, a ten katalog nie istnieje lub wystąpił błąd podczas jego tworzenia, możemy zapewnić, że nasz program nie będzie działał poprawnie. Dlatego, warto mieć na uwadze ten aspekt podczas tworzenia oprogramowania w Rust.

## Zobacz również

1. Dokumentacja Rust na temat funkcji `std::fs::metadata`: https://doc.rust-lang.org/std/fs/fn.metadata.html
2. Poradnik na temat sprawdzania, czy plik lub katalog istnieje w Rust: https://www.tutorialkart.com/rust-lang/check-if-file-or-directory-exists-rust/