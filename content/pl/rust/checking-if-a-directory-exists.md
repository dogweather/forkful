---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:58:45.979575-07:00
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Sprawdzanie istnienia katalogu to potwierdzenie, czy ścieżka prowadzi do realnego folderu. Programiści robią to, aby uniknąć błędów przy próbie dostępu lub zapisu do nieistniejącego katalogu.

## How to: (Jak to zrobić:)
```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/tmp/example");

    if path.exists() {
        println!("Katalog istnieje!");
    } else {
        println!("Katalog nie istnieje!");
    }
}
```
W zależności od istnienia katalogu `/tmp/example`, wydrukowane zostanie "Katalog istnieje!" lub "Katalog nie istnieje!".

## Deep Dive (Dogłębna analiza)
Koncepcja sprawdzania istnienia katalogu jest stara jak systemy plików. W Rust, możemy to robić wykorzystując standardową bibliotekę `std::path::Path`, która zawiera metody do manipulacji i sprawdzania ścieżek plików i katalogów.

Alternatywą dla `path.exists()` jest użycie `fs::metadata()` i sprawdzenie, czy rezultat istnieje oraz czy jest katalogiem, co może dostarczyć więcej informacji, ale również wymaga obsługi błędów.

Implementacyjnie Rust korzysta z wywołań systemowych, aby ustalić stan pliku czy katalogu. Na różnych platformach może to wyglądać inaczej, ale abstrakcja `Path` dba o to, by developer nie musiał się tym martwić.

## See Also (Zobacz również)
- Dokumentacja Rust `Path`: https://doc.rust-lang.org/std/path/struct.Path.html
- Tutorial Rust o obsłudze błędów IO: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- Dokumentacja do `fs::metadata()`: https://doc.rust-lang.org/std/fs/fn.metadata.html
- Opis systemów plików: https://en.wikipedia.org/wiki/File_system
