---
title:    "Rust: Tworzenie pliku tymczasowego"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów często musi tworzyć tymczasowe pliki w swoich projektach. Jest to często stosowane w celu zapisania danych, tymczasowego przechowywania informacji lub tworzenia kopii zapasowych. W tym blogu przeprowadzimy Cię przez proces tworzenia tymczasowych plików w języku Rust.

## Jak to zrobić

Tworzenie tymczasowych plików w języku Rust jest proste i wygodne. Wystarczy użyć funkcji `tempfile` z biblioteki standardowej i przekazać jej nazwę pliku. Poniżej znajduje się kod przykładowy, który tworzy tymczasowy plik o nazwie "plik.txt" i zapisuje w nim ciąg znaków "Witaj, świecie!".

```Rust
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::io::Error;

fn main() {
  // Tworzenie tymczasowego pliku
  let mut temp_file = tempfile::tempfile().unwrap();

  // Zapisywanie tekstu do pliku
  let text = "Witaj, świecie!";
  temp_file.write_all(text.as_bytes()).unwrap();

  // Przeczytanie tekstu z pliku
  let mut buffer = String::new();
  temp_file.read_to_string(&mut buffer).unwrap();

  println!("{}", buffer); // Output: Witaj, świecie!

  // Usuwanie tymczasowego pliku
  let path = Path::new("plik.txt");
  match std::fs::remove_file(path) {
    Ok(_) => println!("Plik usunięty"),
    Err(e) => println!("Błąd: {}", e),
  }
}
```

## Deep Dive

Funkcja `tempfile` zwraca strukturę `NamedTempFile`, która reprezentuje tymczasowy plik w systemie plików. Plik ten jest automatycznie usuwany, gdy jest zamknięty lub gdy struktura jest zwalniana (jest to zaimplementowane w metodzie `Drop`).

Warto również zauważyć, że funkcja `tempfile` jest bezpieczna do wielowątkowego użytku, więc nie musisz martwić się o ryzyko wyścigów między wątkami.

## Zobacz również

- [Dokumentacja funkcji tempfile](https://doc.rust-lang.org/std/fs/fn.tempfile.html)
- [Przykładowe projekty wykorzystujące bibliotekę tempfile](https://crates.io/search?q=tempfile)