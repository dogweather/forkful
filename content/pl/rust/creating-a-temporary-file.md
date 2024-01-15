---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Rust: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Stworzenie tymczasowego pliku może być przydatne w wielu sytuacjach podczas programowania w Rust. Może to pomóc w przechowywaniu danych tymczasowych, testowaniu kodu lub utrzymywaniu czystości w projekcie.

## Jak to zrobić

Tworzenie tymczasowych plików w Rust jest stosunkowo proste. Wystarczy użyć funkcji `tempfile::tempdir()` lub `tempfile::tempfile()` z biblioteki `tempfile`. Następnie możemy wykonać operacje na tym pliku, na przykład zapisywać do niego dane lub odczytywać je z niego.

```Rust
use tempfile::{tempdir, TempDir, TempFile};
use std::io::{Read, Write};

// Tworzenie tymczasowego katalogu
let tmp_dir: TempDir = tempdir().expect("Nie można utworzyć tymczasowego katalogu");

// Tworzenie tymczasowego pliku
let mut tmp_file: TempFile = tmp_dir
    .create("example.txt")
    .expect("Nie można utworzyć tymczasowego pliku");

// Zapisywanie danych
write!(tmp_file, "Przykładowe dane").expect("Nie można zapisać danych do pliku");

// Odczytywanie danych
let mut contents = String::new();
tmp_file
    .read_to_string(&mut contents)
    .expect("Nie można odczytać zawartości pliku");

// Zamykanie tymczasowego pliku i katalogu
drop(tmp_file);
drop(tmp_dir);
```

Możemy również użyć metody `persist()` w celu zachowania tymczasowego pliku po zakończeniu działania programu.

```Rust
let tmp_file = tmp_dir
    .create("example.txt")
    .expect("Nie można utworzyć tymczasowego pliku");
tmp_file.persist("new_name.txt").expect("Nie można zapisać pliku");
```

## Wnikliwiej

Tworzenie tymczasowych plików z biblioteką `tempfile` jest bezpieczne dla wielowątkowości i odporniejsze na błędy niż samo używanie metod `File::create()` i `PathBuf::push()`. Ponadto, przy zakończeniu działania programu, wszystkie tymczasowe pliki zostaną automatycznie usunięte.

Przy używaniu metod `persist()`, tworzymy kopię tymczasowego pliku z zachowaniem atrybutów i dat modyfikacji. Może to być pomocne, gdy chcemy zachować wyniki testów lub danych tymczasowych, aby analizować je później.

## Zobacz również

- [Dokumentacja biblioteki `tempfile`](https://docs.rs/tempfile/latest/tempfile/)
- [Porównanie tworzenia tymczasowego pliku z i bez biblioteki `tempfile`](https://deterministic.space/tempfile.html)
- [Wzorcowy projekt wykorzystujący tworzenie i zachowywanie tymczasowych plików](https://github.com/Galli99/tempfile-example)