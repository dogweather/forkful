---
title:                "Rust: Tworzenie tymczasowego pliku"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego warto tworzyć tymczasowe pliki w Rust?

Tworzenie tymczasowych plików może być przydatne w wielu sytuacjach. Może być to konieczne, gdy chcemy zapisać jakieś dane tymczasowo, a nie mamy potrzeby trwałego zapisu na dysku. Może też być to potrzebne, gdy potrzebujemy wykonać jakieś operacje na pliku, ale nie chcemy nadpisywać oryginalnego pliku. W tym artykule pokażemy, jak w jaki sposób możesz tworzyć tymczasowe pliki w języku Rust.

## Jak to zrobić?

Kodowanie tymczasowych plików w Rust jest bardzo proste. Wystarczy wykorzystać wbudowaną bibliotekę standardową `std::fs` oraz funkcję `std::fs::File::create`. Oto przykładowy kod tworzący tymczasowy plik i zapisujący do niego dane:

```Rust
use std::fs::File;

fn main() {
    // Tworzenie pliku o nazwie "temp.txt" w aktualnym katalogu
    let mut file = File::create("temp.txt").expect("Nie udało się utworzyć pliku");

    // Zapisywanie danych do pliku
    file.write_all(b"Przykładowe dane").expect("Nie udało się zapisać danych do pliku");

    // Plik zostanie automatycznie usunięty po zakończeniu działania programu
    // Ten sam mechanizm jest wykorzystywany w przypadku błędów lub wyjątków
}
```

Po uruchomieniu tego kodu w aktualnym katalogu pojawi się plik o nazwie "temp.txt" zawierający napis "Przykładowe dane". Ważne jest, aby dodać instrukcję `mut` przed zmienną `file`, ponieważ będziemy modyfikować zawartość tego pliku.

## Głębszy insight

Tworzenie tymczasowych plików nie jest złożone, ale jest kilka rzeczy, które warto wiedzieć na temat tego procesu. Po pierwsze, w jaki sposób Rust usuwa tymczasowe pliki? Otóż, Rust tworzy plik w katalogu systemowym przeznaczonym do przechowywania tymczasowych plików, a następnie usuwa go po zakończeniu działania programu. Można także wykorzystać funkcję `std::fs::remove_file` do ręcznego usunięcia tymczasowego pliku.

Należy również zaznaczyć, że istnieje funkcja `std::fs::create_temp` umożliwiająca tworzenie tymczasowych plików w sposób bardziej złożony. Funkcja ta może przydać się, jeśli potrzebujemy tworzyć wiele tymczasowych plików o unikatowych nazwach.

## Zobacz też

- [Dokumentacja Rust dla `std::fs`](https://doc.rust-lang.org/std/fs/)
- [Przykład użycia funkcji `std::fs::create_temp`](https://doc.rust-lang.org/std/fs/fn.create_temp.html)