---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:55:18.500741-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie pliku tekstowego to odczytywanie danych z pliku zapisanego w formacie tekstowym. Programiści robią to, by np. wczytać konfigurację, dane użytkownika lub materiały do dalszej obróbki.

## How to: (Jak to zrobić?)
Oto jak w Rust wczytać plik tekstowy, linia po linii, oraz prosty przypadek użycia.

```rust
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = Path::new("przyklad.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }
    Ok(())
}
```

Gdy masz plik `przyklad.txt` z treścią:
```
Witaj w Rust!
To jest przykład.
```

Po uruchomieniu, otrzymasz:
```
Witaj w Rust!
To jest przykład.
```

## Deep Dive (Dogłębna analiza)
Czytanie plików tekstowych w Rust opiera się na prymitywach wejścia/wyjścia z modułu `std::io`. Historia tego mechanizmu sięga języków C i Unix, gdzie operacje na plikach są fundamentalne dla systemu.

Poza podstawowym `File::open`, można używać `std::io::BufReader` dla efektywności - buforuje on dane, co zmniejsza ilość operacji I/O. Inne metody jak `read_to_string` pozwalają na wczytanie całego pliku od razu do `String`.

Masz też alternatywy: `std::fs::read_to_string` dla krótszego kodu, czy biblioteki zewnętrzne jak `serde` do deserializacji danych w formatach JSON, YAML itp.

## See Also (Zobacz też)
Następujące źródła mogą być przydatne dla lepszego zrozumienia tematu:

- [The Rust Programming Language - Ch 12.2, "Reading a File"](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)
- [Rust by Example - "File I/O"](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [Rust `std::io` documentation](https://doc.rust-lang.org/std/io/)
