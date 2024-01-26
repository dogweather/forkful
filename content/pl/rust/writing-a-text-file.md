---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pisanie pliku tekstowego to zapisywanie danych w formacie czytelnym dla człowieka. Programiści robią to, by trwale zapisywać wyniki pracy programu, ustawienia użytkownika lub logi.

## Jak to zrobić:
```Rust
use std::fs::File;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut plik = File::create("przyklad.txt")?;
    plik.write_all(b"Witaj, swiecie!")?;
    
    Ok(())
}
```
Po uruchomieniu, w pliku `przyklad.txt` zobaczysz:
```
Witaj, swiecie!
```

## Wnikliwe spojrzenie
Zapis pliku tekstowego w Rust ma swoje korzenie w podobnych operacjach w innych językach, ale wyróżnia się obsługą błędów i bezpieczeństwem typów. Alternatywy to między innymi biblioteki zewnętrzne jak `serde` do serializacji danych. Rust używa `trait` jak `Write`, by abstrahować detale implementacyjne.

## Zobacz także
- [Dokumentacja modułu `std::fs`](https://doc.rust-lang.org/std/fs/)
- [Dokumentacja modułu `std::io`](https://doc.rust-lang.org/std/io/)
- [Rust by Example - File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [The Rust Programming Language – Chapter 12](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
