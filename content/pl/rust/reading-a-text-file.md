---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie pliku tekstowego to proces odczytywania danych zapisanych jako tekst. Programiści robią to, aby uzyskać dostęp do informacji zapisanych w plikach i manipulować nimi.

## Jak to zrobić:

Do otwarcia i odczytywania pliku tekstowego w Rust, skorzystamy z funkcji `read_to_string()`. Poniżej znajduje się przykładowy kod.

```Rust
use std::fs;

fn main() {
    let text = fs::read_to_string("plik.txt")
        .expect("Nie można odczytać pliku");

    println!("{}", text);
}
```
Uruchomienie tego kodu spowoduje odczytanie pliku `plik.txt` i wydrukowanie jego zawartości na konsoli.

## Pogłębione informacje:

**Kontekst historyczny**: Rust jest stosunkowo młodym językiem programowania, premiera miała miejsce w 2010 roku. Do obsługi I/O plików, Rust zapewnia moduł `std::fs`, który zawiera różne funkcje pomocnicze, takie jak `read_to_string()`.

**Alternatywy**: Rust oferuje również metody do odczytu pliku linii po linii, co jest bardziej wydajne dla dużych plików. Możemy to zrobić za pomocą metody `lines()`.

```Rust
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let path = Path::new("plik.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }
    
    Ok(())
}
```
**Szczegóły implementacyjne**: `read_to_string()` jest wygodny, ale blokuje wątek, co oznacza, że inne operacje nie mogą być przeprowadzane równocześnie. W przypadku dużych plików mogłoby to prowadzić do problemów wydajności.

## Zobacz też:

1. [Dokumentacja Rust::fs](https://doc.rust-lang.org/std/fs/index.html)
2. [Przewodnik Rust do obsługi błędów](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
3. [Rust by Example: I/O z plikami](https://doc.rust-lang.org/rust-by-example/std_misc/file/open.html)
4. [Rust Async Programming](https://rust-lang.github.io/async-book/) - jak odczytywać pliki tekstowe asynchronicznie.