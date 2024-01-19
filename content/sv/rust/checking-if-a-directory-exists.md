---
title:                "Kontrollera om en katalog finns"
html_title:           "C: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns handlar om att verifiera att en viss sökväg faktiskt leder till en fysisk mapp. Programmörer gör det för att försäkra sig om att filsystemets operationer, till exempel läsning och skrivning, kan utföras säkert.

## Hur man gör:
Här är ett exempel på hur man kan kontrollera om en katalog finns i Rust:

```Rust
use std::path::Path;

fn main() {
    let dir_path = Path::new("/direktori/som/ska/kontrolleras");

    if dir_path.exists() {
        println!("Katalogen finns!");
    } else {
        println!("Katalogen finns inte!");
    }
}
```

Kör detta och du kommer att se ett av följande meddelanden i konsolen:
```
Katalogen finns!
```
eller
```
Katalogen finns inte!
```
beroende på om sökvägen finns eller inte.

## Djupdykning
Rust introducerade `std::path::Path` modulen i version 1.0.0 för att hantera filsystemets sökvägar. Det finns inget inneboende sätt att kontrollera om en mapp finns i tidigare versioner av Rust.

Det finns alternativa lösningar, till exempel att använda `std::fs::metadata(dir_path).is_ok()`, men de fungerar i grunden på samma sätt som `Path::exists()`.

`Path::exists()` fungerar genom att utföra ett systemanrop för att hämta metadata om filen eller katalogen vid den angivna sökvägen. Om anropet misslyckas (t.ex.: på grund av att sökvägen inte finns), returnerar `exists()` `false`.

## Se Även
[API-dokument för std::path::Path i Rust](https://doc.rust-lang.org/std/path/struct.Path.html)

[Officiell tutorial om hantering av filsystem i Rust](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html) 

[Diskussion om Path::exists() vs fs::metadata().is_ok()](https://users.rust-lang.org/t/path-exists-vs-fs-metadata-is-ok/28739)