---
title:                "Kontrollera om en katalog finns"
html_title:           "Rust: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kolla om en mapp existerar kan vara användbart i många situationer, till exempel för att undvika att dubbelkolla om en mapp redan finns innan du skapar den, eller för att verifiera att en angiven mapp verkligen existerar innan du försöker använda den.

## Så här gör du
Kontrollera om en mapp existerar i Rust är enkelt och kräver bara några få rader kod. Här är ett exempel:

```rust
use std::fs;

let dir_name = "min_mapp";

if fs::metadata(dir_name).is_ok() {
    println!("Mappen finns!");
} else {
    println!("Mappen finns inte.");
}
```

Om mappen "min_mapp" faktiskt existerar i det aktuella systemet, kommer du att se "Mappen finns!" som utskrift. Annars kommer du att se "Mappen finns inte.".

Det här är bara en grundläggande kontroll, men det finns flera andra sätt att kontrollera om en mapp existerar i Rust beroende på ditt specifika behov.

## Gräva djupare
I exemplet ovan använde vi funktionen `metadata()` från standardbiblioteket för att kontrollera om en mapp existerar. Detta returnerar en `Result` som antingen är `Ok` om mappen finns, eller `Err` om den inte finns.

En annan funktion som kan vara användbar för att kontrollera mappar är `exists()`, som returnerar ett `bool` direkt istället för ett `Result`. Det finns även möjlighet att använda `PathBuf` och `Path` för att skapa och manipulera sökvägar till mappar.

Det finns också flera externa bibliotek som är inriktade på filsystemshantering som kan vara värdefulla att utforska.

## Se även

* [Dokumentation för std::fs-modulen](https://doc.rust-lang.org/std/fs/index.html)
* [Dokumentation för PathBuf och Path](https://doc.rust-lang.org/std/path/)
* [rust-fs-extra biblioteket](https://github.com/webdesus/rust-fs-extra)