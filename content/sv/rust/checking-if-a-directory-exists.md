---
title:                "Kontrollera om en mapp finns"
html_title:           "Rust: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är en vanlig uppgift för många programmerare. Genom att använda detta kodstycke kan du verifiera om en specifik mapp finns på en viss filväg, vilket kan vara mycket användbart när du arbetar med filhantering eller systemrelaterade applikationer.

## Så här gör du:
```Rust
use std::path::Path;
fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() {
        println!("Mappen finns!");
    } else {
        println!("Mappen existerar inte.");
    }
}

// Output:
// Mappen finns!
```

Här använder vi `Path` modulen som finns i Rusts standardbibliotek för att skapa en ny sökväg till mappen vi vill undersöka. Sedan använder vi funktionen `exists()` för att kontrollera om sökvägen verkligen leder till en existerande mapp. Slutligen skriver vi ut en passande meddelande beroende på resultatet.

## Djupdykning:
Kontrollen av mappens existens är en grundläggande uppgift som ofta används i kombination med andra filhanteringsfunktioner. Det finns också alternativa metoder för att uppnå samma resultat, till exempel att använda externa bibliotek som erbjuder specifika funktioner för att kontrollera mappar.

Implementeringen av `Path` modulen baseras på tekniken "Path normalization", som konverterar strängen till en absolut sökväg och sedan jämför den med den faktiska sökvägen på systemet. Det finns också andra funktioner i modulen som kan vara användbara för att manipulera sökvägar och kontrollera filers existens.

## Se också:
- [Rust dokumentation för Path modulen](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Rusts standardbibliotek](https://doc.rust-lang.org/std/index.html)