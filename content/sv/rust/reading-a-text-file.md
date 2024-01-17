---
title:                "Läsning av en textfil"
html_title:           "Rust: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär att läsa och extrahera data från en fil som innehåller text, såsom en vanlig textdokument. Programutvecklare gör detta för att hämta och bearbeta information som är sparad i filer och använda den i sina program.

## Så här gör man:
```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Öppna textfilen för läsning
    let mut file = match File::open("textfil.txt") {
        Ok(file) => file,
        Err(error) => panic!("Kan inte öppna filen: {}", error),
    };

    // Skapa en tom sträng för att lagra innehållet från filen
    let mut innehall = String::new();

    // Läs innehållet från filen till strängen
    match file.read_to_string(&mut innehall) {
        Ok(_) => println!("{}", innehall),
        Err(error) => println!("Kan inte läsa filen: {}", error),
    }
}
```

Exempel på output från koden ovan:

```
Detta är ett exempel på en textfil som innehåller information. 
Här kan vi skriva flera rader med data som vi senare kan använda i vårt program.
```

## Djupdykning:
Att läsa textfiler har varit en viktig del av programmering sedan de första datorerna. Det finns flera alternativ till Rust för att läsa filer, såsom C++ och Java. Implementeringen av att läsa filer i Rust använder sig av standardbiblioteket "std::fs" och funktionen "File::open" för att öppna en fil och "read_to_string" för att läsa innehållet till en sträng.

## Se även:
* [Rust dokumentation för att läsa filer](https://doc.rust-lang.org/std/fs/struct.File.html)
* [Livslog - En introduktion till filhantering i Rust](https://www.livslog.net/rust-file-io.html)