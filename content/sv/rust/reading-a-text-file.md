---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att läsa en textfil innebär att utvinna data från en fil lagrad på din dator eller server. Programmerare gör det för att begära, bearbeta eller ändra lagrad information.

## Hur gör man:

Här är exempel på hur du läser en textfil i Rust:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("hello.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("{}", contents);
    Ok(())
}
```
Kör koden ovan och om din textfil ("hello.txt") innehåller "Hej, Världen!", kommer utmatningen att vara:

```Rust
Hej, Världen!
```

## Djupdykning

Historiskt sett har olika språk hanterat filinläsning på olika sätt, men i Rust är det hållbart och enkelt tack vare "std::fs" och "std:io::prelude".

Det finns alternativa sätt att läsa textfiler i Rust. Ett exempel är funktionen read_to_string(), vilket ger en snabbare metoden för att läsa hela filer till en sträng på en gång.

När det gäller implementationen öppnar File::open("hello.txt") filen och returnerar en "File" instans. Funktionen read_to_string() läser filen till det medskickade muterbara strängobjektet.

## Se även:

Om du vill lära dig mer om att hantera filer i Rust, kolla in dessa länkar:

1. Rusts officiella dokumentation om std::fs::File: https://doc.rust-lang.org/std/fs/struct.File.html
2. En utmärkt handledning på Rust Cookbook: https://rust-lang-nursery.github.io/rust-cookbook/file/read-write.html
3. En diskussion på Stack Overflow om att läsa textfiler i Rust: https://stackoverflow.com/questions/31192956/whats-the-de-facto-way-of-reading-and-writing-files-in-rust-1-x