---
title:                "Skapa en temporär fil"
date:                  2024-01-20T17:41:17.890266-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skapa en temporär fil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil innebär att man temporärt lagrar data på disken under programmets körning. Programutvecklare gör detta för att hantera stora datamängder, dela data mellan olika processer, eller för att undvika att hålla data i minnet.

## Hur gör man:
```Rust
use std::fs::File;
use std::io::{self, Write};
use tempfile::Builder;

fn main() -> io::Result<()> {
    let mut temp_file = Builder::new().suffix(".tmp").tempfile()?;
    
    writeln!(temp_file, "Hej, det här är en temporär fil!")?;
    
    println!("Temporär fil skapad: {:?}", temp_file.path());
    Ok(())
}
```
Sample output:
```
Temporär fil skapad: Some("/tmp/.tmpQ2v4Z.tmp")
```

## Djupdykning:
Skapandet av temporära filer är vanligt inom Unix-traditionen och andra operativsystem från tidigt 70-tal. I Rust använder vi ofta crates som `tempfile` för att göra detta arbete smidigare och säkrare. Att använda temporära filer hjälper till att minska minnesanvändning, och är ofta snabbare än att arbeta helt i minnet för stora datamängder. `tempfile`-craten skapar en unik fil för varje instans och ser till att filen ofta tas bort automatiskt när den inte längre används. Alternativa sätt att hantera temporära data kan vara att använda in-memory datastrukturer, som till exempel en `Vec<u8>`, men detta är inte alltid lämpligt beroende på användningsfall och minnesförbrukning.

## Se även:
- [tempfile crate documentation](https://docs.rs/tempfile/)
- [The Rust Programming Language – Managing Files](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [Rust by Example – Std::io::Write](https://doc.rust-lang.org/rust-by-example/std_misc/file/create.html)
