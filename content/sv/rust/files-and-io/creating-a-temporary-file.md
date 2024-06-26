---
date: 2024-01-20 17:41:17.890266-07:00
description: "Hur g\xF6r man: Skapandet av tempor\xE4ra filer \xE4r vanligt inom Unix-traditionen\
  \ och andra operativsystem fr\xE5n tidigt 70-tal. I Rust anv\xE4nder vi ofta crates\
  \ som\u2026"
lastmod: '2024-04-05T22:50:52.008786-06:00'
model: gpt-4-1106-preview
summary: "Skapandet av tempor\xE4ra filer \xE4r vanligt inom Unix-traditionen och\
  \ andra operativsystem fr\xE5n tidigt 70-tal."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

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
