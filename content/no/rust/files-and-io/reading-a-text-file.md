---
date: 2024-01-20 17:55:14.991732-07:00
description: "Hvordan gj\xF8re det: Dette vil skrive ut innholdet i `hilsen.txt` til\
  \ konsollen."
lastmod: '2024-04-05T21:53:41.570120-06:00'
model: gpt-4-1106-preview
summary: Dette vil skrive ut innholdet i `hilsen.txt` til konsollen.
title: Lese en tekstfil
weight: 22
---

## Hvordan gjøre det:
```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("hilsen.txt")?;
    let mut innhold = String::new();
    file.read_to_string(&mut innhold)?;
    println!("Filinnhold: {}", innhold);
    
    Ok(())
}
```
Dette vil skrive ut innholdet i `hilsen.txt` til konsollen.

## Dypdykk
Tradisjonelt, i eldre språk som C, involverte å lese filer mye mer manuell håndtering av ressurser og feil. Rusts design omfavner "resource acquisition is initialization" (RAII) prinsippet, noe som betyr at når en variabel går ut av scope, vil dens ressurser automatisk bli frigjort. Det forenkler filhåndtering betydelig. 

Alternativer for å lese tekstfiler inkluderer høyere-nivå funksjoner som `fs::read_to_string` som lar deg lese en fil direkte til en streng med mindre kode, eller streaming av filinnhold med `BufReader` for mer effektiv behandling av store filer.

Når det gjelder implementasjonsdetaljer, vil IO-operasjoner i Rust vanligvis returnere `Result<T, E>`, som må håndteres for å håndtere potensielle feil. Dette fremmer robust, fail-safe kode.

## Se Også
- The Rust Programming Language Book: https://doc.rust-lang.org/book/
- Rust by Example: File I/O: https://doc.rust-lang.org/rust-by-example/std_misc/file.html
- Rust std::fs: https://doc.rust-lang.org/std/fs/index.html
- Rust std::io: https://doc.rust-lang.org/std/io/index.html
