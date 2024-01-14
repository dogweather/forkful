---
title:    "Rust: Dette er tittelen på en artikkel om dataprogrammering: Å finne lengden av en streng."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor 

Rust er et kraftig og effektivt programmeringsspråk som er stadig mer populært blant utviklere. Det er designet for å hjelpe deg med å skrive sikker og rask kode, og har også et sterkt fokus på parallell programmering. En av de grunnleggende oppgavene i enhver programmeringsoppgave er å finne lengden på en streng. I denne bloggposten lærer du hvordan du gjør akkurat det ved hjelp av Rust-programmeringsspråket. 

## Slik gjør du det 

Først må du importere standardbiblioteket for å kunne jobbe med strenger i Rust: 

```Rust 
use std::str; 
``` 

Deretter kan du bruke funksjonen "len" for å finne lengden på en streng: 

```Rust 
let string = "Hei, verden!"; 
let length = str::len(string); 
println!("Lengden på strengen er {}", length); 
``` 

Output: 
Lengden på strengen er 13 

Du kan også bruke metoden "len" på en streng-verdi, uten å importere standardbiblioteket: 

```Rust 
let string = "Hei, verden!"; 
let length = string.len(); 
println!("Lengden på strengen er {}", length); 
``` 

Output: 
Lengden på strengen er 13 

Merk at begge disse metodene tar hensyn til unicode-tegn, så den faktiske lengden på en streng kan variere avhengig av antall tegn i strengen. 

## Dypdykk 

I Rust er en streng en samling av utf8-bytes, mens en unicode kodepoeng kan bestå av en eller flere utf8-bytes. Dette betyr at en enkelt unicode karakter kan ha en annen bytelengde enn en vanlig ascii-karakter. Derfor er det viktig å være oppmerksom på dette når du jobber med strenger i Rust. Det anbefales også å bruke metoden "chars" for å iterere gjennom en streng og få tilgang til hver unicode karakter enkeltvis. 

## Se også 

- [The Rust Programming Language](https://www.rust-lang.org) 
- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/index.html) 
- [Rust String Types](https://doc.rust-lang.org/std/string/struct.String.html)