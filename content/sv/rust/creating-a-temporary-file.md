---
title:                "Att skapa en tillfällig fil"
html_title:           "Rust: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

För att vara ärlig, att skapa tillfälliga filer kan verka lite tråkigt. Men det är faktiskt en användbar funktion i många situationer. Dessa filer kan användas för att lagra temporär data, skapa säkra temporära kopior av befintliga filer, eller till och med för att testa kod utan att behöva permanent lagra den. 

## Hur man gör

För att skapa en temporär fil i Rust, används funktionen `tempfile::Builder`. Detta är en del av standardbiblioteket `tempfile`, så ingen extra installation är nödvändig. Här är en enkel kodexempel som skapar en temporär fil och skriver till den:

```rust
use std::io::prelude::*;
use std::fs::File;
use tempfile::Builder;

let temp_file = Builder::new()
    .prefix("temp")
    .suffix(".txt")
    .tempfile()
    .expect("Kunde inte skapa temporär fil");

// Skriv till filen
writeln!(temp_file, "Detta är en temporär fil")
    .expect("Kunde inte skriva till filen");

// Hämta filnamnet och skriv ut det
let file_name = temp_file.path();
println!("Skapad temporär fil: {:?}", file_name);
```

Det första steget är att importera `prelude` och `File` från standardbiblioteket `io`, samt `Builder` från `tempfile`. Sedan används `Builder` för att skapa en temporär fil med prefix "temp" och en suffix ".txt". Efter att filen har skapats kan du använda `writeln!` för att skriva till filen och `path()` för att hämta filnamnet.

Kör koden och du kommer att se att filen skapas i samma mapp som din Rust-fil finns i. Om du kör koden igen kommer en ny fil att skapas med ett annat slumpmässigt namn.

## Djupdykning

Nu när vi vet hur man skapar en temporär fil, låt oss titta på lite mer detaljerad information om funktionen `tempfile::Builder`.
Det finns flera anpassningsbara alternativ som kan användas till att skapa en temporär fil:

- `prefix` - Lägger till ett prefix till filnamnet för att öka läsbarheten.
- `suffix` - Lägger till en suffix till filnamnet för att indikera vilken typ av fil det är.
- `tempfile` - Skapar en ny fil med det angivna prefixet och suffixet.
- `tempdir` - Skapar en ny temporär katalog istället för en fil, och returnerar sökvägen till den.
- `rand_bytes` - Bestämmer hur många slumpmässiga byte som används för att generera filnamnet. Många bytes leder till längre och säkrare filnamn.

Det finns också några andra användbara funktioner som kan användas efter att en temporär fil har skapats:

- `into_file` - Konverterar `tempfile` till en vanlig `File` som kan användas för att läsa och skriva till.
- `persist` - Konverterar `tempfile` till en vanlig `File` och flyttar den till en önskad plats.
- `close` - Stänger `tempfile` och rensar den från systemet helt och hållet.

Med alla dessa alternativ finns det en hel del flexibilitet i hur du kan skapa och använda temporära filer i Rust.

## Se också

Här är några användbara länkar för att lära dig mer om att skapa temporära filer i Rust:

- [Rust standardbibliotek dokumentation om tempfile](https://doc.rust-lang.org/std/fs/tempfile/)
- [Rust by examples - creating temporary files](https://turreta.com/2016/10/18/creating-temporary-files-in-rust-by-example/)
- [Using Rust's tempfile crate for temporary files](https://joshmcguigan.com/blog/rust-tempfiles/)