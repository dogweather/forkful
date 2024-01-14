---
title:    "Rust: Läsa en textfil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa och förstå textfiler är en grundläggande färdighet för alla programmerare. För att kunna behandla och analysera data måste man kunna läsa in det från textfiler. Här kommer vi att utforska hur man kan göra det med hjälp av Rust-programmeringsspråket.

## Hur man läser en textfil

Först och främst måste vi importera standardbiblioteket "std::fs" för att få tillgång till funktionen "File::open", som låter oss öppna en textfil.

``` Rust
use std::fs::File;

let fil = File::open("exempel.txt").expect("Kunde inte öppna filen");
```

Nästa steg är att skapa en variabel som ska hålla vår filström och läsa in innehållet i textfilen med hjälp av "read_to_string" funktionen.

``` Rust
let mut innehall = String::new();
match fil.read_to_string(&mut innehall) {
   Ok(_) => println!("{}", innehall),
   Err(e) => println!("Kunde inte läsa filen: {}", e),
}
```

Vi kan nu skriva ut innehållet i textfilen på skärmen. Om allt går väl, kommer vi att se texten från vår fil i terminalen.

## Djupdykning

När vi läser in en textfil i Rust, så skapar programmet en instans av filtypen "std::fs::File". Denna fil ger oss tillgång till olika metoder för att läsa och manipulera filen. En av de mest användbara metoderna är "read_to_string", som vi nämnde i det tidigare exempel. Denna metod gör att vi kan läsa in hela filen och spara innehållet i en variabel som en sträng. Vi kan sedan använda detta innehåll för att utföra olika operationer.

En annan nyttig metod är "lines", som låter oss läsa in filen rad för rad och utföra operationer på varje rad separat. Detta kan vara användbart när man arbetar med stora textfiler som inte kan läsas in i minnet i ett enda skede.

## Se även

- [Rust Dokumentation - Läsning och skrivning av filer](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [Rust Dokumentation - Iteratorer för filer](https://doc.rust-lang.org/std/fs/struct.File.html#method.lines)
- [Officiell Rust-webbplats](https://www.rust-lang.org/sv-SE/)