---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:37.408384-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog existerar är att verifiera om en given sökväg pekar på en faktisk mapp i filsystemet. Programmerare gör detta för att förebygga fel när filer ska läsas från eller skrivas till katalogen.

## Så här gör du:
Genom att använda Rusts standardbibliotek `std::fs` och `std::path::Path` kan vi enkelt kontrollera om en katalog finns.

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/en/existerande/katalog");

    if path.exists() {
        println!("Katalogen finns!");
    } else {
        println!("Katalogen finns inte.");
    }
}
```

Utfall från koden varierar beroende på om sökvägen existerar eller inte:

```
Katalogen finns!
```
eller
```
Katalogen finns inte.
```

## Fördjupning:
Historiskt sätt har olika programmeringsspråk olika sätt att hantera filsystemet på. Rust har en modern och säker tillvägagångssätt genom sin ägarskap och lånesemantik som förhindrar race conditions vid filåtkomster. Alternativen till `Path::exists` inkluderar att försöka öppna en fil i katalogen eller att använda metoder som `fs::metadata` för att få mer detaljerad information om filsystemet. När man arbetar med filsystemet är det viktigt att tänka på felhantering och att OS-specifika skillnader kan påverka hur sökvägar och filåtkomst hanteras.

## Se även:
- Rusts dokumentation om `Path`: https://doc.rust-lang.org/std/path/struct.Path.html
- Rust by Example om fil-I/O: https://doc.rust-lang.org/rust-by-example/std_misc/file.html
- `fs::metadata` dokumentation för mer komplex filsystemsinformation: https://doc.rust-lang.org/std/fs/fn.metadata.html
