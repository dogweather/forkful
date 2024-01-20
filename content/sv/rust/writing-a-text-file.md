---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att spara textdata till en fil på din dator. Programmerare gör det för att spara utdata, logga information eller konfigurera program.

## Hur gör man:
```Rust
use std::fs::File;
use std::io::Write;
use std::io;

fn main() -> io::Result<()> {
    let mut fil = File::create("textfil.txt")?;
    fil.write_all(b"Hej, varlden!")?;
    Ok(())
}
```
Output: Skapar en fil som heter "textfil.txt" med innehållet "Hej, varlden!"

## Fördjupning
Att skriva till textfilen i Rust har varit en grundläggande funktion sedan språkets tidiga dagar, inspirerad av liknande funktioner i andra systemnivå-språk som C och C++. Alternativ till `File::create` kan inkludera att arbeta med strömmar, anvÄnda biblioteket `serde` för att serialisera data, eller `async`-lösningar för icke-blockerande I/O. Detaljer som filbehörigheter, teckenkodning och felhantering är viktiga att tänka på när man skriver till filer.

## Se även
- Rust’s officiella dokumentation för modulen `std::fs`: https://doc.rust-lang.org/std/fs/index.html
- `serde` biblioteket för att serialisera och deserialisera data: https://serde.rs/
- Rust by Example för mer kodexempel: https://doc.rust-lang.org/rust-by-example/std_misc/file.html