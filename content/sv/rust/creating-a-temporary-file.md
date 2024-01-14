---
title:                "Rust: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
I många programmeringsprojekt är det nödvändigt att skapa temporära filer för att hantera dataåtkomst eller för att testa funktioner. Att lära sig hur man skapar temporära filer med Rust är ett användbart verktyg för programmerare som arbetar med filer och datahantering. 

## Så här gör man
Att skapa en temporär fil med Rust är enkelt. Du behöver bara använda standardbibliotekets "tempfile" modul och dess "named_tempfile" funktion. Nedan finns ett exempel på hur du skapar en temporär fil och skriver data till den: 

```Rust
use std::fs::File;
use std::io::prelude::*;
use tempfile::NamedTempFile;

fn main() {
    let mut temp_file = NamedTempFile::new().unwrap(); // skapar en temporär fil
    write!(temp_file, "Hej världen!"); // skriver data till filen
    println!("Skapade {} som innehåller: ", temp_file.path().display());
}
```

Output:
```
Skapade /tmp/temp-file-testxr7XL5kbJm6k som innehåller:
```

Det är viktigt att notera att temp_file.path() returnerar den faktiska sökvägen till den skapade filen. Nu kan du använda filen i ditt program och sedan ta bort den när den inte längre behövs. 

## Djupdykning
När vi skapar en temporär fil använder vi faktiskt Linux-kärnan som skapar en unik fil för oss. Standardbibliotekets "tempfile" modul tar hand om allt detta åt oss. Det är också möjligt att skapa en unik fil baserat på ett prefix och en suffix som du kan lägga till som argument för NamedTempFile::new() funktionen. Slutresultatet blir en temporär fil med en unik filnamn som är svårt att duplicera. 

## Se även
- [Rust standardbibliotekets "tempfile" modul](https://doc.rust-lang.org/std/io/struct.TempFile.html)
- [Officiell dokumentation för tempfile biblioteket](https://docs.rs/tempfile/3.1.0/tempfile/)