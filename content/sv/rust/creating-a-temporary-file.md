---
title:    "Rust: Skapa en tillfällig fil"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att skapa en tillfällig fil kan vara mycket användbart när man programmerar i Rust. Det kan till exempel vara användbart när man arbetar med temporära data eller behöver spara information som inte behövs permanent.

## Hur man gör det

För att skapa en tillfällig fil i Rust, kan du använda standardbiblioteket `std::fs::File` tillsammans med `std::fs::tempfile()`-funktionen. Detta kommer att skapa en temporär fil i ditt operativsystems standard temporära katalog. Här är ett exempel på hur du kan göra det:

```Rust
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

fn main() -> Result<(), Box<dyn Error>> {
    // Skapa en ny tempfil
    let mut tmp_file = File::tempfile()?;

    // Skriv lite data till filen
    tmp_file.write(b"Hej, det här är en temporär fil!")?;

    // Hämta sökvägen till filen
    let path = tmp_file.path();

    println!("Tillfällig fil skapad på: {}", path.display());

    Ok(())
}
```

För att öka flexibiliteten kan du också ange ett annat namn eller en annan sökväg när du skapar den temporära filen. Detta görs genom att lägga till en sträng som argument i `tempfile()`-funktionen. Om du till exempel vill skapa en tempfil med namnet "temp.txt" i ditt aktuella projekt, kan du göra så här:

```Rust
let mut tmp_file = File::tempfile("temp.txt")?;
```

Hur som helst kommer filen att raderas automatiskt när `tmp_file`-variabeln går ut ur din scop.

## Djupdykning

Att skapa en tillfällig fil med `File::tempfile()` är enkelt, men vad händer egentligen bakom kulisserna? När du anropar funktionen, använder den det operativsystemsspecifika API:et för att skapa en temporär fil och returnerar en `File`-instans som pekar på den. Här är ett enkelt exempel på hur det kan se ut på Windows:

```Rust
use std::path::PathBuf;
use std::ffi::OsString;

#[cfg(windows)]
fn tempfile() -> Result<PathBuf, OsString> {
    let mut path = PathBuf::new();

    // Hämtar sökvägen till den temporära katalogen
    let result = ::std::env::temp_dir();

    if let Ok(path_dir) = result {
        // Skapar ett unikt filnamn
        let filename = ::uuid::Uuid::new_v4().hyphenated().to_string();
        
        // Sammanfogar parsing av sökvägen och filtiteln
        path.push(path_dir);
        path.push(filename);
        path.set_extension("tmp");
    }

    Ok(path)
}
```

Som du kan se här använder `tempfile()`-funktionen det inbyggda biblioteket `uuid` för att skapa ett unikt filnamn varje gång funktionen anropas. Det är den här typen av detaljer som vi kan ta för givet när vi använder standardbiblioteket i Rust.

## Se även

- [Rust standardbibliotek referens](https://doc.rust-lang.org/std/index.html)
- [File standardbibliotek](https://doc.rust-lang.org/std/fs/struct.File.html)
- [File::tempfile() funktion](https://doc.rust-lang.org/std/fs/fn.tempfile.html)