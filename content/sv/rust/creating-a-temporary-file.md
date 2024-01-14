---
title:    "Rust: Skapa en tillfällig fil"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil är ett vanligt sätt att hantera data och resurser inom programmering. Det kan vara användbart när du behöver tillfälligt lagra data eller skapa en plats för tillfällig kommunikation mellan program eller applikationer.

## Hur man gör det

Det finns många olika sätt att skapa en temporär fil i Rust. Ett av de vanligaste sätten är att använda sig av biblioteket `std::fs::File` för att skapa en fil och sedan använda funktionen `file::create()` för att skapa en temporär fil. Exempelvis:

```rust
use std::fs::File;
use std::io::prelude::*;

let temp_file = File::create("temp.txt")?; // Skapar en temporär fil med namnet "temp.txt"
```

Du kan sedan skriva till filen och läsa från den precis som med en vanlig fil. När du är klar med filen kan du enkelt ta bort den genom att använda funktionen `std::fs::remove_file()`.

```rust
use std::fs::{self, File};
use std::io::prelude::*;

let mut temp_file = File::create("temp.txt")?;
write!(temp_file, "Detta är en temporär fil som skapats av Rust.")?; // Skriver till filen
let mut contents = String::new();
File::open("temp.txt")?.read_to_string(&mut contents)?; // Läser från filen

// Tar bort den temporära filen
fs::remove_file("temp.txt").expect("Kunde inte ta bort filen.");

// Kolla om filen verkligen är borttagen
match fs::metadata("temp.txt") {
    Ok(_) => println!("Fil hittad."), // Om filen fortfarande finns kommer detta att skrivas ut
    Err(_) => println!("Filen är borttagen."), // Om filen är borttagen kommer detta att skrivas ut
}
```

## En djupdykning

När du skapar en temporär fil i Rust, används en process som kallas för "shadowing". Det betyder att en filsystemslänk skapas mellan den temporära filen och en fil som redan existerar. När du tar bort den temporära filen tas bara länken bort, vilket inte påverkar den ursprungliga filen. Detta gör att arbetet med temporära filer effektivt och säkert.

En annan intressant aspekt av att skapa temporära filer i Rust är att det finns en inbyggd funktion som låter dig ange en hint om var filen bör lagras på systemet. Du kan göra detta genom att använda funktionen `std::env::temp_dir()`. Exempelvis:

```rust
use std::fs::File;
use std::io::prelude::*;
use std::env::temp_dir;

let temp_file = File::create(temp_dir().join("temp.txt"))?; // Skapar en temporär fil i den förvalda temporära katalogen
```

Detta kan vara användbart om du har specifika krav på var den temporära filen ska lagras, till exempel för säkerhetsändamål.

## Se även

- [The Rust Programming Language](https://www.rust-lang.org/) - Officiell webbplats för Rust-programmeringsspråket.
- [Rust Standard Library](https://doc.rust-lang.org/std/) - Officiell dokumentation för Rusts standardbibliotek.
- [Create a Temporary File in Rust](https://dev.to/deciduously/safely-creating-temporary-files-in-rust-fd6) - En guide till att skapa temporära filer i Rust.