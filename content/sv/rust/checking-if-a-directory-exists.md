---
title:    "Rust: Kontrollera om en mapp finns"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar kan vara en viktig del av många program, oavsett vilket programmeringsspråk man använder. I Rust är det extra viktigt att ha robusta och säkra program, vilket gör rätt hantering av filsystemet avgörande. Därför kan det vara till nytta att lära sig hur man kontrollerar om en mapp existerar i Rust.

## Så här gör du

För att kontrollera om en mapp existerar i Rust, börjar vi med att importera "std::fs" paketet. Sedan använder vi "Path" och "PathBuf" strukturerna för att skapa en sökväg till mappen vi vill kontrollera. Slutligen använder vi "PathBuf::exists()" funktionen för att kontrollera om sökvägen faktiskt leder till en existerande mapp.

```Rust
use std::fs::{self, Metadata};
use std::path::{Path, PathBuf};

fn main() {
    // Skapa en sökväg till mappen
    let folder_path = PathBuf::from("mapp_name");

    // Kontrollera om sökvägen leder till en existernade mapp
    if folder_path.exists() {
        println!("Mappen existerar!");
    } else {
        println!("Mappen existerar inte");
    }

}
```

Output:
```sh
Mappen existerar!
```

## Djupdykning

När vi använder "exists()" funktionen, returneras ett "true" eller "false" beroende på om sökvägen leder till en mapp eller inte. Detta är en mycket användbar funktion, men det är viktigt att notera att det inte kontrollerar om sökvägen leder till en fil eller inte. För att göra det måste vi använda "PathBuf::is_file()" eller "PathBuf::is_dir()" funktionerna.

Ett annat sätt att kontrollera om en mapp existerar är att använda "fs::metadata()" funktionen för att få metadata för filen eller mappen. Om sökvägen leder till en mapp, så returneras metadata för mappen. Om sökvägen leder till en fil, så returneras metadata för filen.

```Rust
use std::fs;

fn main() {
    // Skapa en sökväg till mappen
    let folder_path = "mapp_name";

    // Kontrollera om sökvägen leder till en existernade mapp
    if let Ok(metadata) = fs::metadata(file_path) {
        if metadata.is_dir() {
            println!("Mappen existerar!");
        } else if metadata.is_file() {
            println!("Filen existerar, inte en mapp");
        }
    } else {
        println!("Sökvägen är ogiltig eller mappen existerar inte");
    }
}
```

Output:
```sh
Mappen existerar!
```

## Se även

- [Rust standardbibliotek för filsystem](https://doc.rust-lang.org/std/fs/)
- [Djupdykning: Hur man arbetar med filsystemet i Rust](https://blog.rust-lang.org/inside-rust/2018/04/27/file-io.html)