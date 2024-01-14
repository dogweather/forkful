---
title:    "Rust: Kontrollera om en mapp finns"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp finns kan vara ett viktigt steg i utveckling av program och applikationer. Det kan hjälpa till att säkerställa att programmet kan nå och använda viktig information som finns lagrad i en specifik mapp eller fil.

## Hur man gör det
```Rust
use std::fs;

fn main() {
    let directory = "test";
    if fs::metadata(directory).is_ok() {
        println!("Mappen {:?} finns!", directory);
    } else {
        println!("Mappen {:?} finns inte.", directory);
    }
}
```

Output: 
```
Mappen "test" finns inte.
```

## Djupdykning
I Rust finns det flera olika sätt att kontrollera om en mapp finns eller inte. En av de vanligaste metoderna är att använda sig av `fs::metadata()` funktionen, som returnerar en `std::fs::Metadata` typ. Om denna metadatado är tom eller inte kan avgöras om mappen existerar eller inte.

En annan metod är att använda sig av `Path` `exists()` funktionen som returnerar `true` eller `false` beroende på om sökvägen existerar eller inte.

## Se även
- [Rust dokumentation om fs::metadata()](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Rust dokumentation om Path.exists()](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)