---
title:                "Rust: Kontrollera om en katalog finns"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför kontrollera om en mapp finns? 

Att kontrollera om en mapp existerar kan vara användbart när man vill utföra en viss handling baserat på dess existens. Till exempel kanske du vill skapa en mapp om den inte redan finns, eller radera en mapp om den redan finns. Detta är en vanlig uppgift inom programmering och därför är det viktigt att veta hur man kontrollerar om en mapp existerar.

## Så här gör du

För att kontrollera om en mapp existerar i Rust kan du använda funktionen `std::path::Path::exists` som returnerar `true` om sökvägen finns och `false` om den inte finns. Du behöver först importera detta bibliotek i din kod genom att inkludera `std::path` i ditt `main.rs` fil. Därefter kan du använda funktionen på följande sätt:

```Rust 
use std::path::Path;

fn main() {
    let path = Path::new("mappnamn");
    if path.exists() {
        println!("Mappen finns!");
    } else {
        println!("Mappen finns inte!");
    }
}
```

Om mappen "mappnamn" finns kommer output att vara "Mappen finns!", annars kommer det att vara "Mappen finns inte!". Detta visar hur man enkelt kan kontrollera om en mapp existerar och agera baserat på det.

## Djupdykning

Det finns flera sätt att kontrollera om en mapp existerar i Rust, men den mest effektiva är genom att använda `std::path::Path::exists` eftersom den bara returnerar en bool och inte söker genom hela filsystemet. Om du vill ha mer information om en mapp kan du också använda funktionen `std::fs::metadata` som ger dig åtkomst till metadata för en fil eller mapp, inklusive storlek, ägare och rättigheter. Detta kan vara användbart om du behöver göra mer detaljerade kontroller.

# Se även

Här är några länkar som kan vara användbara för att lära dig mer om att kontrollera mappar i Rust:

- [Officiell dokumentation för std::path::Path](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Officiell dokumentation för std::fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Stack Overflow-tråd om hur man kontrollerar om en mapp existerar i Rust](https://stackoverflow.com/questions/26934771/how-can-i-check-if-a-directory-exists-in-a-rust-program)