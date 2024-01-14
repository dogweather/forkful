---
title:                "Rust: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns är en viktig del av att skriva effektiv kod i Rust. Genom att koda på ett sätt som gör att du kan hantera fall där en mapp inte finns, kan du förbättra användarupplevelsen och förhindra oönskade buggar i din programvara.

## Hur man gör det

För att kontrollera om en mapp finns i Rust, behöver du importera "std::fs" biblioteket och använda funktionen "metadata" för att hämta metadata om en fil eller mapp. Därifrån kan du använda "is_dir" funktionen för att avgöra om det är en mapp eller inte. Nedan finns ett exempel på hur det skulle se ut i kod:

```Rust
use std::fs;

let path = Path::new("mappen/som/vi/testar");

let metadata = fs::metadata(path).expect("Kan inte hämta metadata");

if metadata.is_dir() {
    println!("Mappen finns!");
} else {
    println!("Mappen finns inte!");
}
```

I exemplet ovan använder vi "expect" funktionen för att hantera eventuella fel som kan uppstå när vi försöker hämta metadata för en mapp. Det är viktigt att hantera detta på ett korrekt sätt för att undvika att programmet kraschar om en mapp inte finns.

## Djupdykning

När du använder "expect" funktionen i ovanstående kod, använder vi en panik för att hantera eventuella fel. Detta kan dock orsaka problem om du vill implementera en mer sofistikerad felhantering. Istället kan du använda "match" uttrycket för att hantera olika returvärden från "fs::metadata" funktionen. Här är ett exempel på hur det skulle se ut:

```Rust
match fs::metadata(path) {
    Ok(metadata)  => {
        if metadata.is_dir() {
            println!("Mappen finns!");
        } else {
            println!("Mappen finns inte!");
        }
    },
    Err(e) => println!("Ett fel inträffade: {}", e),
}
```

Genom att använda detta sätt att hantera fel kan du definiera åtgärder för olika situationer, istället för att bara använda en generell "panik" meddelande. Detta ger dig mer kontroll över din kod och kan hjälpa dig att hantera edge cases på ett bättre sätt.

## Se också

- [Rust dokumentation för "fs" biblioteket](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial för att hantera fel i Rust](https://www.youtube.com/watch?v=TB_9Va3jYOI)
- [Bästa praxis för att hantera filer och mappar i Rust](https://dev.to/qnighy/managing-files-and-directories-with-rust-3ad4)