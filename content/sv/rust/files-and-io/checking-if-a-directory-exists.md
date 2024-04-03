---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:37.008045-07:00
description: "Hur: Rusts standardbibliotek (`std`) inkluderar funktionalitet f\xF6\
  r att kontrollera existensen av en katalog genom modulerna `std::path::Path` och\u2026"
lastmod: '2024-03-13T22:44:37.713801-06:00'
model: gpt-4-0125-preview
summary: "Rusts standardbibliotek (`std`) inkluderar funktionalitet f\xF6r att kontrollera\
  \ existensen av en katalog genom modulerna `std::path::Path` och `std::fs`."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur:
Rusts standardbibliotek (`std`) inkluderar funktionalitet för att kontrollera existensen av en katalog genom modulerna `std::path::Path` och `std::fs`. Här är ett enkelt exempel som använder Rusts standardansats:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("Katalogen finns.");
    } else {
        println!("Katalogen finns inte.");
    }
}
```

Exempelutskrift, med antagandet att katalogen existerar:
```
Katalogen finns.
```

För mer komplexa scenarier eller avancerade funktioner (som asynkrona filsystemoperationer) kan du överväga att använda ett tredjepartsbibliotek såsom `tokio` med dess asynkrona `fs`-modul, särskilt om du arbetar inom en asynkron runtime. Så här kan du åstadkomma samma sak med `tokio`:

Först, lägg till `tokio` i din `Cargo.toml`:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Sedan, använd `tokio::fs` för att asynkront kontrollera om en katalog existerar:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Katalogen finns.");
            } else {
                println!("Sökvägen finns men är inte en katalog.");
            }
        },
        Err(_) => println!("Katalogen finns inte."),
    }
}
```

Exempelutskrift, med antagandet att katalogen inte existerar:
```
Katalogen finns inte.
```

Dessa exempel belyser hur Rust och dess ekosystem erbjuder både synkrona och asynkrona tillvägagångssätt för att kontrollera katalogexistens, vilket tillgodoser ett brett utbud av behov inom programvaruutveckling.
