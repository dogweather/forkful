---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att starta ett nytt projekt innebär att skapa en ny, fräsch kodbas utan tidigare arbete. Programmerare gör detta för att börja från grunden och utforma system enligt unika specifikationer.

## Hur till:
Här är steg för att starta ett nytt Rust-projekt.
```Rust 
// Ställ in Cargo (Rusts pakethanterare)
$ cargo new mitt_projekt

// Gå till projektet
$ cd mitt_projekt

// Visa projektstrukturen
$ tree .

./
├── Cargo.toml
└── src
    └── main.rs

1 directory, 2 files
```
`Cargo.toml` håller metadata för ditt projekt och dina beroenden, medan `src/main.rs` är var ditt program börjar.

Kör projektet med:
```Rust
// Kör programmet
$ cargo run

Compiling my_project v0.1.0 (/path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.65s
    Running `target/debug/my_project`
Hello, world!
```
## Djupgående
Att starta ett nytt projekt i Rust innebär att använda Cargo, Rusts officiella pakethanterare. Detta började med Cargo's införande 2014 som ett sätt att förenkla byggning och delning av Rust-paket.

Alternativen till att starta ett nytt projekt med `cargo new` inkluderar att manuellt skapa din `main.rs` och `Cargo.toml` filer, men detta är klart mer tidskrävande och krångligt.

När du kör `cargo new`, skapar Cargo en minimal "Hello, world!" applikation, tillsammans med `Cargo.toml` filen. Den placerar sedan applikationen i en ny katalog med namnet du angav.

## Se också
1. [Cargo's Officiella Dokumentation](https://doc.rust-lang.org/cargo/)
2. [Rust Programmeringsspråks Officiella Webbplats](https://www.rust-lang.org/)
3. [Rust's Färska ”Getting Started Guide"](https://www.rust-lang.org/learn/get-started)