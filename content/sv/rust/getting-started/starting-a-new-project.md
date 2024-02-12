---
title:                "Att påbörja ett nytt projekt"
aliases:
- sv/rust/starting-a-new-project.md
date:                  2024-01-20T18:04:38.947128-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att påbörja ett nytt projekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att börja ett nytt projekt innebär att skapa en grund för din kodstruktur. Programmerare tar detta steg för att organisera sina idéer och bygga en hållbar bas för applikationsutveckling.

## How to:
För att kickstarta ett nytt Rust-projekt använder vi `cargo`, Rusts inbyggda paketmanager och byggverktyg. Här är grunderna:

```Rust
// Installera Rust med rustup om det inte redan är gjort:
// $ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

// Skapa ett nytt projekt
$ cargo new hej_världen
     Created binary (application) `hej_världen` package

// Navigera till din nya projektmapp
$ cd hej_världen

// Bygg och kör projektet
$ cargo run
   Compiling hej_världen v0.1.0 (/path/to/hej_världen)
    Finished dev [unoptimized + debuginfo] target(s) in 0.65 secs
     Running `target/debug/hej_världen`
Hello, world!
```

Du har nu skapat och kört ditt första Rust-projekt!

## Deep Dive
Rust kom till 2010, en skapelse av Graydon Hoare och sponsras av Mozilla Research. Cargo introducerades för att förenkla beroendehantering och byggprocesser, inspirerade av verktyg som `Bundler`, `npm`, och `pip`. Det konkurrerar med andra system programmeringsspråk men skiljer sig genom att prioritera säkerhet och hastighet samtidigt.

Alternativen till `cargo` för att hantera Rust-projekt kan inkludera manuell hantering eller användning av andra externa byggverktyg, men `cargo` är det överlägset mest populära och stöds väl av Rusts gemenskap.

Implementationen bakom `cargo new` kommandot skapar en `Cargo.toml` fil, som är hjärtat i projektets konfiguration. Det skapar också en `src` katalog med en `main.rs` fil, vilket är standardinställningen för Rusts källkodsfiler. Med Cargo hanteras projektets beroenden, byggen och tester på ett enhetligt sätt.

## See Also
Här är några nyttiga resurser för att fördjupa sig i Rust och Cargo:

- Rusts officiella bok, för en omfattande guide: [The Rust Programming Language](https://doc.rust-lang.org/book/)
- Cargo-dokumentationen, för mer detaljer om paketmanager: [Cargo Book](https://doc.rust-lang.org/cargo/)
- Rust by Example, för kodexempel: [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- Krates.io, Rusts paketregister: [Crates.io](https://crates.io/)

Genom att dyka in i dessa resurser, kan du bygga vidare på din kunskap om Rust och effektivisera din projektstruktur och dina arbetsflöden.
