---
title:                "Å starte et nytt prosjekt"
aliases:
- /no/rust/starting-a-new-project/
date:                  2024-01-20T18:04:21.356131-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å initialisere strukturen du trenger for å bygge noe fra scratch i Rust. Programmerere gjør dette for å skape et rent arbeidsområde med alle nødvendige verktøy og biblioteker.

## Slik gjør du:
For å starte, trenger du Rusts pakkehåndtering og byggeverktøy, Cargo:

```Rust
// Installer først Rust og Cargo ved å følge instruksjonene på https://rustup.rs/

// Åpne så en terminal og kjør:
cargo new mitt_prosjekt

// For å bygge og kjøre ditt nye prosjekt:
cd mitt_prosjekt
cargo run

// Du vil se output lignende dette hvis alt går etter planen:
   Compiling mitt_prosjekt v0.1.0 (/sti/til/mitt_prosjekt)
    Finished dev [unoptimized + debuginfo] target(s) in 0.5 secs
     Running `target/debug/mitt_prosjekt`
Hello, world!
```

## Deep Dive
Rust ble offisielt lansert i 2015 og har raskt blitt et populært språk for systemnære programmer. Med Cargo, som kom med fra starten, får man en robust pakke- og prosjektmanager. I motsetning til noen andre språk hvor du kanskje må bygge opp dette fra grunnen hver gang, gir Cargo deg en standardisert prosjektstruktur. Dette inkluderer en `Cargo.toml` for konfigurasjon, en `src` mappe for kildekoden din, samt det initielle `main.rs` eller `lib.rs` avhengig av om du bygger et binært eller et biblioteksprosjekt.

Det er verdt å nevne alternativer som `cargo-generate`, som lar deg starte et prosjekt basert på forskjellige maler. Dette kan være nyttig når du har mer spesifikke krav eller ønsker å adoptere bestemte prosjektstrukturer fra fellesskapet.

Implementasjonsdetaljer er enkle med Rust. Selve språket håndhever god kodepraksis som hjelper i storskala prosjekter, som eierskapsmodellen som hindrer minnelekkasjer og race conditions. Med Cargo blir det inkludert det som trengs for å kompilere prosjektet ditt: avhengigheter, bygg scripts og kommandoer for testing og benchmarking.

## Se også
- Rust hjemmeside: [https://www.rust-lang.org/](https://www.rust-lang.org/)
- Cargo bok: [https://doc.rust-lang.org/cargo/](https://doc.rust-lang.org/cargo/)
- `cargo-generate` GitHub side: [https://github.com/cargo-generate/cargo-generate](https://github.com/cargo-generate/cargo-generate)
- Rust og WebAssembly bok: [https://rustwasm.github.io/docs/book/](https://rustwasm.github.io/docs/book/)
