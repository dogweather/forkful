---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:10.904229-07:00
description: 'Hoe te: In Rust gebruiken we de `format!` macro.'
lastmod: '2024-03-13T22:44:50.580131-06:00'
model: gpt-4-0125-preview
summary: In Rust gebruiken we de `format!` macro.
title: Een string interpoleren
weight: 8
---

## Hoe te:
In Rust gebruiken we de `format!` macro:

```Rust
fn main() {
    let name = "Ferris";
    let groet = format!("Hallo, {}!", name);
    println!("{}", groet); // Print "Hallo, Ferris!"
}
```
De `format!` macro werkt zoals `println!`, maar het retourneert de geformatteerde string in plaats van deze af te drukken.

## Diepere Duik
Rust koos voor macro's zoals `format!` voor string interpolatie boven syntax in de taal zelf. Waarom? Macro's zijn krachtig en flexibel—ze breiden de functionaliteit van de taal uit zonder complexe syntax.

Historisch gezien gebruikten talen zoals C functies zoals `sprintf`, onhandig en foutgevoelig. Rust's `format!` macro is veiliger en voorkomt veelvoorkomende fouten.

Er bestaan alternatieven, zoals concatenatie met `+` of de `format_args!` macro om heap allocatie te vermijden. Maar als het aankomt op gemak en duidelijkheid, is `format!` koning.

Performance notitie: `format!` alloceert geheugen. Voor prestatie-kritieke code, overweeg andere methoden, zoals rechtstreeks naar een buffer schrijven.

## Zie Ook
- Officiële Rust documentatie over `format!`: https://doc.rust-lang.org/std/macro.format.html
- `format!` versus `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- Rust bij Voorbeeld over formatteren: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
