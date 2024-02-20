---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:17.426859-07:00
description: "Foutafhandeling gaat over omgaan met dingen wanneer ze mis gaan. Programmeren\
  \ doen we om het onverwachte aan te kunnen, door ervoor te zorgen dat hun\u2026"
lastmod: 2024-02-19 22:05:09.650240
model: gpt-4-0125-preview
summary: "Foutafhandeling gaat over omgaan met dingen wanneer ze mis gaan. Programmeren\
  \ doen we om het onverwachte aan te kunnen, door ervoor te zorgen dat hun\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling gaat over omgaan met dingen wanneer ze mis gaan. Programmeren doen we om het onverwachte aan te kunnen, door ervoor te zorgen dat hun Rust-programma's robuust zijn en niet zomaar crashen wanneer ze een hiccup tegenkomen.

## Hoe te:

Rust gaat op twee belangrijke manieren om met fouten: herstelbare en onherstelbare fouten. Laten we beide bekijken.

Herstelbare fouten gebruiken `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Bestand succesvol geopend."),
        Err(_e) => println!("Mislukt om bestand te openen."),
    }
}
```

Output kan "Bestand succesvol geopend." of "Mislukt om bestand te openen." zijn, afhankelijk van je `hello.txt`.

Voor onherstelbare fouten gebruiken we `panic!`:

```Rust
fn main() {
    // Dit zal het programma doen panikeren omdat het bestand waarschijnlijk niet bestaat.
    let _f = File::open("nergens.txt").unwrap();
}
```

Voer het uit, en je ziet een paniekbericht. Je programma stopt direct.

## Diepgaand

Historisch gezien is foutafhandeling in programmering een rommeltje geweest. Rust krijgt het juist met een duidelijk onderscheid tussen herstelbare en onherstelbare fouten.

De `Result` enum is voor herstelbare fouten. Het is expliciet - je handelt de `Ok` of `Err` variant af. Je hebt methoden zoals `unwrap()` en `expect()` ook, maar dat zijn snelle en vuile snelkoppelingen die kunnen leiden tot een `panic!`.

`panic!` is Rust's manier om te schreeuwen dat er iets heel ergs gebeurd is, en het kan niet. Het is als een onherstelbare fout die uitvoering onmiddellijk stopt. Een paniek in Rust wordt vaak gevoeld met bugs die je niet verwacht te moeten afhandelen, zoals indexeren buiten de grenzen van een array.

Foutafhandeling door het teruggeven van `Result` wordt voorkeur gegeven wanneer je verwacht fouten te moeten afhandelen. Het is idiomatic Rust, wat betekent dat het de manier is waarop Rust-ontwikkelaars overeengekomen zijn om dingen te doen. Er is ook `Option<T>`, voor gevallen waarin een fout gewoon iets is dat `None` is in plaats van `Some(T)`. Het gaat allemaal over het verwachten van het onverwachte zonder angst.

Alternatieven? Zeker, je zou andere foutafhandelingscrates kunnen gebruiken voor meer functies of ergonomisch gebruik. Zoals `anyhow` voor eenvoudige foutafhandeling, of `thiserror` voor fouten in bibliotheekcode.

## Zie Ook

Geïnteresseerd in dieper duiken? Hier is waar te gaan:

- [Rust Boek over Foutafhandeling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Een geweldige plek om de filosofie van Rust over foutafhandeling te begrijpen.
- [Rust door Voorbeeld: Foutafhandeling](https://doc.rust-lang.org/rust-by-example/error.html) - Interactieve voorbeelden om je handen vuil te maken.

Onthoud, goede foutafhandeling is niet alleen coderen; het is zorgen voor de gebruikers van je code. Gelukkig coderen!
