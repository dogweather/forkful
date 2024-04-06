---
date: 2024-01-20 17:37:40.399193-07:00
description: "Slik gj\xF8r du: Output."
lastmod: '2024-04-05T21:53:41.564485-06:00'
model: gpt-4-1106-preview
summary: ''
title: Konvertere en dato til en streng
weight: 28
---

## Slik gjør du:
```Rust
use chrono::{NaiveDate, Datelike, Local};

fn main() {
    let local_date = Local::today();
    let date_string = local_date.format("%Y-%m-%d").to_string();
    println!("Dagens dato som streng: {}", date_string);

    let naive_date = NaiveDate::from_ymd(2023, 3, 14);
    let naive_date_string = naive_date.format("%A %d %B %Y").to_string();
    println!("Valgt dato som streng: {}", naive_date_string);
}
```
Output:
```
Dagens dato som streng: 2023-03-22
Valgt dato som streng: Tuesday 14 March 2023
```

## Dypdykk
Før biblioteker som `chrono` og innebygde funksjoner, måtte Rust-programmerere håndtere datoer manuelt, ofte ved å lage egne funksjoner for format og validering. `chrono` er idag "go-to" biblioteket for dato- og tidsoperasjoner i Rust. Når vi konverterer datoer til strenger, lar `chrono` oss bruke forhåndsdefinerte formatstrenger så vel som egendefinerte mønstre. Alternativer inkluderer bruke standardbiblioteket for enklere oppgaver, men det har begrenset funksjonalitet sammenlignet med `chrono`.

Implementasjonsdetaljer avhenger ofte av 'formatereren' vi bruker; i `chrono`, brukes `format`-metoden som tar en formatstreng. Denne strengen følger formateringsdirektiver som spesifisert i `strftime`-stilen kjent fra C-programmering. En annen viktig detalj er tidszonesensitivitet: `Local::today()` gir dagens dato i lokal tidssone, mens `NaiveDate` representerer en "naiv" dato uten tidssoneinformasjon.

## Se også
- `chrono` biblioteket: [docs.rs/chrono](https://docs.rs/chrono/)
- Rust's dato og tid API: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)
- `strftime` formateringsdirektiver: [strftime.org](https://strftime.org/)
