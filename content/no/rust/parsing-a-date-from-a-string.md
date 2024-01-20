---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å analysere en dato fra en streng er operasjonen hvor man oversetter en tekst representerende en dato til en datotype som programmeringsspråket kan forstå og jobbe med. Dette gjøres for å tillate manipulasjon, beregning og sammenligning av datoer på en mer formell og kontrollert måte.

## Hvordan gjøre det:
Her er et grunnleggende eksempel på hvordan du kan analysere en dato fra en streng i Rust:

```Rust
use chrono::{DateTime, NaiveDateTime, Utc};

fn main() {
    let dato_streng = "2022-12-24 08:00:00";
    let dato_format = "%Y-%m-%d %H:%M:%S";
    
    let n_dt = NaiveDateTime::parse_from_str(dato_streng, dato_format).unwrap();
    let dt: DateTime<Utc> = DateTime::from_utc(n_dt, Utc);
    
    println!("{}", dt);
}
```

I dette eksemplet kan vi se at en streng "2022-12-24 08:00:00" analyseres til en `NaiveDateTime` før den konverteres til en `DateTime <Utc>`.

## Deep Dive
Historisk sett har forskjellige programmeringsspråk hatt forskjellige måter å behandle parsing av datoer på. I eldre språk som C, var det nødvendig å manuelt behandle hver del av datostrengen, noe som økte sannsynligheten for feil og gjorde det mer tidkrevende. 

Rust, derimot, drar fordel av høy-nivå biblioteker som `chrono`, som forenkler prosessen betydelig. Andre alternativer inkluderer `time` biblioteket, men `chrono` er generelt mer allsidig og enklere å bruke.

Under parsingen prøver `chrono` å matche hvert element i inngangsstrengen med formatstrengen gitt. Hvis det ikke er en nøyaktig match, vil funksjonen returnere en feil. 

## Se også
For mer dyptgående detaljer, sjekk ut dokumentasjonen for `chrono` og` time`:

- Chrono Dokumentasjon: https://docs.rs/chrono/0.4.0/chrono/
- Time Dokumentasjon: https://docs.rs/time/0.1.42/time/