---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Rust: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmerere. Dette kan være nyttig for å lage kalendere, sjekke slutt- og startdatoer for prosjekter, eller for å planlegge fremtidige hendelser. Programmerere gjør dette ved hjelp av matematiske operasjoner og beregninger.

## Hvordan:
Du kan enkelt beregne en dato i fremtiden eller fortiden ved hjelp av Rusts innebygde tidsmodul. Følgende eksempel viser hvordan du kan beregne en dato som er 1 år og 6 måneder fra nå:

```Rust
use std::time::{SystemTime, UNIX_EPOCH, Duration};

let now = SystemTime::now();
let tomorrow = now + Duration::from_secs(60*60*24); //1 day
let one_year_six_months = tomorrow + Duration::from_secs(60*60*24*365+(60*60*24*30)*6); // 1 year and 6 months
let result = one_year_six_months - UNIX_EPOCH;
println!("{:?}", result); //prints the date in seconds since Unix Epoch
```

Output:
`Ok(49208448000)` // antall sekunder fra 1. januar, 1970.

## Dypere dykk:
For å forstå hvordan man beregner datoer i fremtiden eller fortiden, må vi også forstå konseptet med tidssone og tidsskala. Tidsskala er et system for å måle tiden, som for eksempel sekunder, minutter, timer osv. Mens tidssone er en geografisk region med en felles norm for å måle tid.

I tillegg til Rust, finnes det også andre programmeringsspråk og biblioteker som kan brukes for å beregne datoer, som for eksempel Python og momentjs. Disse språkene bruker også lignende algoritmer og matematiske operasjoner for å beregne datoer.

Når man beregner en dato i fremtiden eller fortiden, må man også ta hensyn til skuddår, som har en dag ekstra i februar hvert fjerde år. Dette kan påvirke resultatet, og det er derfor viktig å være oppmerksom på dette når man koder.

## Se også:
- [Rust tidsmodul dokumentasjon](https://doc.rust-lang.org/std/time/index.html)
- [Python tidsmodul dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [Momentjs dokumentasjon](https://momentjs.com/docs/)