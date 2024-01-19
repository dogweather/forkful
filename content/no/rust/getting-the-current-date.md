---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente nåværende dato er å få nøyaktig dato for det nåværende øyeblikket. Programmere gjør dette for å logge hendelser, generere tidsstemplede data, eller implementering av funksjonalitet som påminnelser og alarmer.

## Hvordan gjøre det:

For å få tak i dagens dato i Rust, bruker vi en pakke som heter `chrono`. Her er hvordan du bruker den:

```Rust
// inkluder 'chrono' biblioteket
extern crate chrono;

// innporter 'Local' fra 'chrono'
use chrono::Local;

fn main() {
    // hente den nåværende datoen
    let i_dag = Local::now();
    println!("I dag er: {}", i_dag);
}
```

Kjør programmet, og du vil se en utskrift som ligner på dette:

```Rust
I dag er: 2023-04-13 13:37:00.764056
```

## Dyp Dykk

Historisk sett, har programmerere alltid hatt behov for å hente den nåværende datoen; det er en grunnleggende funksjon som nesten alle programmer trenger. Før `chrono`, brukte Rust standard ```time``` biblioteket som ikke var like robust eller fleksibel.

Det er alternativer til `chrono`. For eksempel, `time`-pakken er mindre kraftig, men også mindre tungvinne. Hvilken du skal bruke kommer an på dine behov.

Når det gjelder implementeringsdetaljer, så bruker `chrono` systemets klokke for å finne den nåværende datoen og tiden.

## Se Også

Følgende ressurser kan hjelpe deg lærer mer om å jobbe med datoer og tid i Rust:

1. ["Chrono" dokumentasjon](https://docs.rs/chrono/)
2. ["Time" dokumentasjon](https://doc.rust-lang.org/time/time/index.html)
3. ["Working with Dates and Times in Rust"](https://www.andresriancho.com/2020/working-with-dates-and-times-in-rust/)
4. Stack Overflow: ["How to get the current date and time in Rust?"](https://stackoverflow.com/questions/27334692/how-do-i-get-the-current-date-and-time-in-rust)