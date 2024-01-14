---
title:    "Rust: Å få gjeldende dato"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende dato kan være nyttig for mange programmeringsscenarioer. Kanskje du vil registrere når en bruker opprettet en konto, eller når en transaksjon ble gjort. Uansett hva grunnen måtte være, er det enkelt å få den nåværende dato i Rust.

## Hvordan

For å få den nåværende dato i Rust, kan du bruke standardbiblioteket "time". Først må du importere biblioteket ved å legge til følgende linje øverst i koden din:

```
use time;
```

Deretter kan du bruke funksjonen "now()" for å få den nåværende lokale tiden. Du kan også spesifisere en spesifikk tidsone ved å bruke funksjonen "at()". Her er et eksempel på hvordan du kan få den nåværende tiden og datoen i Oslo:

```
let oslo_tz: time::Tm = time::now().at("Europe/Oslo");
println!("Nåværende tid og dato i Oslo: {}", oslo_tz.to_local());
```

Dette vil gi følgende utgang:

```
Nåværende tid og dato i Oslo: Mon May 03 2021 16:15:52 GMT+0200 (Central European Summer Time)
```

## Dypdykk

For de som ønsker å utforske dette emnet nærmere, er det verdt å merke seg at "time" biblioteket i Rust er basert på "Chrono" biblioteket. Dette betyr at du også kan bruke funksjoner fra Chrono for å håndtere dato og tid i Rust.

En annen ting å være oppmerksom på er forskjellen mellom lokale og universelle tider. Lokal tid tar hensyn til tidsforskjellen mellom ulike tidszoner, mens universell tid er basert på UTC (Coordinated Universal Time).

## Se også

Her er noen nyttige lenker for å lære mer om å få den nåværende datoen i Rust:

- [Rust time dokumentasjon](https://doc.rust-lang.org/std/time/index.html)
- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust tidsbehandling med Chrono](https://medium.com/@simonvreux/rust-date-and-time-tools-with-chrono-e98549488f30) (på engelsk)