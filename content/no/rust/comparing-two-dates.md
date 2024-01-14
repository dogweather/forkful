---
title:    "Rust: Sammenligning av to datoer"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med datoer i programmering, kan det være nødvendig å sammenligne to datoer for å sjekke om de er like eller ulike. Dette kan gjøres på mange forskjellige måter, men i denne bloggposten vil jeg fokusere på hvordan man kan gjøre det i Rust.

## Hvordan

For å sammenligne to datoer i Rust, må man først importere biblioteket for dato og tid ved å legge til følgende linje i koden:

```
use std::time::Duration;
```

Deretter må man definere to variabler med datatypen `Duration` og legge til verdier for å representere de to datoene man ønsker å sammenligne:

```
let date_1 = Duration::new(1616067600, 0); // 18. mars 2021
let date_2 = Duration::new(1616067600, 0); // 18. mars 2021
```

For å sammenligne disse to datoene, kan man bruke `Duration` sin innebygde metode `eq`, som returnerer `true` hvis datoene er like og `false` hvis de er ulike:

```
println!("Er datoene like? {}", date_1.eq(&date_2)); // Output: Er datoene like? true
```

Man kan også sammenligne datoer ved hjelp av operatøren `==`, som i dette tilfellet vil gi samme resultat:

```
println!("Er datoene like? {}", date_1 == date_2); // Output: Er datoene like? true
```

## Dypdykk

Når man sammenligner datoer i Rust, må man være oppmerksom på at de må være av samme datatypen `Duration` for å kunne sammenligne dem. Man bør også være nøye med å sørge for at man har korrekte verdier for å unngå feil i sammenligningen.

En annen viktig ting å merke seg er at `Duration` representerer antall sekunder og nanosekunder siden Unix-timen. Dette kan være annerledes enn andre datatyper man er vant til å jobbe med, og det kan være lurt å gjøre seg kjent med dette før man begynner å sammenligne datoer i Rust.

## Se også

- [Dokumentasjon for Rust sin `std::time::Duration`-modul](https://doc.rust-lang.org/std/time/struct.Duration.html)
- [Sammenligning av datoer i Java](https://www.baeldung.com/java-compare-dates)
- [Sammenligning av datoer i Python](https://www.geeksforgeeks.org/comparing-dates-python/)