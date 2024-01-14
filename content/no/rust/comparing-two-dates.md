---
title:    "Rust: Sammenligne to datoer"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Hvorfor
Kodingspråket Rust blir stadig mer populært blant programmerere på grunn av sin effektive og sikre natur. En viktig del av programmering er å kunne sammenligne ulike verdier, spesielt når det kommer til datoer. I denne bloggartikkelen skal vi se på hvordan man kan sammenligne to datoer i Rust.

## Hvordan utføre sammenligninger av datoer i Rust
For å sammenligne to datoer i Rust, må vi først importere datatypen `DateTime` fra biblioteket `chrono`. Dette gjør vi ved å legge til følgende linje i toppen av koden vår:
```Rust
use chrono::{DateTime, Utc};
```
Deretter kan vi lage to variabler av typen `DateTime`, som vil representere de to datoene vi ønsker å sammenligne. Dette kan gjøres på flere ulike måter, avhengig av hvilken format datoene våre er i. La oss se på et eksempel der vi har to datoer lagret som `String`-verdier:
```Rust
let date1 = "2021-06-01".parse::<DateTime<Utc>>().unwrap();
let date2 = "2021-07-15".parse::<DateTime<Utc>>().unwrap();
```
Her har vi brukt `.parse()`-funksjonen for å konvertere `String`-verdiene til `DateTime`-verdier. Vi bruker også `.unwrap()` for å håndtere eventuelle feil som kan oppstå i konverteringsprosessen. Merk at vi også må spesifisere tidsone som en del av `DateTime`-typen, derfor bruker vi `Utc` her.

Nå kan vi sammenligne disse to datoene ved å bruke Rusts sammenligningsoperatører. For eksempel kan vi sjekke om `date1` er tidligere enn `date2` ved å skrive:
```Rust
if date1 < date2 {
    println!("{} er tidligere enn {}", date1, date2);
}
```
Dette vil gi oss følgende output:
```
2021-06-01 00:00:00 UTC er tidligere enn 2021-07-15 00:00:00 UTC
```
Vi kan også bruke `>` for å sjekke om en dato kommer senere enn en annen, eller `==` for å se om de to datoene er identiske.

## Dypdykk
Å sammenligne datoer i Rust kan virke enkelt, men det er også viktig å være klar over noen begrensninger og utfordringer. For eksempel er ikke alle datoformater støttet av `DateTime`-typen, og man må være forsiktig med å håndtere ulike tidszoner.

En annen utfordring kan være å sammenligne datoer som inneholder både dato og tidspunkt. I disse tilfellene må man sørge for å håndtere eventuelle forskjeller i tidssoner og sikre at man sammenligner de riktige verdiene.

## Se også
- [Chrono library documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust language website](https://www.rust-lang.org/no/)