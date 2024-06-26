---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:20.307595-07:00
description: "Hoe te: Rust, een systeemtaal die zich richt op veiligheid en prestaties,\
  \ is niet voorzien van datum- en tijdfunctionaliteiten in zijn\u2026"
lastmod: '2024-04-05T21:53:50.626164-06:00'
model: gpt-4-0125-preview
summary: Rust, een systeemtaal die zich richt op veiligheid en prestaties, is niet
  voorzien van datum- en tijdfunctionaliteiten in zijn standaardbibliotheek.
title: Het huidige datum ophalen
weight: 29
---

## Hoe te:
```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now.format("%Y-%m-%d %H:%M:%S"));
}
```

Uitvoer:
```
2023-04-05 14:20:35
```

## Diepere Duik
Rust, een systeemtaal die zich richt op veiligheid en prestaties, is niet voorzien van datum- en tijdfunctionaliteiten in zijn standaardbibliotheek. In plaats daarvan bouwt de gemeenschap crates - Rust's term voor bibliotheken of pakketten. Een uitblinker is `chrono`.

`chrono` biedt uitgebreide datum- en tijdfuncties. Bovendien handelt het tijdzones af, wat niet triviaal is. De crate gebruikt de tijdszonegegevens van `IANA` (Internet Assigned Numbers Authority) om lokale datums en tijden correct weer te geven.

Er bestaan alternatieve crates zoals `time`, maar deze kunnen verschillende interfaces of functies hebben. Voor minder uitgebreide behoeften kan `time` sneller zijn en minder afhankelijkheden hebben.

Het verkrijgen van de lokale tijd houdt systeemaanroepen in die interageren met het OS. De nauwkeurigheid en precisie kunnen variëren en worden beïnvloed door het systeem en de configuratie ervan.

De implementatiedetails verdienen ook een knik naar de ontwerpfilosofie. Rust heeft een voorkeur voor explicietheid. Dus, wanneer je de huidige tijd pakt, kies je expliciet voor lokale tijd vs. UTC, bewustzijn van tijdzones, enzovoort - dit minimaliseert verrassingen en bevordert intentioneel coderen.

## Zie Ook:
- Documentatie van Rust's `chrono` crate: https://docs.rs/chrono/
- Documentatie van Rust's `time` crate: https://docs.rs/time/
- `IANA` tijdszonedatabase: https://www.iana.org/time-zones
