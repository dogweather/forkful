---
title:                "Rust: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor
Rust er et kraftig programmeringsspråk som har blitt stadig mer populært blant utviklere. En av grunnene til dette er det store utvalget av verktøy og biblioteker som kan hjelpe deg å løse ulike utfordringer. I denne bloggposten skal vi se nærmere på hvordan man kan benytte Rust til å beregne en dato i fremtiden eller fortiden.

# Hvordan
For å komme i gang med å beregne datoer i Rust, må vi først importere biblioteket "chrono" som lar oss jobbe med datoer og tider. Deretter kan vi benytte oss av funksjoner som f.eks. "Local::today()" og "Local::now()" for å få tak i informasjon om dagens dato og tid.

```Rust
use chrono::{ Local, Datelike, Timelike };

let today = Local::today(); // Dagens dato
let now = Local::now(); // Klokken nå

println!("Dagens dato er {} {}, {}", today.month(), today.day(), today.year());
println!("Klokken er nå {}:{}:{}", now.hour(), now.minute(), now.second());
```

For å beregne en dato i fremtiden eller fortiden kan vi bruke funksjonen "with_year()" for å spesifisere et år, og deretter "with_month()" og "with_day()" for å spesifisere en måned og dag.

```Rust
use chrono::NaiveDate; // Importerer NaiveDate for å kunne konvertere fra dato til dato
use chrono::Duration; // Importerer Duration for å kunne håndtere tidsintervall

let today = Local::today();
let future = today.with_year(2020).with_month(5).with_day(10); // Ny dato i fremtiden

let difference = future.signed_duration_since(today); // Henter tidsintervall mellom dagens dato og fremtidsdato
let days = difference.num_days(); // Konverterer til antall dager

println!("Det er {} dager igjen til {} {}, {}.", days, future.month(), future.day(), future.year());
```

# Dypdykk
Det som er spesielt nyttig med "chrono" biblioteket er at det støtter ulike typer datoer og tider, som f.eks. lokale og globale datoer, UTC og ISO datoer. Dette gjør det enkelt å skrive kode som kan håndtere ulike tidssoner og kulturer.

En annen nyttig funksjon i "chrono" biblioteket er muligheten til å formatere datoer og tider. Dette kan gjøres ved hjelp av funksjonen "format()" og angivelse av ønsket format. Dette kan være nyttig hvis man ønsker å vise datoer og tider på en spesifikk måte for brukeren.

```Rust
use chrono::format::{ StrftimeItems }; // Importerer StrftimeItems for å kunne formatere datoer og tider

let now = Local::now();
let formatted_date = now.format("%H:%M, %e %B %Y").to_string(); // Formatterer til ønsket format

println!("Klokken er nå {}.", formatted_date);
```

# Se Også
- [Rust Dokumentasjon](https://doc.rust-lang.org/)
- [Chrono Bibliotek](https://docs.rs/chrono/0.4.11/chrono/)
- [Offisiell Rust Forum](https://users.rust-lang.org/)