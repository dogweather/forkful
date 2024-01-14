---
title:                "Rust: Å få dagens dato"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor
Den nåværende datoen er en viktig del av mange programmer, spesielt de som håndterer tidsfølsomme oppgaver som fakturering, kalenderapplikasjoner og planleggingsverktøy. I Rust, kan vi enkelt få den nåværende datoen ved å bruke standardbiblioteket.

# Hvordan
Vi kan få den nåværende datoen ved å bruke funksjonen `now()` fra `chrono`-biblioteket. Dette biblioteket er en standarddel av Rust og hjelper oss med å håndtere dato og tid. Først må vi importere biblioteket i vår kode ved å skrive `extern crate chrono;`. Deretter kan vi bruke `now()`-funksjonen for å få dagens dato, og lagre den i en variabel for videre bruk.

```
extern crate chrono;
use chrono::{Local, DateTime};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now); // 2021-02-05 04:52:27.153245 +01:00 local
}
```

Koden ovenfor vil skrive ut datoen og tiden i UTC-format ved å bruke `println!`-makroen. Men vi kan også formatere datoen og tiden som vi ønsker ved å bruke `format!`-makroen og angi ønsket format.

```
...
println!("{}", format!("{}", now.format("%A, %b %d, %Y"))); // Friday, Feb 05, 2021
...
```

Her har vi brukt formatet `%A, %b %d, %Y` for å få datoen på formen "day of the week, month day, year". Det er mange forskjellige formatmuligheter som kan brukes, og du kan finne en full liste over disse på Chronos dokumentasjonsside.

# Dypdykk
Den nåværende datoen i Rust er representert ved hjelp av structen `DateTime`. Denne structen inneholder informasjon om dato, tid og tidsforskyvning i forhold til UTC. Hvis du vil lære mer om tidsstyring i Rust, kan du lese Chronos dokumentasjon eller Utforske Rust-serien på bloggen vår.

# Se også
- Chronos offisielle dokumentasjon: https://docs.rs/chrono/
- Utforske Rust-serien: https://www.myrustjourney.com/category/exploring-rust/