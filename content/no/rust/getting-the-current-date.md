---
title:                "Rust: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se nærmere på hvordan man kan få den nåværende datoen i Rust programmeringsspråket. Dette kan være nyttig i tilfeller der man trenger å vise datoen i et program eller trenger å lagre datoen for senere bruk. 

## Hvordan gjøre det

Vi kan bruke standard Rust biblioteket chrono for å få den nåværende datoen. Først må vi legge til chrono biblioteket i vår Rust fil:
```
extern crate chrono;
use chrono::{DateTime, Utc};
```

Deretter kan vi bruke DateTime og Utc funksjonene til å få den nåværende datoen som en streng:
```
let current_date: DateTime<Utc> = Utc::now();
println!("{}", current_date.format("%Y-%m-%d").to_string());
```

Dette vil returnere datoen i formatet ÅÅÅÅ-MM-DD som kan endres ved å endre formatet i format funksjonen. For eksempel, hvis vi vil få datoen i formatet DD.MM.ÅÅÅÅ, kan vi bruke følgende kode:
```
println!("{}", current_date.format("%d.%m.%Y").to_string());
```

## Dykk dypere

Vi kan også bruke DateTime og Utc funksjonene til å få den nåværende datoen med klokkeslettet. I tillegg til å vise datoen, kan vi også bruke chrono biblioteket for å manipulere datoen. For eksempel kan vi legge til en dag til den nåværende datoen ved å bruke følgende kode:
```
let next_day = current_date + Duration::days(1);
println!("{}", next_day.format("%Y-%m-%d").to_string());
```

## Se også

- [Chrono dokumentasjon](https://docs.rs/chrono/0.4.11/chrono/)
- [Offisiell Rust nettside](https://www.rust-lang.org/)
- [Rust programmering på norsk](https://www.rust-lang-no.org/)