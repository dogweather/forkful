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

## Hvorfor
Har du noen gang lurt på hvordan du kan regne ut en dato i fremtiden eller fortiden? Kanskje du vil vite når du skal feire bursdagen din om fem år, eller når det var nøyaktig 1000 dager siden du møtte din kjære? Uansett årsak, lær hvordan du kan beregne datoer i Rust på en enkel måte.

## Hvordan
For å beregne en dato i fremtiden eller fortiden, kan vi bruke Rusts `chrono` bibliotek. Dette biblioteket gir oss funksjoner og typer for å håndtere datoer og tid. La oss se på et eksempel der vi ønsker å finne ut når det er nøyaktig 100 dager etter i dag:

```Rust
use chrono::{Utc, Duration};

let today = Utc::now().date();
let future_date = today + Duration::days(100);

println!("Om 100 dager vil det være {}.", future_date);
```

La oss bryte ned koden: Først importerer vi biblioteket `chrono`. Deretter bruker vi `Utc::now().date()` for å få dagens dato i UTC-tidssone. Vi lagrer denne datoen i en variabel kalt `today`. Deretter bruker vi `Duration::days(100)` for å lage en varighet på 100 dager og legger denne til `today`. Dette gir oss en ny dato som vi lagrer i variabelen `future_date`. Til slutt skriver vi ut denne datoen til konsollen. Dette vil gi oss en output som for eksempel "Om 100 dager vil det være 2021-07-16."

Hva om vi vil finne ut av datoen 1000 dager siden i fortiden? Da kan vi bruke samme kode, men endre `Duration::days(100)` til `Duration::days(-1000)`. Dette vil gi oss en output som for eksempel "1000 dager siden var det 2018-10-09."

## Deep Dive
Det er flere måter du kan manipulere datoer og tider på ved hjelp av `chrono` biblioteket. For eksempel kan du legge til eller trekke fra timer, minutter eller sekunder i tillegg til dager. Du kan også endre tidszonen ved å bruke `with_timezone()` funksjonen.

Det er viktig å merke seg at `chrono` biblioteket bruker UTC som standard tidsstempel, så hvis du vil bruke en annen tidszone må du endre dette manuelt. Det finnes også andre biblioteker for å håndtere datoer og tider i Rust, som for eksempel `time` og `datetime`. Utforsk og finn ut hvilket bibliotek som passer best for ditt prosjekt.

## Se også
- [Rust dokumentasjon for `chrono`](https://docs.rs/chrono/0.4.19/chrono/)
- [Flere eksempler på å beregne datoer i Rust](https://www.codementor.io/@arpitbhayani/manipulating-dates-and-times-in-rust-using-chrono-iinrxp8a9)