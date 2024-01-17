---
title:                "Få gjeldende dato"
html_title:           "Rust: Få gjeldende dato"
simple_title:         "Få gjeldende dato"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Få gjeldende dato og klokkeslett er en vanlig oppgave for programmerere. Dette er viktig for å holde styr på tidsbaserte problemer og å generere rapporter basert på datoer og tider.

## Hvordan:
For å få gjeldende dato og klokkeslett i Rust, kan du bruke standardbiblioteket `std::time` og `chrono` biblioteket. Her er et eksempel på hvordan du kan gjøre det:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use chrono::{Local, Datelike};

let now = SystemTime::now();
let seconds = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
let date = Local.timestamp(seconds.as_secs() as i64, 0);
println!("{}", date.format("%d.%m.%Y %H:%M:%S"));
```
**Output:** 10.10.2021 12:00:00

Her bruker vi `SystemTime` for å få gjeldende systemtid og `UNIX_EPOCH` for å konvertere tiden til sekunder siden 1. januar 1970, som er standardtid for mange systemer. Deretter bruker vi `chrono` biblioteket for å formatere tiden etter ønske.

## Dykk dypere:
Å få gjeldende dato og klokkeslett er en viktig funksjon i de fleste programmeringsspråk. Det varierer imidlertid fra språk til språk hvordan dette gjøres. I Rust har vi flere alternativer for å få gjeldende tid, for eksempel å bruke `std::time` biblioteket som vist i eksemplet over, eller å bruke eksterne biblioteker som `chrono`.

En annen ting å være oppmerksom på er at å få aktuell tid ikke alltid er pålitelig. Det kan være forskjellige faktorer som påvirker nøyaktigheten, som systemets klokke og nettverkstilkoblingen til datamaskinen.

## Se også:
* [Rust standardbiblioteket dokumentasjon for time](https://doc.rust-lang.org/std/time/)
* [Chrono biblioteket for å håndtere datoer og tider i Rust](https://docs.rs/chrono/latest/chrono/)