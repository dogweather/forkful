---
title:                "Rust: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt program kan være en spennende og givende opplevelse. Det kan gi deg muligheten til å løse problemer på en ny og innovativ måte, og å lære deg et fantastisk programmeringsspråk som Rust. 

## Hvordan

Rust er et kraftig og effektivt språk som er populært blant utviklere for sin fokus på sikkerhet, ytelse og konkurranse. La oss ta en titt på hvordan du kan starte ditt neste prosjekt med Rust.

Først må du installere Rust på datamaskinen din. Dette kan gjøres enkelt ved å følge trinnene på Rusts offisielle nettside. Når du har installert Rust, er du klar til å begynne å kode.

La oss si at du ønsker å lage et program som teller antall ord i en tekstfil. I Rust kan du enkelt gjøre dette ved å benytte deg av standardbiblioteket og noen få linjer med kode. Her er et eksempel:

```Rust
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Kunne ikke lese filen");

    let word_count = contents.split_whitespace().count();

    println!("Det er {} ord i filen.", word_count);
}
```

I dette eksempelet benytter vi oss av `std::env` for å hente inn argumenter som er gitt til programmet, og `std::fs` for å lese filinnholdet. Deretter splitter vi teksten ved mellomrom og teller antall ord. Til slutt skriver vi ut antall ord til konsollen.

## Dykk dypere

Nå som du har etablert grunnlaget for ditt nye prosjekt, kan det være lurt å dykke dypere inn i Rusts dokumentasjon og ressurser for å lære enda mer. Her er noen andre ressurser som kan være nyttige for deg:

- [Rust dokumentasjon](https://doc.rust-lang.org/book/)
- [Offisiell Rust bibliotek dokumentasjon](https://doc.rust-lang.org/std/)
- [Rust Reddit-samfunnet](https://www.reddit.com/r/rust)
- [Rust Discord-samfunnet](https://discord.gg/rust-lang)

## Se også

- [Hvordan lære Rust: En nybegynnerveiledning](https://blog.rust-lang.org/2015/04/10/learning-rust.html)
- [Bygge en chatbot i Rust](https://www.freecodecamp.org/news/build-a-chat