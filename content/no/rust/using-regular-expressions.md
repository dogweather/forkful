---
title:                "Rust: Å bruke regulære uttrykk"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva er bruken av regulære uttrykk i Rust?

Regulære uttrykk, også kjent som regex, er et viktig verktøy for å behandle og manipulere tekststrenger i programmering. De kan brukes til å finne og erstatte bestemte mønstre i en tekststreng, og kan derfor være svært nyttige for datamanipulasjon og tekstbehandling. I Rust, et moderne og effektivt programmeringsspråk, er regex en innebygd del av standardbiblioteket, noe som gjør det enkelt å bruke og integrere i dine programmer.

## Slik bruker du regex i Rust

For å bruke regex i Rust, må du først importere biblioteket ved å legge til `use regex::Regex;` øverst i filen din. Deretter kan du definere et regex-objekt ved hjelp av `Regex::new("mønster")`, der "mønster" er mønsteret du vil finne i teksten. For eksempel vil `Regex::new("rust")` finne alle forekomster av ordet "rust" i en tekststreng.

```Rust
use regex::Regex;

fn main() {
    let text = "Rust er et fantastisk programmeringsspråk!";
    let regex = Regex::new("rust").unwrap();
    println!("{:?}", regex.find(text));
}
```

Dette vil skrive ut `Some(Match { start: 0, end: 4 })` som indikerer at ordet "Rust" ble funnet i teksten, og hvilken indeks det starter og slutter på.

## Dypdykk i regex-funksjonalitet

Regex i Rust har et bredt spekter av funksjoner og operasjoner som kan hjelpe deg med å manipulere tekststrenger på en effektiv måte. Her er noen eksempler:

- `is_match()` - returnerer en boolsk verdi som indikerer om et mønster ble funnet i teksten
- `find()` - returnerer en struct med informasjon om den første forekomsten av mønsteret
- `split()` - deler teksten basert på et mønster og returnerer en iterator over delene
- `replace()` - erstatter deler av tekststrengen basert på et mønster
- og mange flere!

For å lære mer om regex i Rust, kan du utforske dokumentasjonen og prøve ut forskjellige eksempler i dine egne programmer.

## Se også

- [Offisiell Rust dokumentasjon for Regex](https://doc.rust-lang.org/regex/regex/index.html)
- [Rust Cookbook: Bruke Regex](https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html)
- [Regex tutorial på Rust Programming Language Forum](https://users.rust-lang.org/t/a-regex-tutorial-regular-expressions-in-rust/2162)