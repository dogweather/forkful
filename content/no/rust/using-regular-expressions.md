---
title:    "Rust: Å bruke regulære uttrykk"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regex, eller regulære uttrykk, er et kraftig verktøy for å finne og manipulere tekststrenger basert på spesifikke mønstre. Dette kan være nyttig for å søke gjennom store mengder data, eller for å utføre kompleks tekstbehandling. Rust tilbyr et innebygd Regex-bibliotek som gjør det mulig for utviklere å utføre dette på en effektiv og trygg måte.

## Slik gjør du det

For å bruke Regex i Rust, må du først importere biblioteket med `use regex::Regex;`. Deretter kan du opprette en ny Regex-variabel ved å bruke `let regex = Regex::new("mønster");`, der "mønster" er det spesifikke mønsteret du ønsker å matche. For eksempel kan du bruke `let regex = Regex::new(r"katt|hund");` for å matche både "katt" og "hund" i en tekststreng.

Når du har opprettet en Regex-variabel, kan du bruke den til å søke gjennom en tekststreng ved hjelp av `find`-metoden. Dette vil returnere en `Option` med en `Match`-struktur som inneholder informasjon om den første matchen som ble funnet. For å få tilgang til selve strengen, kan du bruke `unwrap`-metoden.

```Rust
let result = regex.find("Jeg eier en katt og en hund.");
if let Some(result) = result {
    println!("Jeg eier en {}", result.as_str()); // vil skrive ut "Jeg eier en katt"
}
```

Du kan også bruke `captures`-metoden for å få tilgang til deler av den matchede strengen, slik som gruppen du ønsker å hente ut. For å gjøre dette, må du først bruke parenteser rundt den delen av mønsteret du ønsker å matche og deretter bruke indeksering på `Match`-strukturen for å hente ut gruppen din.

```Rust
let regex = Regex::new(r"([0-9]{2})/([0-9]{2})/([0-9]{4})");
let result = regex.captures("Fødselsdato: 25/12/1990");
if let Some(result) = result {
    println!("Datoen er {}/{}/{}", result[1], result[2], result[3]); // vil skrive ut "Datoen er 25/12/1990"
}
```

For flere avanserte bruksområder, kan du også bruke `replace`-metoden for å erstatte deler av teksten med annen tekst basert på et mønster.

```Rust
let regex = Regex::new(r"mønster");
let result = regex.replace("Dette er en tekst med mønster.", "erstattet mønster");
println!("{}", result); // vil skrive ut "Dette er en tekst med erstattet mønster."
```

## Dykk dypere

Som nevnt tidligere, er det mange flere avanserte bruksområder for Regex i Rust som ikke er dekket her. Du kan lære mer ved å lese dokumentasjonen for Regex-biblioteket og eksperimentere med forskjellige mønstre og metoder.

## Se også

- [Dokumentasjon for Regex-biblioteket i Rust](https://docs.rs/regex)
- [Rust regex-syntaks Finn en god artikkel her](https://dev.to/emmanuelnwabueze/building-your-regex-skills-chapter-one-regex-syntax-and-patterns-in-rust-2a7g) 
- [Offisiell Rust-hjemmeside](https://www.rust-lang.org/)